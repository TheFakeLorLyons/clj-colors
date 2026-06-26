(ns clj-colors.llm.associative
  "Bridge to the Anthropic Messages API for generating association
   entries from referent names. The workflow:
     (propose-association \"obsidian\") => returns parsed EDN map for review
     (propose-and-author! :mineral/obsidian \"obsidian\") => persist via authoring
   Authoring's schema validation runs on persist, so malformed LLM
   output fails loudly rather than corrupting authored.edn.

   Set ANTHROPIC_API_KEY in your environment. The model and the
   system prompt are exposed as dynamic vars so you can rebind for
   experimentation."
  (:require [clj-colors.authoring :as authoring]
            [clj-colors.associations :as associations]
            [clj-colors.color-tags :as color-tags]
            [clj-colors.llm.core :as llm]
            [clj-colors.llm.draft :as draft]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [org.httpkit.client :as http]
            [cheshire.core :as json]))

(defonce ^:private current-draft (atom nil))

(defn stage-entry!
  "Stash an arbitrary entry as the current draft, for refine-without-propose."
  [referent entry]
  (draft/stage! current-draft {:referent referent} entry nil))

(def ^:dynamic *system-prompt*
  "You are curating color-and-tag associations for a Clojure color
   library. Each association anchors a specific real-world referent (a
   material, plant, animal, scene, mineral, atmospheric phenomenon, etc.)
   to a small set of OKLAB-distinct colors with semantic tags.   

   Given a referent name, respond with a single Clojure EDN map. No
   preamble, no markdown fences, no commentary. Just the map.   

   The user-provided portion of the prompt appears at the end after the
   ## Referent header. It always includes a referent name. It may
   optionally include a separate 'Additional context:' section
   disambiguating the referent (e.g. 'Rick' might come with context
   'the character from Rick and Morty', or 'beth' with 'a name, not
   the Hebrew letter'). When additional context is provided, use it to
   resolve which referent is meant — but the primary referent name
   stays as the anchor for the entry. Do not let context drift the
   entry into a different domain than the referent suggests.   
   
   For associations meant to document a specific real-world artifact
   (a flag, a coat of arms, a character, a brand identity), prefer 
   comprehensive coverage of all colors present in the artifact over a curated
   representative subset. The :colors set should approach the actual
   documented palette of the thing. Use :weight on each tag (and
   implicitly on :colors via ordering) to reflect prominence; small
   detail colors get low weights but should still be present if you
   can identify them. Associations are meant to be precise; so do not
   add additional colors unless they are necessary.

   For palettes, which are more interpretive (a scene, a mood, a season),
   a curated ~4 - ~12 color palette is appropriate. As a general rule-of-thumb:
   The more specific an association is, the more [as appropriate] tags are
   expected, whereas broad associations will have a tendency to include
   fewer hues.
   
   ex:
             {\"#102030\" 0.30      ; primary
              \"#506070\" 0.25
              \"#a0b0c0\" 0.20
              \"#d0e0f0\" 0.15
              \"#f0f5ff\" 0.10}


   CRITICAL: Ensure output is one valid clojure map. If you need to
   refine a value (e.g. realize your :colors set has duplicates or
   should be smaller), decide on the final version before writing the
   EDN and *delete any in-progress/tmp maps that may already exist.* 
   Duplicate keys make the response unparseable.

   Required keys:   

     :colors    a set of 3-7 hex strings (#rrggbb) that genuinely
                represent the referent in the world. Aim for accurate
                color sampling, not stereotype. For a material, sample
                the actual surface; for a plant, the actual species at
                its most characteristic state; for a sky phenomenon, the
                actual atmospheric values.   

     :tags      a map from keyword to {:weight n :specificity s}.   

                :weight is 0.0 to 1.0 indicating how strongly the tag
                asserts. Primary identifiers (the referent name itself,
                obvious synonyms) at 1.0; secondary descriptors lower.   

                :specificity is optional, defaults to 1.0, and controls
                how exponentially strong the spatial match must be for
                this tag to fire on a candidate palette. Use these
                ranges:
                  0.4 - 0.7  general color qualities (pale, soft, dark)
                             that apply to any palette in the region.
                  0.8 - 1.2  standard referential tags (the name of the
                             referent, close synonyms).
                  1.3 - 1.8  tags that should only fire on decent
                             matches (era, season, mood, scene).
                  2.0 - 3.0  highly specific cultural, geographic, or
                             contextual tags (national association,
                             religious association, named historical
                             period). These should only fire when the
                             palette closely instantiates the full
                             association.   

     :sigma     a number indicating the spatial breadth of the
                association in OKLAB space:
                  0.03 - 0.05  very narrow (one specific material,
                               one mineral, one named hue).
                  0.06 - 0.09  medium (a specific flower, a recognized
                               scene type).
                  0.10 - 0.16  broad (a season's foliage range, a
                               time-of-day atmosphere).   

     :rationale a short paragraph explaining your color and tag
                choices. Important for future review and for an
                audit trail.   

   Optional but encouraged:   

     :category  a keyword tagging the broad domain
                (:mineral, :flora, :fauna, :sky, :weather, :food,
                :material, :scene, :cultural-artifact, etc.).   

     :references a vector of URLs or notes pointing to your source
                 evidence.   

   Example output for the referent 'obsidian':   

   {:colors    {\"#0a0a0c\" 0.35      ; the pure-black base
                \"#1a1015\" 0.25      ; iridescent purple undertone
                \"#100a0d\" 0.15
                \"#0d0712\" 0.15
                \"#241827\" 0.10}
    :tags      {:obsidian         {:weight 1.0}
                :volcanic-glass   {:weight 1.0}
                :archaeological   {:weight 0.7 :specificity 1.8}
                :iridescent-dark  {:weight 0.8 :specificity 0.9}
                :silicate         {:weight 0.6 :specificity 1.3}
                :black-rich       {:weight 0.9 :specificity 0.6}}
    :sigma     0.04
    :category  :mineral
    :rationale \"Obsidian is near-black volcanic glass with subtle
    iridescent purple-and-grey undertones from light interference at
    conchoidal fractures. Tight sigma because the color signature is
    narrow. :archaeological is highly specific (only fires for palettes
    that genuinely resemble obsidian, not all dark palettes).\"}")

(def ^:dynamic *refine-system-prompt*
  "You are refining an existing color-and-tag association based on
   human feedback. You'll be shown the current draft entry and a request
   for changes. Apply ONLY the requested changes, preserving every other
   field exactly as given. Do not editorialize, do not 'improve' fields
   the human didn't ask about.
   
   The schema is the same as for new associations:
   
     :colors     map of #rrggbb hex strings to their weight
     :tags       map from keyword to {:weight n :specificity s}
     :sigma      number (0.03 narrow, 0.08 medium, 0.15 broad)
     :rationale  paragraph explaining the choices
     :category   domain keyword (optional)
     :references vector (optional)
   
   Tag specificity ranges, review when adjusting:
     0.4 - 0.7  general color qualities (pale, soft, dark)
     0.8 - 1.2  standard referential tags
     1.3 - 1.8  era/season/mood/scene tags
     2.0 - 3.0  highly specific cultural or contextual tags
   
   When you make a tag adjustment, update the rationale's relevant
   sentences to reflect the change. When you make a color or sigma
   change, do likewise. Keep the rationale honest to what the entry
   actually claims.
   
   Respond with a single Clojure EDN map containing the entire updated
   entry. No preamble, no markdown fences, no commentary about what
   you changed (the human can diff it themselves). Just the map.")

(def ^:dynamic *brainstorm-system-prompt*
  "You brainstorm diverse, evocative referent names for color
   associations and palettes. Given a list of categories with counts,
   respond with a single EDN map under the :categories key.

   Each referent should be:
   - A concrete, recognizable thing or scene
   - Specific enough to have characteristic colors
   - Diverse from others in the same category (do not propose
     near-duplicates within a single category)
   - Lowercase, hyphenated (kebab-case)

   Example input:
   Generate diverse referent names for the following categories:
   - ocean: 5 referents
   - forest: 3 referents

   Example output:
   {:categories {:ocean [\"coral-reef\" \"kelp-forest\" \"abyssal-trench\" \"tropical-lagoon\" \"tidal-pool\"]
                 :forest [\"redwood-grove\" \"boreal-pine\" \"tropical-canopy\"]}}

   Respond with the EDN map only. No preamble, no markdown fences,
   no commentary.")

(defn brainstorm-referents
  "Ask the LLM to brainstorm diverse referent names. Takes a vector
   of {:category :count} maps and an optional extra-context string.
   Returns {:categories {category-kw [referent-strings]} :usage {...}}."
  ([category-counts] (brainstorm-referents category-counts nil))
  ([category-counts extra-context]
   (let [user-msg (str "Generate diverse referent names for the following categories:\n\n"
                       (clojure.string/join
                        "\n"
                        (map (fn [{:keys [category count]}]
                               (format "- %s: %d referents" category count))
                             category-counts))
                       (when (seq extra-context)
                         (str "\n\nAdditional context: " extra-context)))
         {:keys [entry usage]}
         (llm/one-shot *brainstorm-system-prompt* user-msg
                       {:category-counts category-counts})]
     {:categories (:categories entry)
      :usage usage})))

(defn- corpus-context
  "Snapshot of the current corpus that the LLM should reference for
   consistency: what tags are already in use (separated into
   authored/curated vs. other), which authored entries already exist
   (so we don't duplicate them), and which namespace conventions
   have been established for authored entries. Called fresh on every
   request so changes from previous proposals are visible."
  []
  (let [assocs   @associations/data
        ctags    @color-tags/data
        authored (filter (fn [[_ v]] (= :authored (:source v))) assocs)
        authored-tags
        (->> authored
             (mapcat (fn [[_ v]] (keys (:tags v))))
             distinct
             sort)
        all-corpus-tags
        (->> (concat (mapcat (comp keys :tags) (vals assocs))
                     (mapcat (comp keys :tags) (vals ctags)))
             distinct
             sort)
        other-tags     (remove (set authored-tags) all-corpus-tags)
        authored-keys  (->> authored (map first) sort)
        categories     (->> authored-keys (keep namespace) distinct sort)]
    {:authored-tags    authored-tags
     :other-tags       other-tags
     :authored-entries authored-keys
     :categories       categories}))

(defn- format-corpus-context
  [{:keys [authored-tags other-tags authored-entries categories]}]
  (str "## Corpus context (for consistency)\n\n"
       "Namespace conventions for authored entries. Match these "
       "rather than inventing new conventions; only introduce a new "
       "namespace if the referent genuinely belongs to a domain that "
       "isn't covered here:\n"
       (if (seq categories) (str/join ", " categories) "(none yet)")
       "\n\n"
       "Authored entries already in the corpus. Do not duplicate. If "
       "you find yourself proposing what would be a near-duplicate, "
       "note this in :rationale and let the human decide whether to "
       "proceed or refine the existing one:\n"
       (if (seq authored-entries)
         (str/join ", " authored-entries)
         "(none yet)")
       "\n\n"
       "Authored tag vocabulary (curated, high-trust). Strongly "
       "prefer reusing these over inventing semantic variants:\n"
       (if (seq authored-tags)
         (str/join " " authored-tags)
         "(none yet)")))

(defn show-context
  "Return the corpus context exactly as it would be sent to the LLM,
   for inspection. Print with (println (show-context)) to read it."
  []
  (format-corpus-context (corpus-context)))

(defn show-draft
  "Pretty-print the current draft to *out* with its referent name as
   a header. Returns nil. Use (draft) instead if you want the data
   as a value."
  []
  (if-let [d @current-draft]
    (do
      (println "## Draft for:" (:referent d))
      (println)
      (pp/pprint (:entry d))
      nil)
    (println "No draft staged.")))

(defn propose-association
  "Ask the LLM to propose an association for a referent. The optional
   extra-context disambiguates ambiguous referents and is presented to
   the LLM as a labeled section after the referent name. The on-disk
   diagnostic filename uses only the referent.

   Returns the parsed entry; the draft is also stashed for review."
  ([referent-name] (propose-association referent-name nil))
  ([referent-name extra-context]
   (let [context (format-corpus-context (corpus-context))
         referent-section (if (seq extra-context)
                            (str referent-name
                                 "\n\nAdditional context: " extra-context)
                            referent-name)
         user-msg (str context "\n\n## Referent to author\n\n" referent-section)
         {:keys [entry usage raw]}
         (llm/one-shot *system-prompt* user-msg
                       {:referent referent-name
                        :extra-context extra-context})]
     (spit (format "test-resources/%s.edn" referent-name) raw)
     (draft/stage! current-draft {:referent referent-name} entry usage)
     entry)))

(defn refine!
  "Send a natural-language refinement request to the LLM with the
   current draft as context. The model returns a modified entry,
   which replaces the current draft. Returns the modified entry.

   Use when the proposal is mostly right but needs adjustments: tag
   weight or specificity changes, additions or removals, sigma
   shifts, rationale tweaks. The model preserves what you don't
   mention, so iterative refinement composes cleanly. The draft's
   :usage is updated to reflect the refine call's API cost."
  [feedback]
  (if-let [d (draft/current current-draft)]
    (let [{:keys [referent entry]} d
          context  (format-corpus-context (corpus-context))
          user-msg (str context "\n\n"
                        "## Current draft for: " referent "\n\n"
                        (with-out-str (pp/pprint entry))
                        "\n## Refinement request\n\n"
                        feedback)
          {:keys [entry usage]}
          (llm/one-shot *refine-system-prompt* user-msg
                        {:referent referent :feedback feedback})]
      (draft/stage! current-draft (assoc d :feedback feedback) entry usage)
      entry)
    (throw (ex-info "No draft to refine" {}))))

(defn discard-draft! [] (draft/discard! current-draft))
(defn draft [] (draft/current current-draft))

(defn accept!
  [k]
  (if-let [d (draft/current current-draft)]
    (let [persisted (authoring/add-association! k (:entry d))]
      (draft/discard! current-draft)
      persisted)
    (throw (ex-info "No draft to accept" {}))))

(defn modify-tag [tag-key changes]
  (draft/modify! current-draft #(update-in % [:tags tag-key] merge changes)))

(defn add-tag [tag-key tag-data]
  (draft/modify! current-draft #(assoc-in % [:tags tag-key] tag-data)))

(defn remove-tag [tag-key]
  (draft/modify! current-draft #(update % :tags dissoc tag-key)))

(defn add-color [hex weight]
  (draft/modify! current-draft #(assoc-in % [:colors hex] weight)))

(defn remove-color [hex]
  (draft/modify! current-draft #(update % :colors dissoc hex)))

(defn set-sigma [sigma]
  (draft/modify! current-draft #(assoc % :sigma sigma)))

(defn load-as-draft! [k]
  (let [entry (get @associations/data k)]
    (when-not entry (throw (ex-info "No association with key" {:key k})))
    (draft/stage! current-draft {:referent (str k)} entry nil)
    entry))

(defn modify-association [k feedback]
  (load-as-draft! k)
  (refine! feedback))

(defn reroll! []
  (if-let [d (draft/current current-draft)]
    (let [referent (:referent d)]
      (draft/discard! current-draft)
      (propose-association referent))
    (throw (ex-info "No draft to reroll" {}))))

(defn propose-and-author! [k referent-name]
  (let [entry (propose-association referent-name)]
    (authoring/add-association! k entry)))