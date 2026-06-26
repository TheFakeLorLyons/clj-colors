(ns clj-colors.llm.palettes
  "LLM bridge for proposing new palettes and modifying existing ones.
   Parallel to clj-colors.llm.associative. Shares the request and
   parse infrastructure from that namespace; you may want to refactor
   the shared pieces into a clj-colors.llm.core later, but reusing
   here is fine for now."
  (:require [clj-colors.llm.associative :as associative]
            [clj-colors.llm.core :as llm]
            [clj-colors.llm.draft :as draft]
            [clj-colors.main :as main]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]))

(defonce ^:private current-draft (atom nil))

(defn stage-entry!
  "Stash an arbitrary entry as the current draft."
  [referent entry]
  (draft/stage! current-draft {:mode :new :referent referent} entry nil))

(def ^:dynamic *system-prompt*
  "You are proposing a color palette for a referent name. A palette
is 4-8 OKLAB-distinct colors with optional prominence weights and
descriptive tags. The colors should be true to the referent: if it's
a real-world thing, sample the actual colors. Order from most
prominent to least.

The user-provided portion of the prompt appears after the
## Referent header. It always includes a referent name. It may
optionally include:
  - 'Additional context:' disambiguating the referent
  - 'Seed colors:' a list of hex codes the user wants reflected in
    the palette. When seed colors are given, treat them as anchors:
    include them when they fit, build around them, but feel free to
    add complementary colors if the palette would be richer for it.


Respond with a single Clojure EDN map:

  :hex     vector of 4-8 #rrggbb strings, the actual palette
  :weights vector of numbers summing to 1.0, prominence per color.
           Optional; omit for an even distribution.
  :tags    set of descriptive strings. Optional; the system computes
           color-derived tags itself, so tag here only when you mean
           something the system can't infer (era, theme, vibe).

No preamble, no markdown fences, no commentary. Just the EDN map.")

(def ^:dynamic *modify-system-prompt*
  "You are modifying an existing color palette based on human
feedback. You'll be shown the current palette and a request for
changes. Apply ONLY the requested changes, preserving every other
field exactly as given.

Schema is identical to a new proposal: :hex (4-8 colors), optional
:weights (sum to 1.0), optional :tags (set of strings).

If the human asks for a color addition, add to :hex and rebalance
:weights if they exist. If they ask for a removal, drop from :hex
and rebalance :weights. If they ask for a color shift, replace the
specific entries they describe. Preserve weights and tags unless
explicitly asked to change them.

Respond with the complete modified palette as a single EDN map. No
preamble, no markdown fences, no commentary about what changed.")


(defn show-draft
  "Pretty-print the current palette draft. Mode-aware (new vs
   modify) so the framing matches what's staged."
  []
  (if-let [d (draft/current current-draft)]
    (do
      (case (:mode d)
        :new
        (println "## New palette draft for:" (:referent d))

        :modify
        (do (println "## Modified palette draft for:" (:key d))
            (when-let [fb (:feedback d)]
              (println "## Refinement request:" fb))))
      (println)
      (pp/pprint (:entry d))
      nil)
    (println "No palette draft staged.")))

(defn draft
  "Current palette draft, or nil."
  []
  (draft/current current-draft))

(defn discard-draft!
  "Clear the current draft without persisting."
  []
  (draft/discard! current-draft))

(defn propose-palette
  "Ask Claude to propose a palette for the given referent name.
   Stashes the result as the current draft with :mode :new.
   Optional extra-context disambiguates the referent. Optional
   seed-hexes anchor the palette around colors the user wants."
  ([referent-name] (propose-palette referent-name nil nil))
  ([referent-name extra-context] (propose-palette referent-name extra-context nil))
  ([referent-name extra-context seed-hexes]
   (let [referent-section
         (str referent-name
              (when (seq extra-context)
                (str "\n\nAdditional context: " extra-context))
              (when (seq seed-hexes)
                (str "\n\nSeed colors: " (clojure.string/join ", " seed-hexes))))
         {:keys [entry usage]}
         (llm/one-shot *system-prompt* referent-section
                       {:referent referent-name
                        :extra-context extra-context
                        :seed-hexes seed-hexes})]
     (draft/stage! current-draft
                   {:mode :new :referent referent-name}
                   entry usage)
     entry)))
 
(defn load-as-draft!
  "Load an existing palette into the draft slot in :modify mode so
   modify-palette can iterate on it. Subsequent accept-palette!
   re-registers it under the same key."
  [k]
  (let [palette (main/get-palette k)]
    (when-not palette
      (throw (ex-info "No palette with key" {:key k})))
    (let [stored (-> palette
                     (select-keys [:hex :weights :tags])
                     (update :hex vec))]
      (draft/stage! current-draft
                    {:mode :modify :key k :original stored}
                    stored
                    nil)
      stored)))

(defn modify-palette
  "Ask Claude to modify an existing palette based on natural-language
   feedback. Stashes the modified palette as the current draft with
   :mode :modify. Does not touch the registry; review and then call
   accept-palette! to commit."
  [k feedback]
  (let [existing (main/get-palette k)
        _ (when-not existing
            (throw (ex-info "No palette with key" {:key k})))
        stored   (-> existing
                     (select-keys [:hex :weights :tags])
                     (update :hex vec))
        user-msg (str "## Current palette for: " k "\n\n"
                      (with-out-str (pp/pprint stored))
                      "\n## Modification request\n\n"
                      feedback)
        {:keys [entry usage]}
        (llm/one-shot *modify-system-prompt*
                      user-msg
                      {:key k :feedback feedback})]
    (draft/stage! current-draft
                  {:mode :modify :key k :original stored :feedback feedback}
                  entry
                  usage)
    entry))

(defn refine!
  "Re-prompt the current draft with additional natural-language
   feedback. Works for both :new and :modify mode drafts. Returns
   the modified entry."
  [feedback]
  (if-let [d (draft/current current-draft)]
    (let [{:keys [entry]} d
          referent-or-key (or (:referent d) (str (:key d)))
          user-msg (str "## Current palette draft for: " referent-or-key "\n\n"
                        (with-out-str (pp/pprint entry))
                        "\n## Refinement request\n\n"
                        feedback)
          {:keys [entry usage]}
          (llm/one-shot *modify-system-prompt*
                        user-msg
                        {:referent-or-key referent-or-key
                         :feedback feedback})]
      (draft/stage! current-draft
                    (assoc d :feedback feedback)
                    entry
                    usage)
      entry)
    (throw (ex-info "No draft to refine" {}))))

(defn reroll!
  "Discard the current draft and re-propose for the same referent.
   Only valid for :new mode drafts since :modify mode needs the
   existing key context."
  []
  (if-let [d (draft/current current-draft)]
    (case (:mode d)
      :new
      (let [referent (:referent d)]
        (draft/discard! current-draft)
        (propose-palette referent))

      :modify
      (throw (ex-info "Cannot reroll a modify draft; reload-as-draft! instead"
                      {:key (:key d)})))
    (throw (ex-info "No draft to reroll" {}))))

(defn accept-palette!
  "Persist the current draft. For :new mode, the explicit key
   argument is required. For :modify mode, the key from the draft
   is used and any explicit key is ignored. Returns the enriched
   palette as registered. Does not save to disk; call
   main/save-registry! when ready to persist."
  ([] (accept-palette! nil))
  ([k]
   (if-let [d (draft/current current-draft)]
     (let [{:keys [hex weights tags]} (:entry d)
           target-k (or (:key d) k)
           opts (cond-> {}
                  (seq weights) (assoc :weights weights)
                  (seq tags)    (assoc :tags tags))]
       (when-not target-k
         (throw (ex-info "New-palette draft requires a key argument" {})))
       (main/register-palette! target-k hex opts)
       (draft/discard! current-draft)
       (main/get-palette target-k))
     (throw (ex-info "No draft to accept" {})))))