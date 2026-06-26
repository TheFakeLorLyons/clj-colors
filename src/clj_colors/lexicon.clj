(ns clj-colors.lexicon
  "Tag-to-pole semantic projection for the international color survey.
   Each tag in the system gets a 20-dimensional similarity vector
   representing how closely its meaning aligns with each of the 20
   emotion poles. The vectors are LLM-derived once per tag and
   cached; subsequent uses are pure lookup.

   The projection lets survey data (which only speaks the 20 poles)
   flow into the wider tag vocabulary without hand-curating synonym
   lists. A tag's weight on a hex from the survey is the dot product
   of its similarity vector with the hex's combined pole vector,
   where the combined pole vector is the weighted sum of family pole
   weights scaled by each family's Gaussian influence on the hex.

   The cache is an EDN file at resources/tag_pole_vectors.edn. Commit
   it to the repo: rebuilding from scratch costs ~$0.002 per tag at
   current Sonnet pricing, so a 2000-tag corpus is ~$4. Cached
   results are reusable across runs and across machines."
  (:require [clj-colors.llm.associative :as associative]
            [clj-colors.llm.core :as core]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]))

(def poles
  "The 20 emotion poles from the survey. Order is canonical for the
   similarity vectors; do not shuffle without invalidating the cache."
  [:admiration :amusement :anger :compassion :contempt :contentment
   :disappointment :disgust :fear :guilt :hate :interest :joy :love
   :pleasure :pride :regret :relief :sadness :shame])

(def pole-count (count poles))

(def cache-path
  "resources/tag_pole_vectors.edn")

(defn- load-cache
  "Read the on-disk cache. Returns {} if the file doesn't exist yet."
  []
  (let [f (io/file cache-path)]
    (if (.exists f)
      (edn/read-string (slurp f))
      {})))

(defonce ^{:doc "In-memory cache atom, loaded from disk on first
deref. Updated by embed-tag! and persisted after every update so the
on-disk file always reflects the latest state."}
  cache
  (atom (load-cache)))

(defn- save-cache!
  []
  (spit cache-path (with-out-str (pp/pprint @cache))))

(def ^:dynamic *embed-system-prompt*
  "You are rating the semantic similarity of a single tag word to
each of 20 emotion poles, on a 0.0 to 1.0 scale.

Score reflects how closely the tag's meaning aligns with each
emotion. Score 1.0 means the tag IS the emotion (or a tight
synonym). Score 0.0 means no semantic relation. Score 0.5 means
moderate relation: the tag often co-occurs with the emotion, or
carries some of its affect.

Calibration examples:
  'joyful'    to :joy         1.0  (tight synonym)
  'cheerful'  to :joy         0.85 (very close)
  'energetic' to :joy         0.55 (related but distinct)
  'obsidian'  to :joy         0.05 (essentially unrelated)
  'obsidian'  to :fear        0.4  (dark, can evoke unease)
  'obsidian'  to :pride       0.2  (some grandeur, weak link)
  'vintage'   to :contentment 0.5  (warmth and nostalgia)
  'vintage'   to :sadness     0.4  (wistfulness)
  'vintage'   to :amusement   0.2  (playful aesthetic, weak)

The 20 poles in canonical order:
  :admiration :amusement :anger :compassion :contempt :contentment
  :disappointment :disgust :fear :guilt :hate :interest :joy :love
  :pleasure :pride :regret :relief :sadness :shame

Respond with a single Clojure EDN vector of 20 numbers in the above
order. No preamble, no commentary, no markdown fences. Just the
vector.")

(defn embed-tag
  "Call the LLM to compute a 20-dimensional similarity vector for
   the given tag keyword. Returns the vector without mutating the
   cache; use embed-tag! for cached storage. Throws if the LLM
   returns malformed output."
  [tag-key]
  (let [tag-str (name tag-key)
        raw     (binding [associative/*system-prompt* *embed-system-prompt*]
                  (core/request-completion tag-str))
        cleaned (core/strip-fences raw)
        parsed  (try
                  (edn/read-string cleaned)
                  (catch Exception e
                    (throw (ex-info "Failed to parse embedding"
                                    {:tag   tag-key
                                     :raw   raw
                                     :error (.getMessage e)}))))]
    (when-not (and (vector? parsed)
                   (= pole-count (count parsed))
                   (every? number? parsed))
      (throw (ex-info "LLM returned invalid embedding shape"
                      {:tag             tag-key
                       :parsed          parsed
                       :expected-length pole-count})))
    (mapv double parsed)))

(defn embed-tag!
  "Embed a tag and persist to the cache. Idempotent: if the tag is
   already cached, returns the existing vector unless force-refresh?
   is true. Otherwise computes via LLM, updates the in-memory cache,
   writes to disk. Returns the vector."
  ([tag-key] (embed-tag! tag-key false))
  ([tag-key force-refresh?]
   (let [existing (get @cache tag-key)]
     (if (and existing (not force-refresh?))
       existing
       (let [vec (embed-tag tag-key)]
         (swap! cache assoc tag-key vec)
         (save-cache!)
         vec)))))

(defn similarity-to-poles
  "Look up a tag's similarity vector. Returns the vector or nil if
   the tag hasn't been embedded yet."
  [tag-key]
  (get @cache tag-key))

(defn embed-corpus!
  "Embed every tag in the supplied collection that isn't already in
   the cache. Sequential to respect rate limits; for very large
   corpora consider running overnight. Returns the count of newly
   embedded tags. Pass {:dry-run? true} to see what would be
   embedded without making LLM calls."
  ([tag-keys] (embed-corpus! tag-keys {}))
  ([tag-keys {:keys [dry-run? sleep-ms]
              :or   {dry-run? false sleep-ms 250}}]
   (let [to-embed (remove (set (keys @cache)) (distinct tag-keys))]
     (println "Would embed" (count to-embed) "tags;"
              (count (keys @cache)) "already cached.")
     (when-not dry-run?
       (doseq [t to-embed]
         (println "Embedding" t)
         (try
           (embed-tag! t)
           (Thread/sleep sleep-ms)
           (catch Exception e
             (println "Failed for" t ":" (.getMessage e))))))
     (count to-embed))))

(defn- extract-tag-keys
  "Pull tag keys from a container that may be a map (computed tags
   keyed by keyword with weight values) or a set (the palette
   schema's user-provided extras, which are strings or keywords).
   Returns a sequence of keywords; strings get coerced for
   downstream uniformity. Returns nil for anything else."
  [tags]
  (cond
    (map? tags) (keys tags)
    (set? tags) (map #(if (keyword? %) % (keyword %)) tags)
    :else       nil))

(defn collect-corpus-tags
  "Gather every distinct tag keyword across the base files. Returns
   a sorted vector. Use as input to embed-corpus! to populate the
   cache for everything currently in the corpus. Missing files are
   skipped silently; pass explicit paths to override defaults."
  ([]
   (collect-corpus-tags
    {:color-tags-path   "resources/color_tags.edn"
     :associations-path "resources/associations.edn"
     :palettes-path     "resources/palettes.edn"}))
  ([{:keys [color-tags-path associations-path palettes-path]}]
   (let [read-safe     (fn [path]
                         (try
                           (-> path slurp edn/read-string)
                           (catch Exception _ {})))
         color-tags    (read-safe color-tags-path)
         associations  (read-safe associations-path)
         palettes      (read-safe palettes-path)
         from-color    (mapcat (comp extract-tag-keys :tags)
                               (vals color-tags))
         from-assoc    (mapcat (comp extract-tag-keys :tags)
                               (vals associations))
         from-palettes (mapcat (fn [p]
                                 (concat (extract-tag-keys (:tags p))
                                         (extract-tag-keys
                                          (get-in p [:attributes :tags]))))
                               (vals palettes))]
     (->> (concat from-color from-assoc from-palettes)
          (remove nil?)
          distinct
          sort
          vec))))