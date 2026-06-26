(ns clj-colors.associations
  "Named color associations. Each entry is a :colors set + :meanings,
   optionally with provenance and context. Loaded from
   resources/associations_base.edn (the kaggle data) merged with any
   hand-authored entries in resources/associations.edn.

   An association's presence in a palette = (fraction of association
   colors matched within OKLAB threshold) * (sum of palette weights on
   the matched palette colors)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clj-colors.compatibility :as cc]
            [clj-colors.color :as color]
            [clj-colors.extensions :as extensions]
            [clj-colors.weights.base :as weights-base]
            [clj-colors.color-tags :as color-tags]))

(defn load-edn
  "Read an EDN resource by path. Returns nil if the resource is
   missing or the file fails to parse, with a warning to stderr.
   Lets the rest of the system carry on with whatever did load."
  [path]
  (when-let [r (io/resource path)]
    (try
      (-> r slurp edn/read-string)
      (catch RuntimeException e
        (binding [*out* *err*]
          (println "Warning: failed to load" path "-" (.getMessage e)))
        nil))))

(defn- normalize-colors-on-load
  "Ensure every entry's :colors is a {hex weight} map. Sets get
   equal weights summing to 1.0. Used during data load so downstream
   code can assume canonical shape regardless of source file age."
  [entry]
  (update entry :colors
          (fn [c]
            (cond
              (map? c) c
              (or (set? c) (sequential? c))
              (let [n (count c)
                    w (if (zero? n) 0.0 (/ 1.0 n))]
                (into {} (map (fn [h] [h w])) c))
              :else c))))

(defn- build-data-delay
  []
  (delay
    (let [base     (or (load-edn "associations.edn") {})
          extended (extensions/load-association-extensions)
          merged   (merge base extended)
          denied   (color-tags/apply-denylist merged)]
      (into {} (map (fn [[k v]] [k (normalize-colors-on-load v)])) denied))))

(defonce data (build-data-delay))

(defn refresh!
  "Drop the cached associations data and rebind to a fresh delay. The
   next dereference of @data will re-read every source file from
   disk, so authored.edn changes (and any other on-disk edits) become
   visible. Call this after authoring CRUD operations or any time you
   want to be sure the runtime sees the current disk state."
  []
  (alter-var-root #'data (constantly (build-data-delay))))

(defn lookup [k] (get @data k))

(defn- hex->oklab
  "OKLAB without alpha for distance computation."
  [hex]
  (let [[L a b _] (color/rgba->oklab (color/hex->rgba hex))]
    [L a b]))

(defn- color-distance
  "Perceptual distance between two hex codes in OKLAB."
  [hex-a hex-b]
  (let [[la aa ba] (hex->oklab hex-a)
        [lb ab bb] (hex->oklab hex-b)
        dl (- la lb), da (- aa ab), db (- ba bb)]
    (Math/sqrt (+ (* dl dl) (* da da) (* db db)))))

(defn- nearest-palette-color
  "The palette hex closest to target by OKLAB distance, when within
   threshold. Returns the palette hex or nil."
  [target palette-dist threshold]
  (let [[best-hex best-dist]
        (->> (keys palette-dist)
             (map (fn [p] [p (color-distance target p)]))
             (sort-by second)
             first)]
    (when (and best-hex (<= best-dist threshold))
      best-hex)))

(defn presence
  ([assoc-colors palette-dist] (presence assoc-colors palette-dist 0.08))
  ([assoc-colors palette-dist threshold]
   (let [;; assoc-colors is now {hex weight}, total-weight is the denominator
         total-weight (reduce + 0.0 (vals assoc-colors))]
     (if (zero? total-weight)
       0.0
       (let [;; For each assoc color, find best palette match within threshold
             matched-weight
             (reduce + 0.0
                     (keep (fn [[hex w]]
                             (when (nearest-palette-color hex palette-dist threshold)
                               w))
                           assoc-colors))
             weight-sum (reduce + 0.0
                                (map #(get palette-dist %)
                                     (set (keep (fn [[hex _]]
                                                  (nearest-palette-color hex palette-dist threshold))
                                                assoc-colors))))
             coverage (/ matched-weight total-weight)]
         (* coverage weight-sum))))))

(defn match
  "Match a palette's distribution against every association. Returns
   entries above the threshold, sorted descending by presence."
  ([palette-dist] (match palette-dist {}))
  ([palette-dist {:keys [threshold meaning-types]
                  :or   {threshold 0.15}}]
   (->> @data
        (keep (fn [[k entry]]
                (when (or (nil? meaning-types)
                          (contains? meaning-types (:meaning-type entry)))
                  (let [score (presence (:colors entry) palette-dist)]
                    (when (>= score threshold)
                      {:association  k
                       :presence     score
                       :meanings     (:meanings entry)
                       :meaning-type (:meaning-type entry)})))))
        (sort-by (comp - :presence))
        vec)))

(defn- named-association?
  [entry]
  (= :authored (:source entry)))

(defn- log-compress
  "Map raw aggregated score to a log-compressed display score.
   A score of 0 maps to 0; a score of 99 maps to 2.0; a score of
   9999 maps to 4.0. Preserves ordering, compresses dynamic range."
  [score]
  (Math/log10 (+ 1.0 (double score))))

(defn presence-kde
  "Coverage-gated KDE presence. Two interlocking factors:
   (1) Weighted fit: for each palette color, take its single best
       Gaussian against any association color, weighted by palette
       weight. Captures 'how illuminated is the palette by the
       brightest nearby lightpost?'
   (2) Coverage: mean best-match Gaussian per association color,
       seen from the palette side. Captures 'is the association
       genuinely present across its colors, or is one color
       accidentally lit by a broad field?'
   The product punishes broad-sigma associations that catch a palette
   on partial overlap and rewards palettes that instantiate the
   association across its full color set. Bounded in [0, 1]."
  [assoc-colors palette-dist sigma]
  (let [;; assoc-colors is now {hex weight}
        assoc-pairs   (mapv (fn [[hex w]] [(hex->oklab hex) (double w)]) assoc-colors)
        palette-pairs (mapv (fn [[hex w]] [(hex->oklab hex) (double w)]) palette-dist)
        total-assoc-weight (reduce + 0.0 (map second assoc-pairs))
        denom         (* 2.0 sigma sigma)
        K             (fn [a b]
                        (let [d (color-tags/oklab-distance a b)]
                          (Math/exp (- (/ (* d d) denom)))))
        weighted-fit
        (reduce
         (fn [total [po pw]]
           (+ total (* pw (apply max 0.0
                                 (map (fn [[ao _]] (K po ao))
                                      assoc-pairs)))))
         0.0
         palette-pairs)
        coverage
        (/ (reduce + 0.0
                   (map (fn [[ao aw]]
                          (* aw (apply max 0.0
                                       (map (fn [[po _]] (K po ao))
                                            palette-pairs))))
                        assoc-pairs))
           total-assoc-weight)]
    (* coverage weighted-fit)))

(defn palette-attributes
  ([hexes weights palette-dist palette-oklabs]
   (palette-attributes hexes weights palette-dist palette-oklabs {}))
  ([hexes weights palette-dist palette-oklabs
    {:keys [tag-threshold assoc-threshold presence-floor digits
            palette-category]
     :or   {tag-threshold   0.03
            assoc-threshold 0.15
            presence-floor  0.05
            digits          2}}]
   (let [color-bw     (weights-base/weights)
         palette-bw   (or (weights-base/palette-weights) {})
         color-band   (fn [tag] (get color-bw tag 1.0))
         palette-band (fn [tag] (get palette-bw tag 1.0))
         compat       (fn [tag]
                        (color-tags/tag-compatibility tag palette-oklabs weights))

         tags-from-colors
         (reduce
          (fn [acc [hex w]]
            (if-let [tag-map (color-tags/tags hex)]
              (reduce-kv
               (fn [a tag {tag-w :weight}]
                 (update a tag (fnil + 0.0)
                         (* (double w) (double tag-w)
                            (color-band tag) (compat tag))))
               acc tag-map)
              acc))
          {}
          (map vector hexes weights))

         [final-tags named-assocs]
         (reduce
          (fn [[t-acc a-acc] [k entry]]
            (let [raw-score (if-let [sigma (:sigma entry)]
                              (presence-kde (:colors entry) palette-dist sigma)
                              (presence (:colors entry) palette-dist))
                  cc-mult   (cc/compatibility-score (:category entry)
                                                    palette-category)
                  score     (* raw-score cc-mult)]
              (if (>= score presence-floor)
                (let [t-acc' (reduce-kv
                              (fn [a tag {tag-w :weight
                                          spec  :specificity
                                          :or   {spec 1.0}}]
                                (let [shaped (Math/pow score (* spec 1.2))]
                                  (update a tag (fnil + 0.0)
                                          (* shaped (double tag-w)
                                             (palette-band tag) (compat tag)))))
                              t-acc
                              (:tags entry))
                      a-acc' (if (named-association? entry)
                               (assoc a-acc k score)
                               a-acc)]
                  [t-acc' a-acc'])
                [t-acc a-acc])))
          [tags-from-colors {}]
          @data)

         compressed-tags (into {}
                               (for [[t v] final-tags
                                     :when (>= v tag-threshold)]
                                 [t (color/round-to (log-compress v) digits)]))]
     {:tags         compressed-tags
      :associations (into {} (filter (fn [[_ v]] (>= v assoc-threshold)) named-assocs))})))