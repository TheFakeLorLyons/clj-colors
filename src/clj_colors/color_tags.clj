(ns clj-colors.color-tags
  "Per-color adjective tag data, loaded from resources/color_tags_base.edn,
   keyed by lowercase hex. Each entry's :tags is a map from tag keyword
   to {:weight ...} (weights sum to 1.0); :color-families is the
   unfiltered keyword set from Color-Pedia's Keywords column."
  (:require [clj-colors.color :as color]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def denylist
  "Tags removed from the system. Applied at data-load time so they
   never enter signatures, band weights, or aggregation. To remove
   them permanently from disk, call strip-tags-from-file! on each
   data file. The on-disk strip is what survives reimport; this
   in-memory list is a safety net that catches stragglers if a file
   slips through, and a quick way to test removing a candidate
   without writing to disk."
  #{:professional :modern :elegant :classic :sophisticated
    :stylish :timeless :versatile :standard})

(defn apply-denylist
  "Remove denylisted tags from every entry's :tags submap. Shape-
   agnostic: works for color-tags ({hex {:tags ...}}) and for
   associations ({key {:tags ...}}) since both nest tags the same way."
  [entries]
  (reduce-kv
   (fn [acc k entry]
     (assoc acc k
            (cond-> entry
              (:tags entry)
              (update :tags (fn [tm] (apply dissoc tm denylist))))))
   {}
   entries))

(defn strip-tags-from-file!
  "Permanently remove the given tags from every :tags map in an EDN
   data file. Writes back in place using a streaming writer (the
   pp/pprint mistake from before is not repeated). Uses the regular
   Clojure reader rather than clojure.edn because some data files
   contain namespaced keywords with digit-leading names like
   :kaggle-emotional/0445, which strict EDN rejects but Clojure's
   reader accepts. *read-eval* is bound off so tagged literals can't
   execute code. Returns a summary map."
  [path tags-to-remove]
  (let [src (io/file path)
        data (binding [*read-eval* false]
               (read-string (slurp src)))
        denied (set tags-to-remove)
        stripped (reduce-kv
                  (fn [acc k entry]
                    (assoc acc k
                           (cond-> entry
                             (:tags entry)
                             (update :tags
                                     (fn [tm] (apply dissoc tm denied))))))
                  {}
                  data)
        total-removed (- (reduce + (map (comp count :tags val) data))
                         (reduce + (map (comp count :tags val) stripped)))]
    (with-open [w (io/writer src)]
      (binding [*out* w]
        (print "{")
        (doseq [[k v] stripped]
          (pr k)
          (print " ")
          (pr v)
          (newline))
        (print "}")))
    {:path            path
     :entries         (count stripped)
     :tags-banned     (count denied)
     :occurrences-cut total-removed}))

(defonce data
  (delay
    (let [smoothed (io/resource "color_tags.edn")
          raw      (-> smoothed slurp edn/read-string)]
      (apply-denylist raw))))

(defonce ^:private cache (atom nil))
(defn invalidate! [] (reset! cache nil))

(defn lookup
  [hex]
  (when hex
    (let [h (str/lower-case hex)
          k (if (str/starts-with? h "#") h (str "#" h))]
      (get @data k))))

(defn tags
  "Tag map for a hex code (keyword -> {:weight ...}), or nil."
  [hex]
  (:tags (lookup hex)))

(defn tag-names
  "Tag keywords for a hex code, as a set."
  [hex]
  (some-> (tags hex) keys set))

(defn tag-weight
  "Weight of a specific tag on a specific color, 0.0 when absent."
  [hex tag]
  (get-in (tags hex) [tag :weight] 0.0))

(defn color-families
  "Color-families set for a hex code, or nil."
  [hex]
  (:color-families (lookup hex)))

(defn colors-with-family
  "Every color whose :color-families set contains the given keyword."
  [kw]
  (->> @data
       (filter (fn [[_ entry]] (contains? (:color-families entry) kw)))
       (into {})))

(defn tag-frequencies
  "How many colors carry each tag (counted by presence, not weight)."
  []
  (->> @data vals (mapcat (comp keys :tags)) frequencies))

(defn tag-idf
  "Inverse document frequency per tag, log(N / colors-with-tag).
   Rare tags get higher IDF."
  []
  (let [freqs (tag-frequencies)
        n     (count @data)]
    (into {} (for [[t f] freqs]
               [t (Math/log (double (/ n f)))]))))

(def tag-idf-memo (memoize tag-idf))

(defn- temperature-tags [t]
  (cond
    (>= t 0.65) [[:warm-dominant    {:weight (min 1.0 (* 2 (- t 0.5)))}]]
    (<= t 0.35) [[:cool-dominant    {:weight (min 1.0 (* 2 (- 0.5 t)))}]]
    :else       [[:neutral-balanced {:weight 0.6}]]))

(defn- brightness-tags [b]
  (cond
    (>= b 0.75) [[:high-key {:weight (min 1.0 (* 2 (- b 0.5)))}]]
    (<= b 0.25) [[:low-key  {:weight (min 1.0 (* 2 (- 0.5 b)))}]]
    :else       [[:mid-key  {:weight 0.5}]]))

(defn- saturation-tags [s]
  (cond
    (>= s 0.65) [[:saturated   {:weight (min 1.0 s)}]]
    (<= s 0.30) [[:muted       {:weight (min 1.0 (- 1.0 s))}]]
    :else       [[:moderate-saturation {:weight 0.5}]]))

(defn- contrast-tags [c]
  (cond
    (>= c 0.60) [[:high-contrast {:weight (min 1.0 c)}]]
    (<= c 0.25) [[:soft-contrast {:weight (min 1.0 (- 1.0 c))}]]
    :else       []))

(defn- hue-concentration-tags [h]
  (cond
    (>= h 0.85) [[:monochromatic {:weight (min 1.0 h)}]]
    (>= h 0.55) [[:analogous     {:weight (min 1.0 h)}]]
    (<= h 0.25) [[:multi-hue     {:weight (min 1.0 (- 1.0 h))}]]
    :else       []))

(defn- combined-tags
  "Tags emerging from interactions between properties."
  [{:keys [brightness saturation temperature]
    :or {brightness 0.5 saturation 0.5 temperature 0.5}}]
  (cond-> []
    (and (>= brightness 0.70) (<= saturation 0.45))
    (conj [:pastel {:weight (min 1.0 (* brightness (- 1.0 saturation)))}])

    (and (>= saturation 0.65) (>= brightness 0.30) (<= brightness 0.70))
    (conj [:jewel-toned {:weight (min 1.0 saturation)}])

    (and (>= brightness 0.30) (<= brightness 0.65)
         (<= saturation 0.50) (>= temperature 0.55))
    (conj [:earthy-palette {:weight 0.7}])

    (and (>= brightness 0.70) (<= saturation 0.35) (<= temperature 0.45))
    (conj [:ethereal {:weight (min 1.0 (* brightness (- 1.0 saturation)))}])

    (and (>= saturation 0.70) (>= brightness 0.50))
    (conj [:vibrant {:weight (min 1.0 (* saturation brightness 1.5))}])

    (and (<= brightness 0.35) (<= saturation 0.40))
    (conj [:somber {:weight (min 1.0 (- 1.0 (* brightness 2)))}])))

(defn- family-tags [family]
  (cond
    (contains? #{:red :orange :brown}      family) [[:warm-family    {:weight 0.85}]]
    (contains? #{:yellow :gold}            family) [[:warm-family    {:weight 0.70}]
                                                    [:luminous       {:weight 0.70}]]
    (contains? #{:blue :cyan :teal}        family) [[:cool-family    {:weight 0.85}]]
    (contains? #{:green}                   family) [[:nature-family  {:weight 0.85}]]
    (contains? #{:purple :violet :magenta} family) [[:rich-family    {:weight 0.75}]]
    (contains? #{:pink}                    family) [[:warm-family    {:weight 0.55}]
                                                    [:tender         {:weight 0.70}]]
    (contains? #{:black}                   family) [[:dark-family    {:weight 0.95}]]
    (contains? #{:white}                   family) [[:light-family   {:weight 0.95}]]
    (contains? #{:gray :grey}              family) [[:neutral-family {:weight 0.85}]]
    :else []))

(defn color-derived-tags
  "Tag map derived from a palette's computed color properties. Output
   shape matches :attributes :tags throughout the system:
   {tag-keyword {:weight 0.x}}. Each weight reflects how strongly the
   underlying property is expressed."
  [{:keys [brightness temperature saturation contrast hue-concentration family]
    :as computed}]
  (into {}
        (concat
         (temperature-tags       (double (or temperature 0.5)))
         (brightness-tags        (double (or brightness 0.5)))
         (saturation-tags        (double (or saturation 0.5)))
         (contrast-tags          (double (or contrast 0.5)))
         (hue-concentration-tags (double (or hue-concentration 0.5)))
         (combined-tags          computed)
         (family-tags            family))))

(defn palette-oklabs
  "Per-color OKLAB triples (no alpha) for a palette's hex list."
  [hexes]
  (mapv (fn [hex]
          (let [[L a b _] (color/rgba->oklab (color/hex->rgba hex))]
            [L a b]))
        hexes))

(defn- color-tag-triples
  "[oklab, weight, tag] from color-tags data."
  []
  (mapcat
   (fn [[_ entry]]
     (when-let [oklab (:oklab entry)]
       (for [[tag {tw :weight}] (:tags entry)]
         [oklab tw tag])))
   @data))

(defn oklab-distance [[L1 a1 b1] [L2 a2 b2]]
  (let [dl (- L1 L2) da (- a1 a2) db (- b1 b2)]
    (Math/sqrt (+ (* dl dl) (* da da) (* db db)))))

(defn- weighted-centroid
  "Mean OKLAB point weighted by accompanying weights."
  [oklab-weights]
  (let [total (reduce + 0.0 (map second oklab-weights))]
    (if (zero? total)
      [0.5 0.0 0.0]
      (let [[sl sa sb] (reduce
                        (fn [[sl sa sb] [[L a b] w]]
                          [(+ sl (* L w)) (+ sa (* a w)) (+ sb (* b w))])
                        [0.0 0.0 0.0]
                        oklab-weights)]
        [(/ sl total) (/ sa total) (/ sb total)]))))

(defn- weighted-spread
  "Weighted mean distance from the centroid. Floor at 0.05 to avoid
   divide-by-zero collapse on tags with only one carrying color."
  [oklab-weights centroid]
  (let [total (reduce + 0.0 (map second oklab-weights))]
    (if (zero? total)
      0.1
      (max 0.05
           (/ (reduce + 0.0
                      (map (fn [[oklab w]]
                             (* w (oklab-distance oklab centroid)))
                           oklab-weights))
              total)))))

(defn signatures
  "Cached signatures map. Returns nil if compute-signatures! hasn't
   been called yet."
  []
  @cache)

(defn tag-compatibility
  "Multiplier in [0, 1] from the BEST-matching cluster. The palette
   only needs to be near one cluster of the tag's signature for a high
   score; if every cluster is far, the tag doesn't fit any region the
   tag is known to live in. Tags without signatures return 1.0."
  [tag palette-oklabs palette-weights]
  (if-let [sig (get @cache tag)]
    (let [total-w (reduce + 0.0 palette-weights)
          per-cluster
          (for [{:keys [centroid spread]} (:clusters sig)
                :let [avg-dist (/ (reduce + 0.0
                                          (map (fn [oklab w]
                                                 (* w (oklab-distance oklab centroid)))
                                               palette-oklabs palette-weights))
                                  (max 0.0001 total-w))
                      ratio (/ avg-dist (max 0.05 (min 0.12 spread)))]]
            (Math/exp (- (* 0.5 ratio ratio))))]
      (if (seq per-cluster) (apply max per-cluster) 1.0))
    1.0))

(defn- greedy-cluster
  "Cluster oklab-weights into up to k groups using greedy weighted
   farthest-point assignment. Returns a vector of cluster maps
   [{:centroid c :spread s :weight w} ...]. Cheap, deterministic, and
   handles k=1 or k=2 gracefully."
  [oklab-weights k]
  (if (<= (count oklab-weights) k)
    ;; one cluster per point
    (mapv (fn [[ok w]]
            {:centroid ok :spread 0.05 :weight w})
          oklab-weights)
    (let [;; pick seeds by greedy farthest-point: start with heaviest,
          ;; then iteratively pick the point with max min-distance to
          ;; existing seeds
          sorted (vec (sort-by (comp - second) oklab-weights))
          seed0  (first sorted)
          seeds  (loop [chosen [seed0]]
                   (if (>= (count chosen) k)
                     chosen
                     (let [next-seed
                           (apply max-key
                                  (fn [[pt _]]
                                    (apply min
                                           (map (fn [[s _]] (oklab-distance pt s))
                                                chosen)))
                                  sorted)]
                       (recur (conj chosen next-seed)))))
          ;; assign every point to nearest seed
          assigned (group-by
                    (fn [[pt _]]
                      (->> seeds
                           (map-indexed (fn [i [s _]] [i (oklab-distance pt s)]))
                           (apply min-key second)
                           first))
                    oklab-weights)]
      (vec (for [i (range (count seeds))
                 :let [members (get assigned i [])]
                 :when (seq members)]
             (let [centroid (weighted-centroid members)
                   spread   (weighted-spread members centroid)
                   total    (reduce + 0.0 (map second members))]
               {:centroid centroid :spread spread :weight total}))))))

(defn- association-tag-triples
  "[oklab, weight, tag] from association data: every (color, tag)
   pair in an association becomes a triple. Weights from the colors
   map flow through so a 0.85-weight color contributes more signal
   to the tag's centroid than a 0.05-weight accent."
  [associations-data]
  (mapcat
   (fn [[_ entry]]
     (let [colors-map (:colors entry)
           hex-weights (cond
                         (map? colors-map) colors-map
                         (set? colors-map) (zipmap colors-map
                                                   (repeat (/ 1.0 (count colors-map))))
                         :else {})]
       (for [[hex color-w] hex-weights
             [tag {tag-w :weight}] (:tags entry)
             :let [[L a b _] (color/rgba->oklab (color/hex->rgba hex))]]
         [[L a b] (* color-w tag-w) tag])))
   associations-data))

(defn compute-signatures!
  "Build multi-centroid signatures from [oklab, weight, tag] triples.
   Each tag gets up to k clusters; compatibility takes the max across
   them so a palette only needs to be near ONE cluster to qualify."
  ([triples] (compute-signatures! triples 3))
  ([triples k]
   (let [by-tag (group-by (fn [[_ _ tag]] tag) triples)
         sigs (into {}
                    (for [[tag entries] by-tag
                          :let [ow (mapv (fn [[ok w _]] [ok w]) entries)]
                          :when (seq ow)]
                      [tag {:clusters (greedy-cluster ow k)
                            :sample-size (count ow)}]))]
     (reset! cache sigs)
     sigs)))

(defn refresh-signatures!
  "Rebuild tag-color signatures from color-tags data + associations
   data. Both sources contribute (oklab, weight, tag) triples so that
   tags appearing in both corpora get unified signatures. The
   association data parameter avoids a require-cycle with the
   associations namespace."
  [associations-data]
  (invalidate!)
  (compute-signatures!
   (concat (color-tag-triples)
           (association-tag-triples associations-data))))