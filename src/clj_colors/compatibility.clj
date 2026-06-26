(ns clj-colors.compatibility
  "Derive category-to-category compatibility from the association
   corpus rather than hardcoded rules. Two categories are compatible
   to the degree their tag vocabularies overlap, weighted by tag
   frequency and specificity. The tensor is rebuilt from
   @associations/data on demand and cached; refresh! invalidates
   so authoring changes flow through.

   Used by palette-attributes to gate association contributions: a
   flag (category :cultural-artifact) should not deposit tags onto
   an autumn scene (category :autumn) just because they share a red.
   The gate is multiplicative on the raw presence score, so partial
   matches in semantically distant categories get crushed below the
   presence-floor and contribute nothing."
  (:require [clj-colors.color :as color]
            [clj-colors.color-tags :as color-tags]
            [clj-colors.meta :as meta]
            [clojure.set :as set]))

(defonce ^:private palettes-for-tensor (atom nil))
(defonce ^:private associations-for-tensor (atom nil))
(defonce ^:private tensor-cache (atom nil))

(defn- category-tag-profile
  "Accumulate tags across all entries in a category, weighted by
   tag-weight times specificity squared. Squaring specificity makes
   the profile lean toward category-defining tags rather than
   generic descriptors."
  [entries]
  (reduce
   (fn [acc entry]
     (reduce-kv
      (fn [a tag {:keys [weight specificity] :or {specificity 1.0}}]
        (update a tag (fnil + 0.0)
                (* (double weight) (double specificity) (double specificity))))
      acc
      (:tags entry)))
   {}
   entries))

(defn- assoc-color-tag-profile
  "Color-tag-derived profile from an association's colors. Mirrors
   palette-color-tag-profile but reads the {hex weight} colors map.
   Used to give associations a foothold in the color-tag vocabulary
   that palette profiles use, so cosine similarity can find genuine
   semantic overlap between authored categories and palette categories."
  [entry]
  (let [colors-map (or (:colors entry) {})]
    (reduce
     (fn [acc [hex w]]
       (if-let [tag-map (color-tags/tags hex)]
         (reduce-kv
          (fn [a tag {tag-w :weight}]
            (update a tag (fnil + 0.0)
                    (* (double w) (double tag-w))))
          acc
          tag-map)
         acc))
     {}
     colors-map)))

(defn- normalize-profile
  "L2-normalize so cosine similarity stays well-behaved and large
   categories don't dominate by mass alone."
  [profile]
  (let [norm (Math/sqrt (reduce + 0.0 (map #(* % %) (vals profile))))]
    (if (zero? norm)
      profile
      (reduce-kv (fn [m k v] (assoc m k (/ v norm))) {} profile))))

(defn- cosine-similarity
  "Cosine similarity between two normalized profiles. 0 = orthogonal
   (no shared tags), 1 = identical direction. Non-negative profiles
   stay in [0, 1]."
  [profile-a profile-b]
  (let [shared (set/intersection (set (keys profile-a))
                                 (set (keys profile-b)))]
    (reduce + 0.0
            (map (fn [t]
                   (* (get profile-a t 0.0) (get profile-b t 0.0)))
                 shared))))

(defn- palette-perceptual-profile
  "Derive perceptual tags from a palette's already-computed metadata.
   Every palette has :brightness, :temperature, :saturation, :contrast,
   :hue-concentration, and :family stored on it by enrich. Feeds those
   to the same color-derived-tags pipeline used elsewhere so palette
   profiles share vocabulary with association profiles even when the
   palette's exact hex codes aren't in the color-tags database."
  [palette]
  (let [computed (select-keys palette [:brightness :temperature :saturation
                                       :contrast :hue-concentration :family])
        derived  (color-tags/color-derived-tags computed)]
    (reduce-kv
     (fn [acc tag {tag-w :weight}]
       (assoc acc tag (double tag-w)))
     {}
     derived)))

(defn- palette-color-tag-profile
  "Tag profile for a palette. Combines per-color tag lookups from the
   color-tags database (when those hex codes are present) with
   metadata-derived perceptual tags (which work regardless). The
   combination ensures every palette contributes SOMETHING to its
   category profile in the tensor, even palettes whose specific hex
   codes are not in the color-tags corpus."
  [palette]
  (let [hexes   (:hex palette)
        n       (count hexes)
        weights (or (:weights palette)
                    (vec (repeat n (/ 1.0 n))))
        from-color-tags
        (reduce
         (fn [acc [hex w]]
           (if-let [tag-map (color-tags/tags hex)]
             (reduce-kv
              (fn [a tag {tag-w :weight}]
                (update a tag (fnil + 0.0)
                        (* (double w) (double tag-w))))
              acc
              tag-map)
             acc))
         {}
         (map vector hexes weights))
        from-perceptual (palette-perceptual-profile palette)]
    (merge-with + from-color-tags from-perceptual)))

(defn- coerce-palettes
  "palettes-for-tensor might be a registry map or a sequence of
   palettes. Return the sequence either way."
  [stored]
  (cond
    (nil? stored) []
    (map? stored) (vals stored)
    :else stored))

(defn- assoc-perceptual-profile
  "Compute perceptual category tags for an association by treating
   its colors as a synthetic palette, computing metadata, then
   deriving tags from that. Uses the same vocabulary as palette
   profiles (warm-family, low-key, saturated, nature-family, etc.)
   so cosine similarity between association and palette categories
   has shared keys to work with even when the association's exact
   hex codes are absent from the color-tags database."
  [entry]
  (let [colors-map (:colors entry)]
    (if (or (nil? colors-map) (empty? colors-map))
      {}
      (let [hex-vec  (vec (keys colors-map))
            w-vec    (vec (vals colors-map))
            rgbas    (mapv color/hex->rgba hex-vec)
            computed (meta/metadata rgbas w-vec)
            derived  (color-tags/color-derived-tags computed)]
        (reduce-kv
         (fn [acc tag {tag-w :weight}]
           (assoc acc tag (double tag-w)))
         {}
         derived)))))

(defn- build-tensor
  []
  (let [assoc-data     (or @associations-for-tensor {})
        assoc-by-cat   (-> (group-by :category (vals assoc-data))
                           (dissoc nil))
        assoc-profiles
        (reduce-kv
         (fn [m cat entries]
           (assoc m cat
                  (normalize-profile
                   (merge-with +
                               ;; Authored semantic tags (specificity-weighted)
                               (category-tag-profile entries)
                               ;; Perceptual tags derived from metadata —
                               ;; shares vocabulary with palette profiles
                               (reduce
                                (fn [acc e]
                                  (merge-with + acc (assoc-perceptual-profile e)))
                                {}
                                entries)))))
         {}
         assoc-by-cat)

        palettes        (coerce-palettes @palettes-for-tensor)
        palette-by-cat  (-> (group-by :category palettes)
                            (dissoc nil))
        palette-profiles
        (reduce-kv
         (fn [m cat palettes-in-cat]
           (assoc m cat
                  (normalize-profile
                   (reduce
                    (fn [acc p]
                      (merge-with + acc (palette-color-tag-profile p)))
                    {}
                    palettes-in-cat))))
         {}
         palette-by-cat)

        all-profiles (merge palette-profiles assoc-profiles)
        cats         (keys all-profiles)]
    (into {}
          (for [a cats]
            [a (into {}
                     (for [b cats]
                       [b (cosine-similarity (get all-profiles a)
                                             (get all-profiles b))]))]))))
(defn tensor
  "Current compatibility tensor, built on first access and cached.
   Call refresh! to rebuild after authoring."
  []
  (or @tensor-cache
      (let [t (build-tensor)]
        (reset! tensor-cache t)
        t)))

(defn refresh!
  "Invalidate the tensor cache. Accept palettes and (optionally)
   associations data so the next tensor build sees both. Associations
   are injected here rather than read directly to avoid a
   require-cycle with the associations namespace, since associations
   depends on compatibility for the compatibility-score gate inside
   palette-attributes."
  ([] (reset! tensor-cache nil))
  ([palettes]
   (reset! tensor-cache nil)
   (reset! palettes-for-tensor palettes))
  ([palettes associations-data]
   (reset! tensor-cache nil)
   (reset! palettes-for-tensor palettes)
   (reset! associations-for-tensor associations-data)))

(defn explain-compatibility
  [cat-a cat-b]
  (let [data     (or @associations-for-tensor {})
        by-cat   (group-by :category (vals data))
        prof-a   (normalize-profile (category-tag-profile (by-cat cat-a)))
        prof-b   (normalize-profile (category-tag-profile (by-cat cat-b)))
        shared   (set/intersection (set (keys prof-a)) (set (keys prof-b)))
        scored   (->> shared
                      (map (fn [t]
                             {:tag t
                              :profile-a (get prof-a t)
                              :profile-b (get prof-b t)
                              :product (* (get prof-a t) (get prof-b t))}))
                      (sort-by (comp - :product)))]
    {:score (cosine-similarity prof-a prof-b)
     :shared-tag-count (count shared)
     :top-shared (take 10 scored)}))

(def ^:dynamic *compatibility-threshold*
  "Hard gate threshold for compatible?. The multiplicative gate
   in palette-attributes uses the raw similarity, not this threshold.
   0.15 is a moderate starting point; tune based on gallery."
  0.15)

(defn compatible?
  "Hard boolean: are these categories above the threshold? Returns
   true on self-pairs and when either side is nil so the gate
   doesn't accidentally block uncategorized entries."
  [assoc-cat palette-cat]
  (cond
    (= assoc-cat palette-cat) true
    (or (nil? assoc-cat) (nil? palette-cat)) true
    :else (>= (get-in (tensor) [assoc-cat palette-cat] 0.0)
              *compatibility-threshold*)))

(defn compatibility-score
  "Raw cosine similarity in [0, 1]. Use as a multiplicative gate
   in scoring code: gated-score = raw-score * compatibility-score.
   Self-pairs and nil categories return 1.0 so they pass through
   unchanged."
  [assoc-cat palette-cat]
  (cond
    (= assoc-cat palette-cat) 1.0
    (or (nil? assoc-cat) (nil? palette-cat)) 1.0
    :else (let [raw (get-in (tensor) [assoc-cat palette-cat] 0.0)]
            (if (< raw 0.40) 0.0 raw))))