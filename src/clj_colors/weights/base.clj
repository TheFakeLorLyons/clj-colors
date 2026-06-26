(ns clj-colors.weights.base
  "Corpus rarity bands. A tag's base weight is a step function of its
   occurrence count: rare tags get a strong base weight, common tags
   get a weak one. Two corpora are tracked: color-level (how many
   colors carry the tag) for use in color->palette aggregation, and
   palette-level (how many palettes carry the tag) for use in
   association-side aggregation. Each is memoized independently."
  (:require [clj-colors.color-tags :as color-tags]))

(def default-bands
  "Moderate ratios so rare tags get a boost without dominating.
   Range is 4x (0.5 to 2.0)."
  [[2      2.0]    ; appears on only 1 palette
   [10     1.5]    ; rare
   [30     1.0]    ; common
   [60     0.5]    ; very common
   [90     0.2]    ; pervasive
   [##Inf  0.1]])

(def default-palette-bands
  "Palette-side schedule. Calibrated for a corpus of ~100 palettes.
   Wider dampening range (20x) since palette-level pervasiveness is
   much more aggressive than color-level."
  [[2      2.0]
   [10     1.5]
   [30     1.0]
   [60     0.5]
   [90     0.2]
   [##Inf  0.1]])

(defn band-weight
  "Look up the band weight for an occurrence count."
  ([occ] (band-weight default-bands occ))
  ([bands occ]
   (or (some (fn [[ub w]] (when (< occ ub) w)) bands)
       (second (last bands)))))

(defn weights-from-counts
  "Generic: tag -> count map to tag -> band-weight map."
  ([counts] (weights-from-counts counts default-bands))
  ([counts bands]
   (into {} (for [[tag occ] counts]
              [tag (band-weight bands occ)]))))

;; Color side --------------------------------------------------------------

(defn color-corpus-counts
  "Tag -> how many colors carry that tag, from color-tags data."
  []
  (->> @color-tags/data
       vals
       (mapcat (comp keys :tags))
       frequencies))

(defonce ^:private color-cache (atom nil))

(defn weights
  "Memoized color-tag band weights. Use for color-side contributions
   in palette aggregation."
  []
  (or @color-cache
      (reset! color-cache (weights-from-counts (color-corpus-counts)))))

(defn invalidate! [] (reset! color-cache nil))

;; Palette side ------------------------------------------------------------

(defonce ^:private palette-cache (atom nil))

(defn palette-corpus-counts
  "Tag -> how many palettes carry that tag, from their attribute maps.
   Caller passes the palette sequence so this namespace stays free of
   main/registry dependencies."
  [palettes]
  (->> palettes
       (mapcat (comp keys :tags :attributes))
       frequencies))

(defn palette-weights
  "Memoized palette-tag band weights. Zero-arg form returns the cache
   (nil if not initialized). Pass a palette sequence to (re)compute.
   Use for association-side contributions in palette aggregation."
  ([] @palette-cache)
  ([palettes]
   (reset! palette-cache
           (weights-from-counts (palette-corpus-counts palettes)))
   @palette-cache))

(defn invalidate-palette! [] (reset! palette-cache nil))

;; Diagnostics --------------------------------------------------------------

(defn band-histogram
  ([] (band-histogram default-bands))
  ([bands]
   (let [counts (color-corpus-counts)]
     (mapv (fn [[ub w]]
             [w (count (filter (fn [[_ c]] (< c ub)) counts))])
           bands))))

(defn palette-band-histogram
  ([palettes] (palette-band-histogram palettes default-palette-bands))
  ([palettes bands]
   (let [counts (palette-corpus-counts palettes)]
     (mapv (fn [[ub w]]
             [w (count (filter (fn [[_ c]] (< c ub)) counts))])
           bands))))

;; Aggregation --------------------------------------------------------------

(defn palette-tags
  "Aggregate Color-Pedia tags across a palette's colors. Each tag's
   score is sum(palette-weight * color-tag-weight * band-weight)
   across the constituent colors. Rare tags get a strong base weight
   from the color-side rarity bands; pervasive tags get a small one.
   Returns a map of tag => score, filtered to tags above the threshold."
  ([hexes weights] (palette-tags hexes weights 0.01))
  ([hexes weights threshold]
   (let [bw (clj-colors.weights.base/weights)]
     (->> (map vector hexes weights)
          (reduce
           (fn [acc [hex w]]
             (if-let [tag-map (color-tags/tags hex)]
               (reduce-kv
                (fn [a tag {tag-w :weight}]
                  (update a tag (fnil + 0.0)
                          (* (double w) (double tag-w)
                             (get bw tag 1.0))))
                acc tag-map)
               acc))
           {})
          (filter (fn [[_ score]] (>= score threshold)))
          (into {})))))