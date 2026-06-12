(ns clj-colors.meta
  "Derive descriptive metadata from a palette's colors: color family,
   brightness, temperature, saturation, contrast, and a set of tags. Input is a
   sequence of [r g b a] vectors. Thresholds were tuned against the bundled
   palette set and are meant to be adjusted as the library grows."
  (:require [clj-colors.color :as color]))

(defn- mean [xs]
  (let [coll (seq xs)]
    (if coll (/ (reduce + 0.0 coll) (count coll)) 0.0)))

(defn- clamp01 [x] (max 0.0 (min 1.0 x)))

(def ^:private family-buckets
  "Pairs of [exclusive-upper-hue-bound family-keyword], scanned in order."
  [[15 :red] [45 :orange] [70 :yellow] [160 :green]
   [200 :teal] [255 :blue] [290 :indigo] [330 :purple]
   [345 :pink] [360 :red]])

(defn- hue->family [h]
  (or (some (fn [[upper fam]] (when (< h upper) fam)) family-buckets) :red))

(defn metadata
  "Return a metadata map for a sequence of [r g b a] colors:
     {:family            color family keyword
      :brightness        mean perceptual luminance, 0-1
      :temperature       0 cool .. 1 warm (red minus blue dominance)
      :saturation        mean HSL saturation, 0-1
      :contrast          luminance range across the palette, 0-1
      :mean-lightness    mean HSL lightness, 0-1
      :hue-concentration resultant length of saturation-weighted hues, 0-1
      :tags              set of descriptive tag strings}"
  [rgbas]
  (let [hsls   (mapv color/rgba->hsl rgbas)
        lums   (mapv color/relative-luminance rgbas)
        sats   (mapv second hsls)
        lights (mapv (fn [[_ _ l]] l) hsls)
        brightness (mean lums)
        contrast   (if (seq lums) (- (apply max lums) (apply min lums)) 0.0)
        saturation (mean sats)
        mean-light (mean lights)
        warmth     (clamp01 (/ (+ 1.0 (mean (map (fn [[r _ b _]] (/ (- r b) 255.0))
                                                 rgbas)))
                               2.0))
        sx         (reduce + 0.0 (map (fn [[h s _]] (* s (Math/cos (Math/toRadians h)))) hsls))
        sy         (reduce + 0.0 (map (fn [[h s _]] (* s (Math/sin (Math/toRadians h)))) hsls))
        total-sat  (reduce + 0.0 sats)
        concentration (if (pos? total-sat) (/ (Math/hypot sx sy) total-sat) 0.0)
        mean-hue   (mod (Math/toDegrees (Math/atan2 sy sx)) 360.0)
        family     (if (< saturation 0.16) :neutral (hue->family mean-hue))
        max-sat    (if (seq sats) (apply max sats) 0.0)
        tags       (cond-> #{(name family)}
                     (< brightness 0.21) (conj "dark")
                     (> brightness 0.42) (conj "light")
                     (> contrast 0.9)    (conj "high-contrast")
                     (< contrast 0.68)   (conj "low-contrast")
                     (> saturation 0.6)  (conj "vivid")
                     (< saturation 0.28) (conj "muted")
                     (and (> mean-light 0.72) (< 0.12 saturation 0.7))
                     (conj "pastel")
                     (and (> max-sat 0.9) (> saturation 0.6)
                          (> contrast 0.9) (< mean-light 0.6))
                     (conj "neon")
                     (and (not= family :neutral) (> concentration 0.9)
                          (> saturation 0.12))
                     (conj "monochrome")
                     (< saturation 0.1) (conj "grayscale")
                     (and (#{:orange :yellow :green} family)
                          (<= 0.2 saturation 0.55) (< brightness 0.38))
                     (conj "earth-tone"))]
    {:family            family
     :brightness        brightness
     :temperature       warmth
     :saturation        saturation
     :contrast          contrast
     :mean-lightness    mean-light
     :hue-concentration concentration
     :tags              tags}))