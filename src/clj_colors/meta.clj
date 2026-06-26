(ns clj-colors.meta
  "Derive descriptive metadata from a palette's colors: color family,
   brightness, temperature, saturation, contrast, and a set of tags. Input is a
   sequence of [r g b a] vectors, optionally with a weight distribution giving
   each color's prominence.

   Hue, saturation, and lightness come from HSLuv rather than HSL. HSLuv is
   perceptually honest where HSL is geometric: its lightness is CIE L*
   (yellow reads light, navy reads dark), its saturation is the fraction of
   the maximum chroma the sRGB gamut offers at that lightness and hue (so a
   pale mint no longer outranks a leaf green), and its hue angles are spaced
   by appearance. Family buckets and tag thresholds below are tuned for
   HSLuv values and are not interchangeable with HSL ones."
  (:require [clj-colors.color :as color]))

(defn- clamp01 [x] (max 0.0 (min 1.0 x)))

(def ^:private family-buckets
  "Pairs of [exclusive-upper-hue-bound family-keyword] on the HSLuv hue
   wheel, scanned in order. Chromatic colors only."
  [[22 :red] [50 :orange] [105 :yellow] [155 :green]
   [230 :teal] [265 :blue] [280 :indigo] [325 :purple]
   [352 :pink] [360 :red]])

(defn- hue->family [h]
  (or (some (fn [[upper fam]] (when (< h upper) fam)) family-buckets) :red))

(defn- achromatic-family
  "Classify an achromatic color (low saturation) into :black, :white,
   or :gray by lightness."
  [lightness]
  (cond
    (< lightness 0.15) :black
    (> lightness 0.85) :white
    :else :gray))

(defn classify-family
  "Public family classifier used by the smoothing ingest. hue is in
   [0, 360); saturation and lightness are in [0, 1]. Chromatic colors
   are bucketed by hue; achromatic colors (saturation below 0.14) split
   into :black, :white, :gray by lightness."
  [hue saturation lightness]
  (if (< saturation 0.14)
    (achromatic-family lightness)
    (hue->family hue)))

(defn metadata
  "Return a metadata map for a sequence of [r g b a] colors:
     {:family            color family keyword
      :brightness        mean perceptual luminance, 0-1
      :temperature       0 cool .. 1 warm (red minus blue dominance)
      :saturation        mean HSL saturation, 0-1
      :contrast          luminance range across the palette, 0-1
      :mean-lightness    mean HSL lightness, 0-1
      :hue-concentration resultant length of saturation-weighted hues, 0-1
      :tags              set of descriptive tag strings}

   With weights (a distribution summing to 1, one entry per color), every
   mean becomes prominence-weighted, so a speck of a color contributes a
   speck of influence: family, brightness, temperature, saturation, and
   hue concentration all follow the distribution. Contrast deliberately
   stays the full luminance range: a fleck of white on black still spans
   it, because contrast is about extremes, not averages. Without weights,
   colors count evenly."
  ([rgbas] (metadata rgbas nil))
  ([rgbas weights]
   (let [n      (count rgbas)
         ws     (if (seq weights)
                  (mapv double weights)
                  (vec (repeat n (if (pos? n) (/ 1.0 n) 0.0))))
         wmean  (fn [xs] (reduce + 0.0 (map * xs ws)))
         hsls   (mapv color/rgba->hsluv rgbas)
         lums   (mapv color/relative-luminance rgbas)
         sats   (mapv second hsls)
         lights (mapv (fn [[_ _ l]] l) hsls)
         brightness (wmean lums)
         contrast   (if (seq lums) (- (apply max lums) (apply min lums)) 0.0)
         saturation (wmean sats)
         mean-light (wmean lights)
         warmth     (clamp01 (/ (+ 1.0 (wmean (map (fn [[r _ b _]] (/ (- r b) 255.0))
                                                   rgbas)))
                                2.0))
         sx         (reduce + 0.0 (map (fn [[h s _] w]
                                         (* w s (Math/cos (Math/toRadians h))))
                                       hsls ws))
         sy         (reduce + 0.0 (map (fn [[h s _] w]
                                         (* w s (Math/sin (Math/toRadians h))))
                                       hsls ws))
         total-sat  (wmean sats)
         concentration (if (pos? total-sat) (/ (Math/hypot sx sy) total-sat) 0.0)
         mean-hue   (mod (Math/toDegrees (Math/atan2 sy sx)) 360.0)
         family     (classify-family mean-hue saturation mean-light)
         max-sat    (if (seq sats) (apply max sats) 0.0)
         tags       (cond-> #{(name family)}
                      (< brightness 0.21) (conj "dark")
                      (> brightness 0.42) (conj "light")
                      (> contrast 0.9)    (conj "high-contrast")
                      (< contrast 0.68)   (conj "low-contrast")
                      (> saturation 0.74) (conj "vivid")
                      (< saturation 0.40) (conj "muted")
                      (and (> mean-light 0.74) (< 0.10 saturation 0.65))
                      (conj "pastel")
                      (and (> max-sat 0.98) (> saturation 0.72)
                           (> contrast 0.9) (< mean-light 0.62))
                      (conj "neon")
                      (and (not= family :neutral) (> concentration 0.9)
                           (> saturation 0.12))
                      (conj "monochrome")
                      (< saturation 0.08) (conj "grayscale")
                      (and (#{:orange :yellow :green} family)
                           (<= 0.3 saturation 0.7) (< brightness 0.38))
                      (conj "earth-tone"))]
     {:family            family
      :brightness        brightness
      :temperature       warmth
      :saturation        saturation
      :contrast          contrast
      :mean-lightness    mean-light
      :hue-concentration concentration
      :tags              tags})))