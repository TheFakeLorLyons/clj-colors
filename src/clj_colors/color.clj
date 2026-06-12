(ns clj-colors.color
  "Conversions between hex strings, RGBA vectors, and HSL, plus perceptual
   luminance. Channels are integers 0-255 and alpha defaults to 255."
  (:require [clojure.string :as str]))

(defn- expand-shorthand
  "Expand a 3-character hex body like \"abc\" into \"aabbcc\"."
  [body]
  (apply str (mapcat (fn [ch] [ch ch]) body)))

(defn hex->rgba
  "Parse a hex color into [r g b a]. Accepts #rgb, #rrggbb, or #rrggbbaa,
   with or without the leading #. Alpha defaults to 255 when absent."
  [hex]
  (let [body (str/replace hex #"^#" "")
        body (if (= 3 (count body)) (expand-shorthand body) body)
        channels (->> body
                      (partition 2)
                      (mapv (fn [pair] (Integer/parseInt (apply str pair) 16))))]
    (case (count channels)
      3 (conj channels 255)
      4 channels
      (throw (ex-info "Hex color must have 3, 6, or 8 digits"
                      {:hex hex :parsed channels})))))

(defn- clamp-255 ^long [n]
  (long (max 0 (min 255 (Math/round (double n))))))

(defn rgba->hex
  "Render [r g b] or [r g b a] as a hex string. When include-alpha? is true and
   alpha differs from 255, an 8-digit string is produced."
  ([rgba] (rgba->hex rgba false))
  ([[r g b a] include-alpha?]
   (let [pair (fn [n] (format "%02X" (clamp-255 n)))]
     (str "#" (pair r) (pair g) (pair b)
          (when (and include-alpha? a (not= 255 (clamp-255 a)))
            (pair a))))))

(defn rgba->hsl
  "Return [h s l] where h is degrees 0-360 and s, l are 0-1."
  [[r g b _]]
  (let [r' (/ r 255.0) g' (/ g 255.0) b' (/ b 255.0)
        mx (max r' g' b') mn (min r' g' b')
        d (- mx mn)
        l (/ (+ mx mn) 2.0)
        s (if (zero? d) 0.0 (/ d (- 1.0 (Math/abs (- (* 2.0 l) 1.0)))))
        h (cond
            (zero? d) 0.0
            (= mx r')  (* 60.0 (mod (/ (- g' b') d) 6.0))
            (= mx g')  (* 60.0 (+ (/ (- b' r') d) 2.0))
            :else      (* 60.0 (+ (/ (- r' g') d) 4.0)))]
    [(mod h 360.0) s l]))

(defn- srgb->linear [channel]
  (let [c (/ channel 255.0)]
    (if (<= c 0.03928)
      (/ c 12.92)
      (Math/pow (/ (+ c 0.055) 1.055) 2.4))))

(defn relative-luminance
  "Rec. 709 relative luminance in 0-1 for an [r g b a] color."
  [[r g b _]]
  (+ (* 0.2126 (srgb->linear r))
     (* 0.7152 (srgb->linear g))
     (* 0.0722 (srgb->linear b))))