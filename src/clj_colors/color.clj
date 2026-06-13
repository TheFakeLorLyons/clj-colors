(ns clj-colors.color
  "Conversions between hex strings, RGBA vectors, and HSL, plus perceptual
   luminance. Channels are integers 0-255 and alpha defaults to 255."
  (:require [clojure.string :as str])
  (:import (org.hsluv HsluvColorConverter)))

(defn- clamp-255 ^long [n]
  (long (max 0 (min 255 (Math/round (double n))))))

(defn- lerp ^double [^double x ^double y ^double t]
  (+ x (* (- y x) t)))

(defn- blend-oklab [lab1 lab2 t]
  (mapv (fn [x y] (lerp (double x) (double y) (double t))) lab1 lab2))

(defn oklab->oklch [[L A B alpha]]
  [L (Math/hypot A B) (mod (Math/toDegrees (Math/atan2 B A)) 360.0) alpha])

(defn oklch->oklab [[L C H alpha]]
  (let [h (Math/toRadians H)]
    [L (* C (Math/cos h)) (* C (Math/sin h)) alpha]))

(defn- srgb->linear ^double [^double v]
  (if (<= v 0.04045)
    (/ v 12.92)
    (Math/pow (/ (+ v 0.055) 1.055) 2.4)))

(defn- linear->srgb ^double [^double v]
  (let [v (max 0.0 (min 1.0 v))]
    (if (<= v 0.0031308)
      (* v 12.92)
      (- (* 1.055 (Math/pow v (/ 1.0 2.4))) 0.055))))

(defn- expand-shorthand
  "Expand a 3-character hex body like \"abc\" into \"aabbcc\"."
  [body]
  (apply str (mapcat (fn [ch] [ch ch]) body)))

(defn relative-luminance
  "Rec. 709 relative luminance in 0-1 for an [r g b a] color."
  [[r g b _]]
  (+ (* 0.2126 (srgb->linear (/ (double r) 255.0)))
     (* 0.7152 (srgb->linear (/ (double g) 255.0)))
     (* 0.0722 (srgb->linear (/ (double b) 255.0)))))

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

(defn rgba->hsluv
  "Convert [r g b (a)] (0-255) to HSLuv [h s l]: h in degrees 0-360 on the
   perceptually spaced HSLuv wheel, s and l scaled 0-1. Unlike HSL, both s
   and l are perceptual: l is CIE L* lightness (yellow reads light, navy
   reads dark) and s is the fraction of the maximum chroma the sRGB gamut
   offers at that lightness and hue."
  [[r g b _]]
  (let [conv (HsluvColorConverter.)]
    (set! (.-rgb_r conv) (/ (double r) 255.0))
    (set! (.-rgb_g conv) (/ (double g) 255.0))
    (set! (.-rgb_b conv) (/ (double b) 255.0))
    (.rgbToHsluv conv)
    [(.-hsluv_h conv) (/ (.-hsluv_s conv) 100.0) (/ (.-hsluv_l conv) 100.0)]))

(defn hsluv->rgba
  "Convert HSLuv [h s l] (s and l in 0-1) back to an [r g b a] vector,
   alpha 255. Useful for generating colors: pick a hue, hold l steady, and
   the results actually look equally light."
  [[h s l]]
  (let [conv (HsluvColorConverter.)]
    (set! (.-hsluv_h conv) (double h))
    (set! (.-hsluv_s conv) (* 100.0 (double s)))
    (set! (.-hsluv_l conv) (* 100.0 (double l)))
    (.hsluvToRgb conv)
    [(int (Math/round (* 255.0 (.-rgb_r conv))))
     (int (Math/round (* 255.0 (.-rgb_g conv))))
     (int (Math/round (* 255.0 (.-rgb_b conv))))
     255]))

(defn rgba->oklab
  "Convert [r g b (a)] (0-255) to Oklab [L a b alpha]: L is perceptual
   lightness 0-1, a and b are the green-red and blue-yellow axes (roughly
   -0.4..0.4), alpha 0-1. Straight lines in this space look like smooth,
   perceptually even transitions, which makes it the right working space
   for blending and gradient resampling."
  [[r g b a]]
  (let [rl (srgb->linear (/ (double r) 255.0))
        gl (srgb->linear (/ (double g) 255.0))
        bl (srgb->linear (/ (double b) 255.0))
        l  (Math/cbrt (+ (* 0.4122214708 rl) (* 0.5363325363 gl) (* 0.0514459929 bl)))
        m  (Math/cbrt (+ (* 0.2119034982 rl) (* 0.6806995451 gl) (* 0.1073969566 bl)))
        s  (Math/cbrt (+ (* 0.0883024619 rl) (* 0.2817188376 gl) (* 0.6299787005 bl)))]
    [(+ (* 0.2104542553 l) (* 0.7936177850 m) (* -0.0040720468 s))
     (+ (* 1.9779984951 l) (* -2.4285922050 m) (* 0.4505937099 s))
     (+ (* 0.0259040371 l) (* 0.7827717662 m) (* -0.8086757660 s))
     (/ (double (or a 255)) 255.0)]))

(defn oklab->rgba
  "Inverse of rgba->oklab. Out-of-gamut results are clamped channelwise
   into sRGB; for blends between in-gamut colors the excursions are tiny."
  [[L A B alpha]]
  (let [l3 (+ L (* 0.3963377774 A) (* 0.2158037573 B))
        m3 (+ L (* -0.1055613458 A) (* -0.0638541728 B))
        s3 (+ L (* -0.0894841775 A) (* -1.2914855480 B))
        l  (* l3 l3 l3)
        m  (* m3 m3 m3)
        s  (* s3 s3 s3)
        rl (+ (* 4.0767416621 l) (* -3.3077115913 m) (* 0.2309699292 s))
        gl (+ (* -1.2684380046 l) (* 2.6097574011 m) (* -0.3413193965 s))
        bl (+ (* -0.0041960863 l) (* -0.7034186147 m) (* 1.7076147010 s))
        ch (fn [v] (int (Math/round (* 255.0 (linear->srgb (double v))))))]
    [(ch rl) (ch gl) (ch bl)
     (int (Math/round (* 255.0 (max 0.0 (min 1.0 (double (or alpha 1.0)))))))]))


(defn- blend-oklch [lab1 lab2 t]
  (let [[l1 c1 h1 a1] (oklab->oklch lab1)
        [l2 c2 h2 a2] (oklab->oklch lab2)
        ;; neutrals have no meaningful hue; borrow the other endpoint's so
        ;; a gray-to-color blend does not spin through arbitrary hues
        h1 (if (< c1 1e-4) h2 h1)
        h2 (if (< c2 1e-4) h1 h2)
        d  (let [d (- h2 h1)]
             (cond (> d 180.0)  (- d 360.0)
                   (< d -180.0) (+ d 360.0)
                   :else d))]
    (oklch->oklab [(lerp l1 l2 t)
                   (lerp c1 c2 t)
                   (mod (+ h1 (* d t)) 360.0)
                   (lerp a1 a2 t)])))

(defn- ->rgba-vec [c]
  (if (string? c)
    (hex->rgba c)
    (if (= 4 (count c)) (vec c) (conj (vec c) 255))))

(defn- blend-fn [space]
  (case space
    :oklab blend-oklab
    :oklch blend-oklch
    (throw (ex-info "Unknown blend space" {:space space}))))

(defn blend
  "Blend two colors (hex strings or [r g b (a)] vectors) at t in 0..1,
   through Oklab by default or Oklch with {:space :oklch}. Oklab takes the
   straight perceptual line (chroma may dip a little through the middle);
   Oklch holds chroma and walks the hue wheel by the shorter arc, keeping
   blends vivid. Returns [r g b a]; feed it to rgba->hex if hex is wanted."
  ([c1 c2 t] (blend c1 c2 t {}))
  ([c1 c2 t {:keys [space] :or {space :oklab}}]
   (oklab->rgba ((blend-fn space)
                 (rgba->oklab (->rgba-vec c1))
                 (rgba->oklab (->rgba-vec c2))
                 (double t)))))

(defn ramp
  "Resample colors into n stops blended perceptually, for gradients that
   avoid sRGB's muddy midpoints. SVG and CSS interpolate gradient stops in
   raw sRGB, so handing them a dense perceptual ramp (16-32 stops) makes
   the rendered gradient follow the Oklab path to within invisible error:
 
     (svg/gradient-svg (color/ramp (:hex palette) 24))
 
   colors are hex strings or [r g b (a)] vectors; hex in means hex out,
   and alpha is interpolated alongside the color.
   opts:
     :space   :oklab (default) or :oklch (vivid: holds chroma, walks hue)
     :weights one number per color; positions each color at the center of
              its prominence band, so a dominant color holds more of the
              ramp (the same semantics as palette :weights)."
  ([colors n] (ramp colors n {}))
  ([colors n {:keys [space weights] :or {space :oklab}}]
   (let [hex-out?  (string? (first colors))
         labs      (mapv (comp rgba->oklab ->rgba-vec) colors)
         k         (count labs)
         f         (blend-fn space)
         positions (if (seq weights)
                     (do (when (not= (count weights) k)
                           (throw (ex-info "Weight count must match color count"
                                           {:weights weights :colors k})))
                         (let [total (reduce + 0.0 weights)]
                           (loop [ws weights cum 0.0 acc []]
                             (if (empty? ws)
                               acc
                               (let [w (double (first ws))]
                                 (recur (rest ws) (+ cum w)
                                        (conj acc (/ (+ cum (/ w 2.0)) total))))))))
                     (if (= k 1)
                       [0.5]
                       (mapv #(/ (double %) (dec k)) (range k))))
         sample    (fn [t]
                     (cond
                       (<= t (first positions)) (first labs)
                       (>= t (peek positions))  (peek labs)
                       :else
                       (let [i  (dec (count (take-while #(<= % t) positions)))
                             p1 (double (nth positions i))
                             p2 (double (nth positions (inc i)))
                             lt (if (== p1 p2) 0.0 (/ (- t p1) (- p2 p1)))]
                         (f (nth labs i) (nth labs (inc i)) lt))))
         out       (mapv (fn [i]
                           (oklab->rgba
                            (sample (if (= n 1) 0.5 (/ (double i) (dec n))))))
                         (range n))]
     (if hex-out?
       (mapv (fn [[_ _ _ a :as c]]
               (if (< a 255) (rgba->hex c true) (rgba->hex c)))
             out)
       out))))