(ns clj-colors.fade
  "Transparency ramps over a palette. Adjust the alpha channel across the colors
   of a swatch to fade out (toward transparent) or in (toward opaque) along a
   chosen easing curve, leaving the RGB channels untouched. Palette-agnostic: it
   takes hex strings, [r g b] / [r g b a] vectors, or the :hex / :rgb of a
   registry palette."
  (:require [clj-colors.color :as color]))

(defn- ->rgba [c]
  (cond
    (string? c)     (color/hex->rgba c)
    (= 4 (count c)) (vec c)
    :else           (conj (vec c) 255)))

(defn- ->rgbas [colors] (mapv ->rgba colors))

(defn- clamp01 [x] (max 0.0 (min 1.0 x)))

(def curves
  "Named easing curves, each mapping t in [0,1] to an eased fraction in [0,1].
   A custom curve is any fn with the same shape."
  {:linear      (fn [t] t)
   :ease-in     (fn [t] (* t t))
   :ease-out    (fn [t] (- 1.0 (* (- 1.0 t) (- 1.0 t))))
   :ease-in-out (fn [t] (if (< t 0.5)
                          (* 2.0 t t)
                          (- 1.0 (/ (Math/pow (- (* 2.0 t) 2.0) 2.0) 2.0))))
   :exponential (fn [t] (if (<= t 0.0) 0.0 (Math/pow 2.0 (* 10.0 (- t 1.0)))))
   :logarithmic (fn [t] (clamp01 (/ (Math/log (+ 1.0 (* 9.0 t))) (Math/log 10.0))))
   :sqrt        (fn [t] (Math/sqrt t))})

(defn- resolve-curve [curve]
  (cond
    (fn? curve)              curve
    (contains? curves curve) (get curves curve)
    :else (throw (ex-info "Unknown fade curve"
                          {:curve curve :known (vec (keys curves))}))))

(defn fade
  "Return [r g b a] vectors with alpha ramped across the colors.
     :direction :out  opaque first color -> transparent last (default)
                :in   transparent first -> opaque last
     :curve     a keyword in `curves` or a fn t->[0,1] (default :linear)
     :min-alpha lowest alpha fraction, 0..1 (default 0.0)
     :max-alpha highest alpha fraction, 0..1 (default 1.0)
   Only alpha changes; the RGB channels are preserved. To fade along the other
   spatial direction, reverse the colors first."
  ([colors] (fade colors {}))
  ([colors {:keys [direction curve min-alpha max-alpha]
            :or {direction :out curve :linear min-alpha 0.0 max-alpha 1.0}}]
   (let [rgbas (->rgbas colors)
         n     (count rgbas)
         f     (resolve-curve curve)
         denom (double (max 1 (dec n)))]
     (vec (map-indexed
           (fn [i [r g b _]]
             (let [t    (/ (double i) denom)
                   frac (f (clamp01 (if (= direction :in) t (- 1.0 t))))
                   a-fr (+ min-alpha (* (- max-alpha min-alpha) frac))
                   a    (long (Math/round (* 255.0 (clamp01 a-fr))))]
               [r g b a]))
           rgbas)))))

(defn fade-out
  "Opaque first color -> transparent last. See fade for opts."
  ([colors] (fade colors {:direction :out}))
  ([colors opts] (fade colors (assoc opts :direction :out))))

(defn fade-in
  "Transparent first color -> opaque last. See fade for opts."
  ([colors] (fade colors {:direction :in}))
  ([colors opts] (fade colors (assoc opts :direction :in))))

(defn fade-hex
  "Like fade, but returns #rrggbbaa hex strings (6-digit where fully opaque)."
  ([colors] (fade-hex colors {}))
  ([colors opts] (mapv #(color/rgba->hex % true) (fade colors opts))))

(defn with-alpha
  "Set every color's alpha to a constant fraction (0..1). Useful for a flat
   wash. Returns [r g b a] vectors."
  [colors fraction]
  (let [a (long (Math/round (* 255.0 (clamp01 fraction))))]
    (mapv (fn [c] (let [[r g b _] (->rgba c)] [r g b a])) colors)))