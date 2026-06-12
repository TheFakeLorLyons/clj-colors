(ns clj-colors.svg
  "Render palettes as SVG, either as discrete color blocks or as a smooth
   linear gradient suitable as a base for sky, water, terrain, and the like.
   Returns SVG strings with no external dependencies. Sizes are in user units."
  (:require [clojure.string :as str]
            [clj-colors.color :as color])
  (:import (java.util Locale)))

(defn- fmt
  "Locale-stable format: always a '.' decimal separator regardless of the
   JVM's default locale."
  [pattern & args]
  (String/format Locale/ROOT pattern (to-array args)))

(defn- ->hex [c]
  (if (string? c) c (color/rgba->hex c)))

(defn- ->rgba [c]
  (cond
    (string? c) (color/hex->rgba c)
    (= 4 (count c)) (vec c)
    :else (conj (vec c) 255)))

(defn swatch-svg
  "One rectangle per color.
   opts: :width :height :orientation (:horizontal or :vertical)."
  ([colors] (swatch-svg colors {}))
  ([colors {:keys [width height orientation]
            :or {width 500 height 100 orientation :horizontal}}]
   (let [hexes (mapv ->hex colors)
         n (count hexes)
         horizontal? (= orientation :horizontal)
         span (double (if horizontal? width height))
         block (/ span n)
         rect (fn [i hx]
                (let [pos (* i block)]
                  (if horizontal?
                    (fmt "<rect x=\"%.4f\" y=\"0\" width=\"%.4f\" height=\"%d\" fill=\"%s\"/>"
                         pos block height hx)
                    (fmt "<rect x=\"0\" y=\"%.4f\" width=\"%d\" height=\"%.4f\" fill=\"%s\"/>"
                         pos width block hx))))]
     (str (fmt "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">"
               width height width height)
          (str/join (map-indexed rect hexes))
          "</svg>"))))

(defn gradient-svg
  "A smooth linear gradient across the colors.
   opts: :width :height :orientation (:vertical or :horizontal) :id."
  ([colors] (gradient-svg colors {}))
  ([colors {:keys [width height orientation id]
            :or {width 500 height 200 orientation :vertical id "swatch-gradient"}}]
   (let [hexes (mapv ->hex colors)
         n (count hexes)
         denom (double (max 1 (dec n)))
         stop (fn [i hx]
                (fmt "<stop offset=\"%.2f%%\" stop-color=\"%s\"/>"
                     (* 100.0 (/ (double i) denom)) hx))
         [x2 y2] (if (= orientation :horizontal) ["100%" "0%"] ["0%" "100%"])]
     (str (fmt "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">"
               width height width height)
          (fmt "<defs><linearGradient id=\"%s\" x1=\"0%%\" y1=\"0%%\" x2=\"%s\" y2=\"%s\">"
               id x2 y2)
          (str/join (map-indexed stop hexes))
          "</linearGradient></defs>"
          (fmt "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" fill=\"url(#%s)\"/>"
               width height id)
          "</svg>"))))

(defn alpha-gradient-svg
  "Like gradient-svg, but honors each color's alpha through stop-opacity, so a
   faded swatch (see clj-colors.fade) renders as a real transparency ramp.
   Colors are [r g b a] vectors or hex strings. A checkerboard backdrop makes
   the transparency visible.
   opts: :width :height :orientation :id :checkerboard?."
  ([colors] (alpha-gradient-svg colors {}))
  ([colors {:keys [width height orientation id checkerboard?]
            :or {width 500 height 200 orientation :vertical id "swatch-fade"
                 checkerboard? true}}]
   (let [rgbas (mapv ->rgba colors)
         n (count rgbas)
         denom (double (max 1 (dec n)))
         stop (fn [i [r g b a]]
                (fmt "<stop offset=\"%.2f%%\" stop-color=\"%s\" stop-opacity=\"%.3f\"/>"
                     (* 100.0 (/ (double i) denom))
                     (color/rgba->hex [r g b])
                     (/ (double a) 255.0)))
         [x2 y2] (if (= orientation :horizontal) ["100%" "0%"] ["0%" "100%"])
         checker (when checkerboard?
                   (str "<defs><pattern id=\"checker\" width=\"16\" height=\"16\" "
                        "patternUnits=\"userSpaceOnUse\">"
                        "<rect width=\"16\" height=\"16\" fill=\"#FFFFFF\"/>"
                        "<rect width=\"8\" height=\"8\" fill=\"#CCCCCC\"/>"
                        "<rect x=\"8\" y=\"8\" width=\"8\" height=\"8\" fill=\"#CCCCCC\"/>"
                        "</pattern></defs>"
                        (fmt "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" fill=\"url(#checker)\"/>"
                             width height)))]
     (str (fmt "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">"
               width height width height)
          checker
          (fmt "<defs><linearGradient id=\"%s\" x1=\"0%%\" y1=\"0%%\" x2=\"%s\" y2=\"%s\">"
               id x2 y2)
          (str/join (map-indexed stop rgbas))
          "</linearGradient></defs>"
          (fmt "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" fill=\"url(#%s)\"/>"
               width height id)
          "</svg>"))))

(defn spit-svg
  "Write an SVG string to a file path and return the path."
  [path svg]
  (spit path svg)
  path)