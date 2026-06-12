(ns clj-colors.hiccup
  "Hiccup forms for palettes, composable into larger SVG scenes.

   Where clj-colors.svg returns finished SVG document strings for file
   export, this namespace returns hiccup data for embedding inside other
   hiccup trees: Clay/kindly notebooks render the standalone forms under
   ^:kind/hiccup directly, and the gradient primitives slot into any
   existing [:svg ...] as layer fills.

   Callers supply gradient ids. Several SVGs on one HTML page share a
   single id namespace, so a hardcoded id makes every url(#id) on the
   page resolve to the first definition. The standalone display forms
   default to an id derived from the colors, which is deterministic and
   only collides when the content is identical anyway.

   All entry points accept hex strings or [r g b (a)] vectors and honor
   each color's alpha through stop-opacity, so faded swatches (see
   clj-colors.fade) render as real transparency ramps with no separate
   code path."
  (:require [clj-colors.color :as color]))

(defn- ->rgba [c]
  (cond
    (string? c)     (color/hex->rgba c)
    (= 4 (count c)) (vec c)
    :else           (conj (vec c) 255)))

(defn gradient-stops
  "Seq of [:stop ...] forms spread evenly across the colors, honoring
   each color's alpha. The composable primitive: put these inside any
   [:linearGradient ...] or [:radialGradient ...]."
  [colors]
  (let [rgbas (mapv ->rgba colors)
        denom (double (max 1 (dec (count rgbas))))]
    (map-indexed
     (fn [i [r g b a]]
       [:stop {:offset       (str (* 100.0 (/ (double i) denom)) "%")
               :stop-color   (color/rgba->hex [r g b])
               :stop-opacity (/ (double a) 255.0)}])
     rgbas)))

(defn linear-gradient
  "[:linearGradient ...] across the colors, ready to drop inside [:defs].
   opts: :orientation (:vertical default, or :horizontal)."
  ([id colors] (linear-gradient id colors {}))
  ([id colors {:keys [orientation] :or {orientation :vertical}}]
   (let [[x2 y2] (if (= orientation :horizontal) ["1" "0"] ["0" "1"])]
     (into [:linearGradient {:id id :x1 "0" :y1 "0" :x2 x2 :y2 y2}]
           (gradient-stops colors)))))

(defn gradient-fill
  "The fill attribute value referencing a gradient id."
  [id]
  (str "url(#" id ")"))

(defn- default-id [colors]
  (str "swatch-" (Math/abs (long (hash colors)))))

(defn gradient-rect
  "Standalone hiccup SVG of a smooth gradient, for ^:kind/hiccup display
   or as a full-bleed layer.
   opts: :width :height :orientation :id."
  ([colors] (gradient-rect colors {}))
  ([colors {:keys [width height orientation id]
            :or {width 400 height 200 orientation :vertical}}]
   (let [id (or id (default-id colors))]
     [:svg {:xmlns   "http://www.w3.org/2000/svg"
            :width   width
            :height  height
            :viewBox (str "0 0 " width " " height)}
      [:defs (linear-gradient id colors {:orientation orientation})]
      [:rect {:x 0 :y 0 :width width :height height
              :fill (gradient-fill id)}]])))

(defn swatch-blocks
  "Standalone hiccup SVG of discrete color blocks, one per color.
   opts: :width :height :orientation."
  ([colors] (swatch-blocks colors {}))
  ([colors {:keys [width height orientation]
            :or {width 500 height 100 orientation :horizontal}}]
   (let [rgbas       (mapv ->rgba colors)
         n           (count rgbas)
         horizontal? (= orientation :horizontal)
         span        (double (if horizontal? width height))
         block       (/ span n)]
     (into [:svg {:xmlns   "http://www.w3.org/2000/svg"
                  :width   width
                  :height  height
                  :viewBox (str "0 0 " width " " height)}]
           (map-indexed
            (fn [i [r g b a]]
              (let [pos  (* i block)
                    base {:fill (color/rgba->hex [r g b])
                          :fill-opacity (/ (double a) 255.0)}]
                (if horizontal?
                  [:rect (merge base {:x pos :y 0 :width block :height height})]
                  [:rect (merge base {:x 0 :y pos :width width :height block})])))
            rgbas)))))