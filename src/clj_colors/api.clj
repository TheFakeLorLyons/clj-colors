(ns clj-colors.api
  "Curated public interface. Require this one namespace to reach the common
   functions for fetching palettes, pulling their colors, and building faded
   gradients. Call (print-api) for a listing, or (catalog) for plain data a tool
   or model can scan."
  (:require [clojure.string :as str]
            [clj-colors.access :as access]
            [clj-colors.extensions :as extensions]
            [clj-colors.main :as main]
            [clj-colors.fade :as fade]
            [clj-colors.svg :as svg]))

(defmacro ^:private export
  [target]
  `(do
     (def ~(symbol (name target)) (var ~target))
     (alter-meta! (var ~(symbol (name target)))
                  merge
                  (meta (var ~target)))))

; Lookups and listings
(export main/get-palette)
(export main/palette-keys)
(export main/palette-names)
(export main/categories)
(export main/all-palettes)
(export main/palettes-in-category)

; Thematic getters
(export access/palettes-by-family)
(export access/get-red-palettes)
(export access/get-orange-palettes)
(export access/get-yellow-palettes)
(export access/get-green-palettes)
(export access/get-teal-palettes)
(export access/get-blue-palettes)
(export access/get-indigo-palettes)
(export access/get-purple-palettes)
(export access/get-pink-palettes)
(export access/get-neutral-palettes)
(export access/get-tagged-palettes)


(export extensions/list-loaded)

; Color extraction
(export access/palette-hex)
(export access/palette-rgb)
(export access/family-hex)

; Random selection for generative callers
(export access/random-palette)
(export access/random-hex)
(export access/random-rgb)

; Fades / transparency ramps
(export fade/fade)
(export fade/fade-in)
(export fade/fade-out)
(export fade/fade-hex)
(export fade/with-alpha)

; SVG
(export svg/swatch-svg)
(export svg/gradient-svg)
(export svg/alpha-gradient-svg)
(export svg/spit-svg)

(defn print-api
  "Print every public function here with its arglists and docstring summary."
  []
  (doseq [[sym v] (sort-by key (ns-publics 'clj-colors.api))]
    (let [m (meta v)]
      (when (:arglists m)
        (println (format "%-24s %s" (str sym) (pr-str (:arglists m))))
        (when (:doc m)
          (println "    " (first (str/split-lines (:doc m)))))))))

(defn catalog
         "Lightweight data view of every palette: a vector of maps with :key,
   :family, :category, :count, attribute :tags, and :hex. Handy to hand
   to a tool or model so it can choose a palette without loading the
   whole registry."
         []
         (->> (main/all-palettes)
              (map (fn [[k p]]
                     {:key      k
                      :family   (:family p)
                      :category (:category p)
                      :count    (:count p)
                      :tags     (get-in p [:attributes :tags] {})
                      :hex      (:hex p)}))
              (sort-by :key)
              vec))

(comment
  ; Protocols answer the question "how do different types implement the same
  ; operations", not "what functions does this library expose". If you later
  ; want pluggable palette sources (the bundled set, a user file, a remote
  ; service) behind one interface, that is the case for a protocol:
  (defprotocol PaletteSource
    (-lookup [src k])
    (-all [src]))
  ; and you would extend it to a record per source. For the public surface of
  ; this library, this api namespace plus docstrings is the idiomatic answer.
  )