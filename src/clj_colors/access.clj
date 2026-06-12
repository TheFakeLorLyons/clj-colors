(ns clj-colors.access
  "Thematic getters over the registry: by family, category, and tag, plus hex /
   rgb extraction and random selection. Built for downstream tools (landscape
   generation and the like) that ask for 'an orange palette' or 'a blue
   gradient' and want the colors back directly."
  (:require [clj-colors.main :as main]))

(def families
  "The computed color families, in hue order."
  [:red :orange :yellow :green :teal :blue :indigo :purple :pink :neutral])

(defn palettes-by-family
  "key->palette map of palettes whose computed :family is `family`."
  [family]
  (into {} (filter (fn [[_ p]] (= family (:family p))) (main/all-palettes))))

(defmacro ^:private def-family-getters
  []
  (let [fams [:red :orange :yellow :green :teal :blue :indigo :purple :pink :neutral]]
    `(do ~@(for [f fams]
             `(defn ~(symbol (str "get-" (name f) "-palettes"))
                ~(str "key->palette map of every palette whose family is " (name f) ".")
                []
                (palettes-by-family ~f))))))

(def-family-getters)

(defn get-category-palettes
  "key->palette map of every palette in a category, e.g. :ocean."
  [category]
  (main/palettes-in-category category))

(defn get-tagged-palettes
  "Find palettes by tags. With no tags, returns the whole registry in
   either match mode.

   Defaults to relaxed matching:
     (get-tagged-palettes \"forest\" \"earth-tone\")
      => Returns palettes tagged with either \"forest\" or \"earth-tone\".

   Require all tags:
     (get-tagged-palettes {:match :all}
                          \"forest\"
                          \"earth-tone\")
      => Returns an empty map, because no palette carries both."
  [& args]
  (let [[opts tags]
        (if (map? (first args))
          [(first args) (rest args)]
          [{} args])
        match-mode (:match opts :any)]
    (if (empty? tags)
      (main/all-palettes)
      (case match-mode
        :all (apply main/palettes-with-tags tags)
        :any (apply main/palettes-with-any-tags tags)
        (throw
         (ex-info "Unknown match mode"
                  {:match match-mode}))))))

; Color extraction
(defn palette-hex
  "Vector of hex strings for a palette key (full or bare), or nil."
  [k]
  (:hex (main/get-palette k)))

(defn palette-rgb
  "Vector of [r g b a] vectors for a palette key, or nil."
  [k]
  (:rgba (main/get-palette k)))

(defn family-hex
  "Vector of hex vectors, one per palette in the family."
  [family]
  (mapv :hex (vals (palettes-by-family family))))

;; Random selection, for generative callers

(defn- pool
  [{:keys [family category tags min-count] :or {min-count 0}}]
  (cond->> (vals (main/all-palettes))
    family      (filter #(= family (:family %)))
    category    (filter #(= category (:category %)))
    (seq tags)  (filter #(every? (:tags %) tags))
    true        (filter #(>= (:count %) min-count))))

(defn random-palette
  "A random enriched palette matching opts, or nil.
   opts: :family :category :tags (coll) :min-count."
  [opts]
  (let [candidates (vec (pool opts))]
    (when (seq candidates) (rand-nth candidates))))

(defn random-hex
  "Hex vector of a random palette matching opts, or nil. See random-palette."
  [opts]
  (:hex (random-palette opts)))

(defn random-rgb
  "[r g b a] vectors of a random palette matching opts, or nil."
  [opts]
  (:rgba (random-palette opts)))