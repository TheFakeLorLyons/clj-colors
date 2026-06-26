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

(defn get-tagged-palettes
  "Find palettes by attribute tags (the semantic kind from
   :attributes :tags). With no tags, returns the whole registry.

   Defaults to relaxed matching (any-mode):
     (get-tagged-palettes :passionate :bold)
       => palettes carrying either tag

   Require all tags:
     (get-tagged-palettes {:match :all} :passionate :bold)

   Custom threshold (default 0.0 = any presence):
     (get-tagged-palettes {:match :all :threshold 0.05} :passionate)

   Tags absent from a palette never match, regardless of threshold."
  [& args]
  (let [[opts tags] (if (map? (first args))
                      [(first args) (rest args)]
                      [{} args])
        match-mode  (:match opts :any)
        threshold   (:threshold opts 0.0)
        all?        (= match-mode :all)
        score-ok?   (fn [attr-tags tag]
                      (when-let [score (get attr-tags tag)]
                        (>= score threshold)))
        pred (fn [p]
               (let [attr-tags (get-in p [:attributes :tags] {})]
                 (if all?
                   (every? #(score-ok? attr-tags %) tags)
                   (some   #(score-ok? attr-tags %) tags))))]
    (if (empty? tags)
      (main/all-palettes)
      (into {} (filter (fn [[_ p]] (pred p)) (main/all-palettes))))))

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
  [{:keys [family category attr-tags min-count attr-threshold]
    :or {min-count 0 attr-threshold 0.0}}]
  (cond->> (vals (main/all-palettes))
    family        (filter #(= family (:family %)))
    category      (filter #(= category (:category %)))
    (seq attr-tags)
    (filter (fn [p]
              (every? (fn [t]
                        (when-let [score (get-in p [:attributes :tags t])]
                          (>= score attr-threshold)))
                      attr-tags)))
    true          (filter #(>= (:count %) min-count))))

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