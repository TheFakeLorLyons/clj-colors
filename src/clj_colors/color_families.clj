(ns clj-colors.color-families
  "Corpus-derived vocabulary of color-family terms. The :color-families
   field on each color entry is the unfiltered keyword set from
   Color-Pedia's Keywords column; this namespace exposes the union
   across all colors as a single set, persisted to
   resources/color_families.edn for fast loading and inspection.

   The vocabulary is derived rather than hand-curated. Regenerate with
   derive-and-save! after any change to the color tag base; the
   Color-Pedia ingest does this automatically."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]))

(defonce data
  (delay (some-> "color_families.edn" io/resource slurp edn/read-string)))

(defn vocabulary
  "The full set of color-family keywords known to the registry."
  []
  @data)

(defn includes?
  "True when the given keyword is in the derived vocabulary."
  [kw]
  (contains? @data kw))

(defn derive-families
  "Compute the vocabulary as the union of :color-families sets across a
   color tag map."
  [color-tag-data]
  (->> color-tag-data
       vals
       (mapcat :color-families)
       set))

(defn derive-and-save!
  "Recompute the vocabulary from the color tag base and persist it to
   resources/color_families.edn. Called automatically by the Color-Pedia
   ingest; also runnable standalone."
  ([color-tag-data]
   (derive-and-save! color-tag-data "resources/color_families.edn"))
  ([color-tag-data out-path]
   (let [vocab (derive-families color-tag-data)]
     (spit out-path (with-out-str (pp/pprint vocab)))
     (println "Wrote vocabulary of" (count vocab) "color-families to" out-path)
     vocab)))