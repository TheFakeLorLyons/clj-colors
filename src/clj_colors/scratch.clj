(ns scratch
  "Interactive examples. Evaluate forms in the comment block at a REPL."
  (:require [clj-colors.access :as access]
            [clj-colors.associations :as associations]
            [clj-colors.api :as api]
            [clj-colors.color-tags :as color-tags]
            [clj-colors.extensions :as extensions]
            [clj-colors.fade :as fade]
            [clj-colors.main :as main]
            [clj-colors.meta :as meta]
            [clj-colors.ingest.smooth-weights :as smooth]
            [clj-colors.svg :as svg]
            [clj-colors.llm.batch :as batch]
            [clj-colors.llm.associative :as associative]
            [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]))

;; Palette inspection ----------------------------------------------------------

(defn inspect
  "Pretty-print a palette's full enriched form."
  [k]
  (when-let [p (main/get-palette k)]
    (pp/pprint p)
    nil))

(defn attrs
  "Just the :attributes of a palette, sorted by weight descending."
  [k]
  (when-let [p (main/get-palette k)]
    {:tags         (sort-by val > (:tags (:attributes p)))
     :associations (sort-by val > (:associations (:attributes p)))}))

(defn top-attrs
  "Top n tags and associations on a palette, by score."
  ([k] (top-attrs k 5))
  ([k n]
   (when-let [p (main/get-palette k)]
     {:tags         (take n (sort-by val > (:tags (:attributes p))))
      :associations (take n (sort-by val > (:associations (:attributes p))))})))

;; Corpus statistics -----------------------------------------------------------

(defn tag-frequencies
  "How many palettes carry each attribute tag (by presence, not weight)."
  []
  (->> (main/all-palettes)
       vals
       (mapcat (comp keys :tags :attributes))
       frequencies
       (sort-by val >)))

(defn association-frequencies
  "How many palettes match each named association."
  []
  (->> (main/all-palettes)
       vals
       (mapcat (comp keys :associations :attributes))
       frequencies
       (sort-by val >)))

(defn family-distribution
  "Count of palettes per color family."
  []
  (->> (main/all-palettes)
       vals
       (map :family)
       frequencies
       (sort-by val >)))

(defn coverage
  "Fraction of palettes with non-empty attributes."
  []
  (let [all (vals (main/all-palettes))
        n   (count all)]
    {:total             n
     :with-tags         (count (filter #(seq (:tags (:attributes %))) all))
     :with-associations (count (filter #(seq (:associations (:attributes %))) all))
     :with-both         (count (filter #(and (seq (:tags (:attributes %)))
                                             (seq (:associations (:attributes %))))
                                       all))}))

;; IDF exploration -------------------------------------------------------------

(defn rarest-tags
  "Tags with the highest IDF (least common across the color corpus).
   These get the largest score boost when they appear on a palette."
  ([] (rarest-tags 20))
  ([n]
   (->> (color-tags/tag-idf-memo)
        (sort-by val >)
        (take n))))

(defn commonest-tags
  "Tags with the lowest IDF (most common across the color corpus).
   These get the smallest score boost when they appear on a palette."
  ([] (commonest-tags 20))
  ([n]
   (->> (color-tags/tag-idf-memo)
        (sort-by val)
        (take n))))

;; Querying --------------------------------------------------------------------

(defn top-for-tag
  "Palettes scoring highest on a given attribute tag."
  ([tag] (top-for-tag tag 10))
  ([tag n]
   (->> (main/all-palettes)
        (filter (fn [[_ p]] (get-in p [:attributes :tags tag])))
        (sort-by (fn [[_ p]] (get-in p [:attributes :tags tag])) >)
        (take n)
        (map (fn [[k p]] [k (get-in p [:attributes :tags tag])])))))

(defn top-for-association
  "Palettes scoring highest on a given association."
  ([assoc-key] (top-for-association assoc-key 10))
  ([assoc-key n]
   (->> (main/all-palettes)
        (filter (fn [[_ p]] (get-in p [:attributes :associations assoc-key])))
        (sort-by (fn [[_ p]] (get-in p [:attributes :associations assoc-key])) >)
        (take n)
        (map (fn [[k p]] [k (get-in p [:attributes :associations assoc-key])])))))

;; Color-level lookups ---------------------------------------------------------

(defn what-tags
  "Color-Pedia tags for a hex code, sorted by weight."
  [hex]
  (some->> (color-tags/tags hex)
           (sort-by (comp :weight val) >)))

(defn what-families
  "Color-families set for a hex code."
  [hex]
  (color-tags/color-families hex))

;; Tag frequency distribution -----------------------------------------------

(defn tag-occurrences
  "Map of tag-keyword -> how many colors carry that tag."
  []
  (->> @color-tags/data
       vals
       (mapcat (comp keys :tags))
       frequencies))

(defn tag-occurrence-summary
  "Top-level stats on the tag frequency distribution: min, max, mean,
   percentiles. The right input for picking rarity band boundaries."
  []
  (let [counts (vec (sort (vals (tag-occurrences))))
        n      (count counts)
        at     (fn [p] (nth counts (min (dec n) (int (* p (dec n))))))]
    {:total-distinct-tags n
     :min     (first counts)
     :max     (last counts)
     :mean    (double (/ (reduce + counts) n))
     :median  (at 0.5)
     :p10     (at 0.10)
     :p25     (at 0.25)
     :p75     (at 0.75)
     :p90     (at 0.90)
     :p95     (at 0.95)
     :p99     (at 0.99)}))

(defn tags-in-band
  "Count of distinct tags whose occurrence count falls in [lo, hi)."
  [lo hi]
  (let [counts (vals (tag-occurrences))]
    (count (filter (fn [c] (and (>= c lo) (< c hi))) counts))))

(defn band-histogram
  "Histogram of tag counts across the given band boundaries. Each row
   is [range tag-count]. Use to see how candidate band edges would
   distribute tags across rarity tiers.

     (band-histogram [1 10 25 50 100 250 1000 10000])"
  [edges]
  (mapv (fn [[lo hi]]
          [(str lo "-" (dec hi)) (tags-in-band lo hi)])
        (partition 2 1 edges)))

(defn rarest-by-count
  "Tags sorted by occurrence count, ascending. The first N are the
   rarest in the corpus."
  ([] (rarest-by-count 30))
  ([n]
   (->> (tag-occurrences)
        (sort-by val)
        (take n))))

(defn commonest-by-count
  "Tags sorted by occurrence count, descending."
  ([] (commonest-by-count 30))
  ([n]
   (->> (tag-occurrences)
        (sort-by val >)
        (take n))))

;; Tag comparisons

(defn compare-color-tags
  "Side-by-side raw and smoothed weights for one color, sorted by
   the smoothed weight. Useful for verifying smoothing is reinforcing
   the tags you'd expect on a known color."
  [hex]
  (let [base (edn/read-string (slurp "resources/color_tags_base.edn"))
        smooth-file (java.io.File. "resources/color_tags.edn")
        smooth (when (.exists smooth-file)
                 (edn/read-string (slurp smooth-file)))
        old (some-> base (get hex) :tags)
        new (some-> smooth (get hex) :tags)
        tags (set/union (set (keys (or old {})))
                        (set (keys (or new {}))))]
    (->> tags
         (map (fn [t] [t {:raw      (get-in old [t :weight] 0.0)
                          :smoothed (get-in new [t :weight] 0.0)}]))
         (sort-by (fn [[_ {:keys [smoothed]}]] (- smoothed))))))

(defn smoothing-stats
  "Corpus-wide before/after stats so you can see whether smoothing
   is making things sparser, denser, or about the same."
  []
  (let [base (edn/read-string (slurp "resources/color_tags_base.edn"))
        smooth (edn/read-string (slurp "resources/color_tags.edn"))
        avg-tags (fn [data]
                   (double (/ (reduce + 0 (map (comp count :tags val) data))
                              (count data))))]
    {:raw-avg-tags-per-color      (avg-tags base)
     :smoothed-avg-tags-per-color (avg-tags smooth)
     :colors-with-zero-tags-raw   (count (filter #(empty? (:tags %)) (vals base)))
     :colors-with-zero-tags-smooth (count (filter #(empty? (:tags %)) (vals smooth)))}))

(defn tags-for-family
  "Top n tags by smoothed weight aggregated across all colors in a
   family. Handy for sanity-checking that families have the profiles
   you'd intuitively expect."
  ([family] (tags-for-family family 15))
  ([family n]
   (let [data (edn/read-string (slurp "resources/color_tags.edn"))
         in-family? (fn [entry]
                      (let [[h s l] (:hsl entry)]
                        (= family (meta/classify-family
                                   h (/ s 100.0) (/ l 100.0)))))]
     (->> (vals data)
          (filter in-family?)
          (mapcat (fn [e] (for [[t {:keys [weight]}] (:tags e)]
                            [t weight])))
          (reduce (fn [acc [t w]]
                    (update acc t (fnil + 0.0) (double w)))
                  {})
          (sort-by (comp - val))
          (take n)))))

;; Validation runs -------------------------------------------------------------

(defn trace-smooth
  "Run smooth-color directly on one hex and report intermediates."
  [hex]
  (let [raw      (edn/read-string (slurp "resources/color_tags_base.edn"))
        params   smooth/default-params
        profiles (smooth/build-family-profiles raw)
        grid     (smooth/build-spatial-grid raw (:grid-cells params))
        entry    (get raw hex)
        fam      (smooth/family-of entry)
        profile  (get profiles fam)
        own-tags (set (keys (:tags entry)))
        first-t  (first own-tags)
        result   (smooth/smooth-color hex entry raw profiles grid params)]
    {:hex              hex
     :raw-tags-count   (count (:tags entry))
     :own-tags         own-tags
     :family           fam
     :profile-nil?     (nil? profile)
     :profile-size     (:size profile)
     :first-tag        first-t
     :first-tag-type   (type first-t)
     :contains-first?  (contains? own-tags first-t)
     :result-tags      (:tags result)
     :result-tag-count (count (:tags result))}))

(defn inspect-tags
  "Recompute attribute tags for one palette at custom precision and
   threshold without mutating the registry. Defaults to threshold
   0.001 (essentially unfiltered) so weak signals stay visible for
   diagnostic inspection. Pass an explicit threshold (e.g. 0.015 to
   match current production) to preview what refresh-all-attributes!
   would actually store.

   Returns a sorted vector of [tag score] pairs, descending."
  ([k] (inspect-tags k 4 0.001))
  ([k digits] (inspect-tags k digits 0.001))
  ([k digits threshold]
   (when-let [p (main/get-palette k)]
     (let [hexes  (:hex p)
           cnt    (:count p)
           ws     (or (:weights p)
                      (vec (repeat cnt (/ 1.0 cnt))))
           dist   (main/palette-distribution hexes ws)
           oklabs (color-tags/palette-oklabs hexes)
           attrs  (associations/palette-attributes
                   hexes ws dist oklabs
                   {:tag-threshold   threshold
                    :assoc-threshold 0.01
                    :presence-floor  0.05
                    :digits          digits})]
       (->> (:tags attrs)
            (sort-by (comp - val))
            vec)))))

(defn inspect-palette
  "Detailed view of a palette as currently in the registry. Shows
   computed attributes tags and associations sorted by weight,
   with optional rounding precision."
  ([k] (inspect-palette k 3))
  ([k digits]
   (let [palette (main/get-palette k)
         attrs   (:attributes palette)
         tags    (->> (:tags attrs)
                      (sort-by (comp - val))
                      (mapv (fn [[tag w]]
                              [tag (smooth/round-to w digits)])))
         assocs  (->> (:associations attrs)
                      (sort-by (comp - val))
                      (mapv (fn [[a w]]
                              [a (smooth/round-to w digits)])))]
     {:key k
      :hex (:hex palette)
      :family (:family palette)
      :category (:category palette)
      :user-tags (:tags palette)
      :computed-tags tags
      :associations assocs
      :tag-count (count tags)
      :association-count (count assocs)})))

(defn tag-sources
  "For a palette key and a tag, show which of the palette's colors
   carry that tag in color-tags data, with their weights. Useful for
   tracking down why a specific tag appears on a palette."
  [palette-k tag]
  (let [palette (main/get-palette palette-k)
        hexes (:hex palette)]
    (->> hexes
         (mapv (fn [hex]
                 (let [tag-data (get-in @color-tags/data
                                        [(clojure.string/lower-case hex)
                                         :tags tag])]
                   {:hex hex
                    :weight (:weight tag-data)
                    :source (:source tag-data)
                    :pole (:pole tag-data)})))
         (filter :weight))))

(defn reload-all!
  "Refresh the full pipeline after underlying data changes: rebuild
   color-tags signatures, refresh associations, and re-enrich every
   palette in the registry. Use after editing color_tags.edn,
   associations.edn, or any extension file in resources/extensions/."
  []
  (color-tags/invalidate!)
  (color-tags/refresh-signatures!)
  (associations/refresh!)
  (main/reset-registry!)
  :reloaded)

(comment

  ; --- base lookups ---
  (main/get-palette :holiday/halloween)
  (main/get-palette :jungle) ; bare name works too
  (main/palette-keys)
  (main/categories)
  (keys (main/palettes-in-category :ocean))

  ; --- attribute-tag getters (access) ---
  ; any-match: palettes carrying either tag
  (keys (access/get-tagged-palettes :passionate :bold))
  ; all-match: palettes carrying both
  (keys (access/get-tagged-palettes {:match :all} :calm :peaceful))
  ; threshold: only tags scoring above the bar
  (keys (access/get-tagged-palettes {:threshold 0.3} :warm))
  ; no tags: whole registry
  (keys (access/get-tagged-palettes))

  ; pull colors straight out
  (access/palette-hex :ocean/abyss)
  (access/palette-rgb :sunset/golden-hour)
  (access/family-hex :green)

  ; random selection for generative callers
  (access/random-hex {:family :blue :min-count 5})
  (access/random-hex {:category :sunset})
  (access/random-palette {:attr-tags [:neon] :attr-threshold 0.2})

  ; --- fades / transparency ramps (the cream) ---
  (fade/fade (access/palette-hex :ocean/oasis))
  (fade/fade-in (access/palette-hex :ocean/oasis))
  (fade/fade-out (access/palette-hex :spring/cherry-sky) {:curve :logarithmic})
  (fade/fade-in  (access/palette-hex :sunset/golden-hour) {:curve :exponential})
  (fade/fade (access/palette-hex :forest/fern) {:curve (fn [t] (* t t t))})
  (fade/fade (access/palette-hex :ocean/abyss) {:min-alpha 0.25})
  (fade/fade-hex (access/palette-hex :ocean/oasis) {:curve :ease-in})
  (fade/with-alpha (access/palette-hex :pastel/peachy) 0.5)

  ; --- render a faded gradient (honors alpha) ---
  (svg/spit-svg "/tmp/oasis-fade.svg"
                (svg/alpha-gradient-svg
                 (fade/fade-out (access/palette-hex :ocean/oasis)
                                {:curve :logarithmic})
                 {:width 400 :height 600}))

  ; --- the public interface ---
  (api/print-api)
  (count (api/catalog))
  (first (api/catalog))

  ; --- runtime mutation + persistence ---
  (main/register-palette! :forest/swamp
                          ["#0A140C" "#1C3220" "#3E5E38" "#6E9A52" "#C4E29A"])
  (main/unregister-palette! :forest/swamp)
  (main/save-registry! "/tmp/palettes-out.edn")

    ;; Inspect specific palettes you know well.
  (inspect :ocean/abyss)
  (top-attrs :synthwave/synthwave)
  (top-attrs :forest/alien-jungle 10)
  (attrs :neutral/ink)

  ;; What does the corpus look like overall?
  (coverage)
  (family-distribution)
  (take 20 (tag-frequencies))
  (take 20 (association-frequencies))
  (rarest-tags 15)
  (commonest-tags 15)

  ;; Inverse direction: find palettes by tag or association.
  (top-for-tag :passionate 10)
  (top-for-tag :calm 10)
  (top-for-tag :warm 10)
  (top-for-association :fire 10)

  ;; Threshold tuning: how many palettes clear different bars?
  (count (access/get-tagged-palettes :calm))
  (count (access/get-tagged-palettes {:threshold 0.1} :calm))
  (count (access/get-tagged-palettes {:threshold 1.0} :calm))

  ;; Per-color exploration.
  (what-tags "#1a8043")
  (what-families "#1a8043")
  (color-tags/lookup "#ff0000")

  ;; Random sampling.
  (main/random-palette)
  (access/random-palette {:family :blue})
  (access/random-palette {:attr-tags [:bold] :attr-threshold 0.5})

  ;; Pick a palette you know well and see what tags it earned
  (:attributes (clj-colors.main/get-palette :ocean/abyss))
  (:attributes (clj-colors.main/get-palette :synthwave/synthwave))
  (:attributes (clj-colors.main/get-palette :forest/alien-jungle))

  ;; Inverse direction: find palettes by tag
  (clj-colors.access/get-tagged-palettes {:threshold 0.3} :passionate)
  (clj-colors.access/get-tagged-palettes :calm :peaceful)
  (clj-colors.access/get-tagged-palettes {:match :all :threshold 0.2} :warm :bold)

  ;; What's the IDF distribution actually looking like?
  (->> (clj-colors.color-tags/tag-idf-memo)
       (sort-by val >)
       (take 10))
  (->> (clj-colors.color-tags/tag-idf-memo)
       (sort-by val)
       (take 10))

  ;; How many palettes light up for each association?
  (->> (clj-colors.main/all-palettes)
       vals
       (mapcat (comp keys :associations :attributes))
       frequencies
       (sort-by val >)
       (take 20))

  (smoothing-stats)
  ;; Corpus-wide: did smoothing make tags denser? sparser? How many zero-tag
  ;; colors now exist vs. before? Compare avg-tags before/after.

  (compare-color-tags "#ff0000")  ; pure red
  (compare-color-tags "#0000ff")  ; pure blue
  (compare-color-tags "#94cc44")  ; lime green
  (compare-color-tags "#000000")  ; black
  (compare-color-tags "#ffffff")  ; white
  (compare-color-tags "#808080")  ; medium gray


  (tags-for-family :red 15)
  (tags-for-family :blue 15)
  (tags-for-family :green 15)
  (tags-for-family :black 15)
  (tags-for-family :white 15)
  (tags-for-family :gray 15)
  ;{:raw-avg-tags-per-color 7.37412,
  ; :smoothed-avg-tags-per-color 9.20619,
  ; :colors-with-zero-tags-raw 0,
  ; :colors-with-zero-tags-smooth 0}

  (associative/propose-association "nightvision")
  ;; review the returned map

  ;; if happy as-is:
  (associative/accept! :technology/nightvision)

  ;; view the currently proposed entry:
  (:entry (associative/draft))
  (pp/pprint (:entry (associative/draft))) ; ...as formatted text
  (associative/show-draft) ; simple helper function to do the above.

  ;; if it needs touching up:
  (associative/modify-tag :verdant {:specificity 0.7})       ; loosen the verdant gate
  (associative/remove-tag :technological)                     ; drop a tag
  (associative/add-tag :light-amplification {:weight 0.6 :specificity 1.3})
  (associative/remove-color "#2d3d1a")                        ; trim a too-dark sample
  (associative/set-sigma 0.05)                                ; tighten the spatial breadth
  (associative/modify! #(assoc % :rationale "Custom rationale text..."))

  ;; alternatively:
  (associative/refine! "Lower :verdant specificity to 0.5 since it overlaps with foliage.
            Drop :technological — too vague. Add :light-amplification at
            weight 0.7, specificity 1.4. Tighten sigma to 0.05.")
  ;; review the modified draft

  (associative/refine! "Add one more very-dark sample around #1a2010 for the deepest
            shadow areas, and update the rationale to mention the
            luminance-range sampling explicitly.")
  ;; review again

  (associative/draft)                                         ; check current state
  (associative/accept! :technology/nightvision)


  ;; first, on disk:
  ;;   resources/extensions/palettes/holidays.edn
  ;;     contents:
  ;;     {:holidays/halloween {:hex ["#FF6B00" "#1A0E04" "#2D1810" "#FFD83D"]}
  ;;      :holidays/valentine {:hex ["#E94560" "#F38BA0" "#FFFFFF" "#9D0208"]}}

  (extensions/list-loaded)
  ;; should show the holidays.edn file

  (main/reset-registry!)
  (get-in @main/registry [:holidays/halloween])
  ;; should be the enriched holiday palette


  (:attributes (main/get-palette :arctic/arctic-water))

  (reload-all!)
  (inspect-palette :arctic/arctic-water)

  (tag-sources :arctic/arctic-water :airy)

  (get-in @color-tags/data ["#548eb6" :tags])

  (associations/palette-attributes
   (:hex (main/get-palette :arctic/arctic-water))
   (:weights (main/get-palette :arctic/arctic-water))
   {:tag-threshold 0.005})

  ; Set your key in the REPL session:
  (alter-var-root #'clj-colors.llm.core/*api-key* (constantly "sk-ant-api-key"))

  ; Creating associations en-bulk:
  (def authoring-batches
    [{:namespace :mineral
      :referents ["granite" "limestone" "serpentine" "shale"
                  "basalt" "sandstone" "slate" "marble"]
      :file "minerals_1"}
     {:namespace :flora
      :referents ["oak-leaf" "pine-needle" "birch-bark" "moss"
                  "lichen" "fern-frond" "maple-leaf" "willow-leaf"
                  "eucalyptus-leaf" "bracken"]
      :file "flora_1"}
     {:namespace :atmosphere
      :referents ["clear-noon" "overcast" "golden-hour" "blue-hour"
                  "civil-twilight" "nautical-twilight" "high-haze"]
      :file "atmosphere_1"}
     {:namespace :weather
      :referents ["snow-shadow" "storm-cell" "heat-shimmer"
                  "frozen-fog" "monsoon-rain" "dust-storm"]
      :file "weather_1"}
     {:namespace :water
      :referents ["glacier-melt" "peat-bog" "clear-shallow"
                  "storm-sea" "kelp-shallows" "tannin-stream"]
      :file "water_1"}])

  (def authoring-batches-2
    [{:namespace :industrial
      :referents ["sheet-metal" "conveyor-belt" "furnace"]
      :file "industrial_test_1"}
     {:namespace :characters/rick-and-morty
      :referents ["rick" "beth" "summer" "morty" "jerry"]
      :file "rick_and_morty"}])


  (binding [associative/*model* "claude-opus-4-7"]
    (doseq [{:keys [namespace referents file]} authoring-batches-2]
      (batch/batch-author!
       {:type :association
        :namespace namespace
        :referents referents
        :review-mode :auto
        :checkpoint-usd nil
        :skip-existing true
        :authored-file file})))

  ; Failed because of the nested map
  ;(batch/batch-modify!
  ; {:type :association
  ;  :modifications {:characters/rick-and-morty/rick " Good job but you missed the white lab coat. Can you add that?"
  ;                  :characters/rick-and-morty/beth " It was supposed to be Beth from Rick and Morty, not the symbol Beth from the Hebrew alphabet."
  ;                  :characters/rick-and-morty/jerry " Jerry has a characteristic shirt that is not represented in the colors."}
  ;  :review-mode :interactive})

  ; Leads to:
  ; Batch modify: 3 entries, checkpoint every $5.00 (already spent: $0.00)
  ; 
  ; [1/3] :rick ::  Good job but you missed the white lab coat. Can you add that? (spent $0.00)
  ; Execution error (ExceptionInfo) at clj-colors.llm.associative/load-as-draft! (associative.clj:521).
  ; No association with key


  ; This works, but I can't get the modified edn to be accepted and re-written (I may be missing some kind of accept/finalize function or something)
  (batch/batch-modify!
   {:type :association
    :modifications {;:rick-and-morty/rick " Good job but you missed the white lab coat. Can you add that?"
                    :rick-and-morty/beth " Add blue for Beth's pants."
                    :rick-and-morty/jerry " Jerry's shirt color isn't present (a green striped shirt with 2 stripes)."}
    :review-mode :interactive
    :authored-file "rick_and_morty"})
  

  ;; More category/list examples
  
  )