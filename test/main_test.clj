(ns main-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing run-tests]]
            [clj-colors.access :as access]
            [clj-colors.associations :as associations]
            [clj-colors.color :as color]
            [clj-colors.color-tags :as color-tags]
            [clj-colors.main :as main]))

;; Color conversions ------------------------------------------------------------

(deftest hex-roundtrip
  (testing "hex parses to rgba and back"
    (is (= [18 24 16 255] (color/hex->rgba "#121810")))
    (is (= "#121810" (color/rgba->hex (color/hex->rgba "#121810")))))
  (testing "shorthand expands"
    (is (= [170 187 204 255] (color/hex->rgba "#abc"))))
  (testing "alpha is preserved when requested"
    (is (= "#11223380" (color/rgba->hex [17 34 51 128] true)))))

;; Enrichment -------------------------------------------------------------------

(deftest metadata-shape
  (let [p (main/get-palette :forest/alien-jungle)]
    (testing "rgba count matches hex count"
      (is (= (count (:hex p)) (count (:rgba p))))
      (is (= (:count p) (count (:hex p)))))
    (testing "family is a keyword"
      (is (keyword? (:family p))))
    (testing "no top-level :tags on enriched palettes"
      (is (not (contains? p :tags))))
    (testing "scalar metadata sits in 0-1"
      (is (<= 0.0 (:brightness p) 1.0))
      (is (<= 0.0 (:saturation p) 1.0))
      (is (<= 0.0 (:temperature p) 1.0))
      (is (<= 0.0 (:contrast p) 1.0)))))

(deftest attributes-shape
  (testing "every enriched palette carries an :attributes map"
    (doseq [[k p] (main/all-palettes)]
      (is (map? (:attributes p))
          (str "attributes missing on " k))
      (is (map? (get-in p [:attributes :tags]))
          (str ":attributes :tags missing on " k))
      (is (map? (get-in p [:attributes :associations]))
          (str ":attributes :associations missing on " k))))
  (testing "attribute tag scores are keyword-keyed, non-negative numbers"
    (doseq [[_ p] (main/all-palettes)
            [t v] (get-in p [:attributes :tags])]
      (is (keyword? t))
      (is (number? v))
      (is (>= v 0.0))))
  (testing "attribute association scores are keyword-keyed, non-negative numbers"
    (doseq [[_ p] (main/all-palettes)
            [a v] (get-in p [:attributes :associations])]
      (is (keyword? a))
      (is (number? v))
      (is (>= v 0.0)))))

(deftest rgba-only-palettes-load
  (testing "a stored palette carrying only :rgba enriches"
    (let [reg (main/enrich-registry
               {:probe/rgb-only {:rgba [[16 16 16] [240 240 240 255]]}})
          p   (get reg :probe/rgb-only)]
      (is (= ["#101010" "#F0F0F0"] (:hex p)))
      (is (= 2 (:count p)))
      (is (map? (:attributes p))))))

(deftest authored-tags-do-not-survive-enrichment
  (testing "raw :tags input is ignored, not promoted, not preserved"
    (let [reg (main/enrich-registry
               {:probe/legacy {:hex ["#FF0000"]
                               :tags ["should-not-survive" "anywhere"]}})
          p   (get reg :probe/legacy)]
      (is (not (contains? p :tags)))
      (is (not (contains? (get-in p [:attributes :tags])
                          :should-not-survive))))))

;; Lookup -----------------------------------------------------------------------

(deftest bare-and-full-lookup-agree
  (is (= (main/get-palette :forest/jungle)
         (main/get-palette :jungle)))
  (is (nil? (main/get-palette :no-such-palette))))

(deftest category-grouping
  (is (contains? (set (main/categories)) :ocean))
  (let [ocean (main/palettes-in-category :ocean)]
    (is (every? (fn [[_ p]] (= :ocean (:category p))) ocean))))

;; Attribute-tag queries --------------------------------------------------------

(deftest get-tagged-palettes-modes
  (let [fake {:test/a {:attributes {:tags {:warm 0.8 :passionate 0.3}}}
              :test/b {:attributes {:tags {:warm 0.5 :cool 0.2}}}
              :test/c {:attributes {:tags {:cool 0.9}}}
              :test/d {:attributes {:tags {}}}}]
    (with-redefs [main/all-palettes (constantly fake)]
      (testing "no tags returns the whole registry, both modes"
        (is (= fake (access/get-tagged-palettes)))
        (is (= fake (access/get-tagged-palettes {:match :all}))))
      (testing "any-mode (default) finds palettes carrying any of the tags"
        (let [result (access/get-tagged-palettes :warm)]
          (is (= #{:test/a :test/b} (set (keys result))))))
      (testing "all-mode requires every tag"
        (let [result (access/get-tagged-palettes {:match :all} :warm :passionate)]
          (is (= #{:test/a} (set (keys result))))))
      (testing "threshold filters out weak matches"
        (is (= #{:test/a :test/b}
               (set (keys (access/get-tagged-palettes {:threshold 0.4} :warm)))))
        (is (= #{:test/a}
               (set (keys (access/get-tagged-palettes {:threshold 0.6} :warm))))))
      (testing "absent tags never match, regardless of threshold"
        (is (empty? (access/get-tagged-palettes :nonexistent))))
      (testing "all-mode with mixed presence rejects when any tag is missing"
        (is (empty? (access/get-tagged-palettes
                     {:match :all} :warm :nonexistent)))))))

(deftest family-getters
  (let [blues (access/get-blue-palettes)]
    (is (seq blues))
    (is (every? (fn [[_ p]] (= :blue (:family p))) blues))))

(deftest pool-filters
  (let [fake {:warm/a   {:family :red :category :warm :count 5
                         :attributes {:tags {:passionate 0.8}}}
              :warm/b   {:family :orange :category :warm :count 3
                         :attributes {:tags {:earthy 0.6}}}
              :cool/a   {:family :blue :category :cool :count 4
                         :attributes {:tags {:calm 0.9}}}}]
    (with-redefs [main/all-palettes (constantly fake)]
      (testing "family filter"
        (let [p (access/random-palette {:family :blue})]
          (is (= :blue (:family p)))))
      (testing "category filter"
        (let [hexes-only-warms (mapv :family
                                     (filter (fn [p] (= :warm (:category p)))
                                             (vals fake)))]
          (is (= #{:red :orange} (set hexes-only-warms)))))
      (testing "attr-tags filter with threshold"
        (let [p (access/random-palette {:attr-tags [:passionate]
                                        :attr-threshold 0.5})]
          (is (= :red (:family p))))) ;; :passionate above 0.5 only on :warm/a

      (testing "attr-tags filter with no threshold accepts any presence"
        (let [p (access/random-palette {:attr-tags [:earthy]})]
          (is (= :orange (:family p))))) ;; :earthy only on :warm/b

      (testing "min-count filter"
        (let [p (access/random-palette {:min-count 5})]
          (is (= :red (:family p)))))))) ;; count >= 5 only on :warm/a

;; Random selection (smoke only; values are nondeterministic) -------------------

(deftest random-smoke
  (testing "random-palette returns a [key palette] entry"
    (let [r (main/random-palette)]
      (is (some? r))
      (is (keyword? (key r)))
      (is (map? (val r)))))
  (testing "random-color picks a hex from a known palette"
    (is (string? (main/random-color :ocean/abyss))))
  (testing "random-color returns nil for unknown palettes"
    (is (nil? (main/random-color :no-such-palette)))))

;; Bundled data invariants ------------------------------------------------------

(deftest n-many-gradients
  (testing "palettes keep arbitrary color counts"
    (is (= 7 (:count (main/get-palette :neutral/ink))))
    (is (= 4 (:count (main/get-palette :synthwave/outrun))))
    (is (= 6 (:count (main/get-palette :ocean/abyss))))))

(deftest registry-keys-are-namespaced
  (testing "every palette key has a category namespace"
    (doseq [k (main/palette-keys)]
      (is (some? (namespace k))
          (str "missing category namespace: " k)))))

(deftest registry-rgba-matches-hex
  (testing "rgba and hex agree, every entry"
    (doseq [[k p] (main/all-palettes)]
      (is (= (count (:hex p)) (count (:rgba p)))
          (str "hex/rgba mismatch on " k))
      (is (= (mapv color/rgba->hex (:rgba p))
             (mapv str/upper-case (:hex p)))
          (str "hex/rgba content mismatch on " k)))))

;; Registry mutation ------------------------------------------------------------

(deftest registry-roundtrip
  (try
    (testing "register, look up, see :attributes"
      (main/register-palette! :test/scratch
                              ["#101010" "#808080" "#F0F0F0"])
      (let [p (main/get-palette :test/scratch)]
        (is (= 3 (:count p)))
        (is (= :test (:category p)))
        (is (= :scratch (:name p)))
        (is (map? (:attributes p)))
        (is (not (contains? p :tags)))))
    (finally
      (main/unregister-palette! :test/scratch)))
  (testing "removal sticks"
    (is (nil? (main/get-palette :test/scratch)))))

(deftest add-palette-ignores-tags-in-opts
  (testing ":tags in opts is silently dropped (no longer part of the model)"
    (let [reg (main/add-palette {} :probe/x ["#000000" "#FFFFFF"]
                                {:tags ["legacy"]})
          p   (get reg :probe/x)]
      (is (not (contains? p :tags))))))

;; Weights ----------------------------------------------------------------------

(deftest weights-default-and-roundtrip
  (testing "palette-weights returns even split when none stored"
    (is (= [0.25 0.25 0.25 0.25] (main/palette-weights :synthwave/outrun))))
  (testing "weights normalize on enrich"
    (let [reg (main/add-palette {} :probe/weighted ["#000000" "#FFFFFF"]
                                {:weights [3 1]})]
      (is (= [0.75 0.25] (:weights (get reg :probe/weighted))))))
  (testing "even palettes carry no :weights key"
    (is (nil? (:weights (main/get-palette :ocean/abyss))))
    (let [reg (main/add-palette {} :probe/even ["#000000" "#FFFFFF"])]
      (is (not (contains? (get reg :probe/even) :weights)))))
  (testing "mismatched weight count throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (main/add-palette {} :probe/bad ["#000000" "#FFFFFF"]
                                   {:weights [1 2 3]})))))

(deftest weighted-metadata-shifts
  (let [palette (fn [opts]
                  (get (main/add-palette {} :p/x ["#000000" "#FFFFFF"] opts) :p/x))
        dark    (palette {:weights [9 1]})
        even    (palette {})
        light   (palette {:weights [1 9]})]
    (testing "brightness follows prominence"
      (is (< (:brightness dark) (:brightness even) (:brightness light))))
    (testing "contrast stays the full range regardless of weights"
      (is (= (:contrast dark) (:contrast even) (:contrast light))))))

;; Persistence ------------------------------------------------------------------

(defn- temp-path [suffix]
  (let [f (java.io.File/createTempFile "clj-colors-test" suffix)]
    (.delete f)
    (.deleteOnExit f)
    (str f)))

(deftest rendered-file-shape
  (let [path (temp-path ".edn")
        reg  (-> {}
                 (main/add-palette :arctic/aa ["#101010" "#F0F0F0"])
                 (main/add-palette :ocean/bb ["#001122" "#AABBCC"]
                                   {:weights [0.7 0.3]}))]
    (main/save-registry! path reg)
    (let [out (slurp path)]
      (testing "category comments and grouping, sorted"
        (is (str/includes? out "; Arctic"))
        (is (str/includes? out "; Ocean"))
        (is (< (str/index-of out ":arctic/aa")
               (str/index-of out ":ocean/bb"))))
      (testing "one blank line between category groups"
        (is (str/includes? out "}\n\n ; Ocean")))
      (testing "weights are written only when given"
        (is (str/includes? out ":weights [0.7 0.3]"))
        (is (not (re-find #":arctic/aa[^}]*:weights" out))))
      (testing "no :tags key anywhere in the rendered file"
        (is (not (re-find #":tags" out))))
      (testing "no :attributes key either (computed, not stored)"
        (is (not (re-find #":attributes" out)))))
    (testing "the file round-trips through load-palettes"
      (let [loaded (main/load-palettes path)
            loaded-keys (set (keys loaded))]
        (is (contains? loaded-keys :arctic/aa))
        (is (contains? loaded-keys :ocean/bb))
        (is (= [0.7 0.3] (:weights (get loaded :ocean/bb))))
        (is (not (contains? (get loaded :arctic/aa) :weights)))
        (is (map? (:attributes (get loaded :ocean/bb))))))
    (testing "the index is generated beside the registry file"
      (let [idx (clojure.java.io/file (.getParent (clojure.java.io/file path))
                                      "swatch_index.md")]
        (is (.exists idx))
        (is (str/includes? (slurp idx) "## Arctic"))))
    (testing "removal is just saving a registry without the palette"
      (main/save-registry! path (main/remove-palette reg :ocean/bb))
      (let [out (slurp path)]
        (is (not (str/includes? out ":ocean/bb")))
        (is (not (str/includes? out "; Ocean")))
        (is (str/includes? out ":arctic/aa"))))))

(deftest index-generation
  (let [path (temp-path ".md")
        reg  (-> {}
                 (main/add-palette :arctic/aa ["#101010" "#F0F0F0"])
                 (main/add-palette :ocean/bb ["#001122" "#AABBCC"]))]
    (main/write-index! path reg)
    (let [out (slurp path)]
      (is (str/includes? out "# Swatch Index"))
      (is (str/includes? out "## Arctic"))
      (is (str/includes? out "- aa")))
    (testing "regeneration drops removed palettes"
      (main/write-index! path (main/remove-palette reg :ocean/bb))
      (let [out (slurp path)]
        (is (not (str/includes? out "## Ocean")))
        (is (str/includes? out "## Arctic"))))))

;; Oklab blending ---------------------------------------------------------------

(deftest oklab-roundtrip
  (doseq [h ["#FF0000" "#00FF00" "#0000FF" "#121810" "#A8DCEC"]]
    (is (= (color/hex->rgba h)
           (color/oklab->rgba (color/rgba->oklab (color/hex->rgba h))))
        h))
  (testing "white maps to L=1, a=b=0"
    (let [[L a b] (color/rgba->oklab [255 255 255 255])]
      (is (< 0.999 L 1.001))
      (is (< (Math/abs (double a)) 1e-6))
      (is (< (Math/abs (double b)) 1e-6)))))

(deftest perceptual-ramp
  (let [r (color/ramp ["#0000FF" "#FFFF00"] 5)]
    (testing "hex in means hex out, endpoints preserved, n stops"
      (is (= 5 (count r)))
      (is (= "#0000FF" (first r)))
      (is (= "#FFFF00" (last r))))
    (testing "the midpoint is not sRGB gray mud"
      (let [[mr mg mb] (color/hex->rgba (nth r 2))]
        (is (not= mr mg mb)))))
  (testing "oklch ramp hits its endpoints too"
    (let [r (color/ramp ["#C00000" "#0040A0"] 7 {:space :oklch})]
      (is (= 7 (count r)))
      (is (= "#C00000" (first r)))
      (is (= "#0040A0" (last r)))))
  (testing "weights shift the ramp toward the dominant color"
    (let [heavy (color/ramp [[0 0 0] [255 255 255]] 9 {:weights [9 1]})
          [r' _ _] (nth heavy 4)]
      (is (< r' 128))))
  (testing "unknown space throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (color/ramp ["#000000" "#FFFFFF"] 3 {:space :sideways})))))

(deftest color-tags-data-present
  (let [data @color-tags/data]
    (is (pos? (count data)))
    (is (every? string? (keys data)))
    (is (every? #(re-matches #"#[0-9a-f]{6}" %) (keys data)))))

(deftest associations-data-present
  (let [data @associations/data]
    (is (pos? (count data)))
    (testing "every association has a :colors map of hex → weight"
      (is (every? (fn [[_ v]] (map? (:colors v))) data))
      (doseq [[k v] data]
        (let [colors (:colors v)]
          (is (every? string? (keys colors))
              (str "non-string color key in " k))
          (is (every? #(re-matches #"#[0-9a-fA-F]{6}" %) (keys colors))
              (str "malformed hex in " k))
          (is (every? number? (vals colors))
              (str "non-numeric weight in " k))
          (is (every? #(<= 0.0 % 1.0) (vals colors))
              (str "weight out of [0,1] range in " k)))))))

(comment
  (run-tests))