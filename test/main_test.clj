(ns main-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing run-tests]]
            [clj-colors.access :as access]
            [clj-colors.color :as color]
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
    (testing "tags is a set including the family name"
      (is (set? (:tags p)))
      (is (contains? (:tags p) "green")))
    (testing "scalar metadata sits in 0-1"
      (is (<= 0.0 (:brightness p) 1.0))
      (is (<= 0.0 (:saturation p) 1.0))
      (is (<= 0.0 (:temperature p) 1.0))
      (is (<= 0.0 (:contrast p) 1.0)))))

(deftest rgba-only-palettes-load
  (testing "a stored palette carrying only :rgba enriches"
    (let [reg (main/enrich-registry
               {:probe/rgb-only {:rgba [[16 16 16] [240 240 240 255]]}})
          p   (get reg :probe/rgb-only)]
      (is (= ["#101010" "#F0F0F0"] (:hex p)))
      (is (= 2 (:count p))))))

;; Lookup -----------------------------------------------------------------------

(deftest bare-and-full-lookup-agree
  (is (= (main/get-palette :forest/jungle)
         (main/get-palette :jungle)))
  (is (nil? (main/get-palette :no-such-palette))))

(deftest category-grouping
  (is (contains? (set (main/categories)) :ocean))
  (let [ocean (main/palettes-in-category :ocean)]
    (is (every? (fn [[_ p]] (= :ocean (:category p))) ocean))))

;; Tag queries --------------------------------------------------------------------

(deftest tag-queries
  (let [retro (main/palettes-with-tags "retro" "80s")]
    (is (contains? retro :synthwave/synthwave))
    (is (every? (fn [[_ p]] (and (contains? (:tags p) "retro")
                                 (contains? (:tags p) "80s")))
                retro))))

(deftest relaxed-vs-strict-tags
  (let [strict  (main/palettes-with-tags "retro" "80s")
        relaxed (main/palettes-with-any-tags "retro" "80s")]
    (testing "relaxed is a superset of strict"
      (is (every? (set (keys relaxed)) (keys strict))))
    (testing "every relaxed result carries at least one of the tags"
      (is (every? (fn [[_ p]] (some (:tags p) ["retro" "80s"])) relaxed)))))

(deftest tagged-palette-front-door
  (testing "no tags means the whole registry in either match mode"
    (is (= (main/all-palettes) (access/get-tagged-palettes)))
    (is (= (main/all-palettes) (access/get-tagged-palettes {:match :all}))))
  (testing "the modes agree with the primitives"
    (is (= (main/palettes-with-tags "blue" "vivid")
           (access/get-tagged-palettes {:match :all} "blue" "vivid")))
    (is (= (main/palettes-with-any-tags "blue" "vivid")
           (access/get-tagged-palettes "blue" "vivid"))))
  (testing "an unknown match mode throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (access/get-tagged-palettes {:match :sideways} "blue")))))

(deftest family-getters
  (let [blues (access/get-blue-palettes)]
    (is (seq blues))
    (is (every? (fn [[_ p]] (= :blue (:family p))) blues))))

;; Random selection (smoke only; values are nondeterministic) ---------------------

(deftest random-smoke
  (is (some? (main/random-palette)))
  (is (string? (main/random-color :ocean/abyss)))
  (is (nil? (main/random-color :no-such-palette))))

;; Bundled data invariants ----------------------------------------------------------

(deftest n-many-gradients
  (testing "palettes keep arbitrary color counts"
    (is (= 7 (:count (main/get-palette :neutral/ink))))
    (is (= 4 (:count (main/get-palette :synthwave/outrun))))
    (is (= 6 (:count (main/get-palette :ocean/abyss))))))

;; Registry mutation ------------------------------------------------------------------

(deftest registry-roundtrip
  (try
    (testing "register then read back"
      (main/register-palette! :test/scratch
                              ["#101010" "#808080" "#F0F0F0"]
                              {:tags ["probe"]})
      (let [p (main/get-palette :test/scratch)]
        (is (= 3 (:count p)))
        (is (= :test (:category p)))
        (is (contains? (:tags p) "probe"))))
    (finally
      (main/unregister-palette! :test/scratch)))
  (testing "removal sticks"
    (is (nil? (main/get-palette :test/scratch)))))

;; Weights ----------------------------------------------------------------------

(deftest weights-default-and-roundtrip
  (testing "even distribution when none given"
    (is (= [0.25 0.25 0.25 0.25] (main/palette-weights :synthwave/outrun))))
  (testing "weights normalize on enrich"
    (let [reg (main/add-palette {} :probe/weighted ["#000000" "#FFFFFF"]
                                {:weights [3 1]})]
      (is (= [0.75 0.25] (:weights (get reg :probe/weighted))))))
  (testing "even palettes carry no :weights key"
    (is (nil? (:weights (main/get-palette :ocean/abyss)))))
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

;; Persistence ---------------------------------------------------------------------
;; The registry file is machine-owned: saving renders the whole registry.
;; These run against temp files, never the bundled resources/palettes.edn.

(defn- temp-path [suffix]
  (let [f (java.io.File/createTempFile "clj-colors-test" suffix)]
    (.delete f)
    (.deleteOnExit f)
    (str f)))

(deftest rendered-file-shape
  (let [path (temp-path ".edn")
        reg  (-> {}
                 (main/add-palette :arctic/aa ["#101010" "#F0F0F0"] {:tags ["one"]})
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
        (is (not (re-find #":arctic/aa[^}]*:weights" out)))))
    (testing "the file round-trips through load-palettes"
      (let [loaded (main/load-palettes path)]
        (is (= #{:arctic/aa :ocean/bb} (set (keys loaded))))
        (is (= [0.7 0.3] (:weights (get loaded :ocean/bb))))))
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

;; Oklab blending --------------------------------------------------------------

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

(comment
  (run-tests))