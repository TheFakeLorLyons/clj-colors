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

;; Surgical persistence ----------------------------------------------------------------
;; These run against a temp file, never the bundled resources/palettes.edn.

(defn- temp-edn-path []
  (let [f (java.io.File/createTempFile "clj-colors-test" ".edn")]
    (.delete f)
    (.deleteOnExit f)
    (str f)))

(deftest surgical-persistence
  (let [path (temp-edn-path)
        reg  (-> {}
                 (main/add-palette :arctic/aa ["#101010" "#F0F0F0"] {:tags ["one"]})
                 (main/add-palette :ocean/bb  ["#001122" "#AABBCC"] {:tags ["two"]}))]

    (testing "a fresh path gets the full rendering"
      (main/save-registry! path reg)
      (let [out (slurp path)]
        (is (str/includes? out ":arctic/aa"))
        (is (str/includes? out ";; arctic"))))

    (testing "hand-written comments survive a sync that adds a palette"
      (spit path (str ";; hand index, do not touch\n" (slurp path)))
      (let [reg (main/add-palette reg :arctic/cc ["#202020" "#E0E0E0"]
                                  {:tags ["three"]})]
        (main/save-registry! path reg)
        (let [out (slurp path)]
          (is (str/includes? out ";; hand index, do not touch"))
          (is (str/includes? out ":arctic/cc"))
          (testing "and the new entry lands inside its category block"
            (is (< (str/index-of out ":arctic/cc")
                   (str/index-of out ":ocean/bb")))))))

    (testing "a changed palette is replaced in place; others untouched"
      (let [reg (-> reg
                    (main/add-palette :arctic/cc ["#202020" "#E0E0E0"]
                                      {:tags ["three"]})
                    (main/add-palette :ocean/bb ["#334455" "#CCDDEE"]
                                      {:tags ["two"]}))]
        (main/save-registry! path reg)
        (let [out (slurp path)]
          (is (str/includes? out "#334455"))
          (is (not (str/includes? out "#001122")))
          (is (str/includes? out "#101010"))
          (is (str/includes? out ";; hand index, do not touch")))))

    (testing "the synced file loads back as a registry"
      (is (= #{:arctic/aa :arctic/cc :ocean/bb}
             (set (keys (main/load-palettes path))))))

    (testing "removal deletes exactly the entry's lines"
      (main/remove-palette-from-file! path :arctic/cc)
      (let [out (slurp path)]
        (is (not (str/includes? out ":arctic/cc")))
        (is (str/includes? out ":arctic/aa"))
        (is (str/includes? out ";; hand index, do not touch"))))

    (testing "removing the last of a category takes its header along"
      (main/remove-palette-from-file! path :ocean/bb)
      (let [out (slurp path)]
        (is (not (str/includes? out ":ocean/bb")))
        (is (not (str/includes? out ";; ocean")))
        (is (str/includes? out ";; arctic"))))

    (testing "removing a key not in the file is a quiet no-op"
      (let [before (slurp path)]
        (is (nil? (main/remove-palette-from-file! path :never/was)))
        (is (= before (slurp path)))))))

(comment
  (run-tests))