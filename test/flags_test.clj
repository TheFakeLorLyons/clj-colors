(ns flags-test
  "Benchmark eval of LLM-authored flag associations against known
   reference colors. Run after batch-author!'ing the :flag namespace
   to score how well the proposals match real flag designs.

   Reference colors are official Pantone/RGB specs where possible;
   verify against authoritative sources before adding new entries.
   Each entry should cite a :source so you can re-verify later.

   Run all tests: (clojure.test/run-tests 'flags-test)
   Score breakdown:  (score-all)
   Per-flag detail:  (score-flag :flag/japan)"
  (:require [clj-colors.llm.batch :as batch]
            [benchmark :as bench]
            [clj-colors.color :as color]
            [clj-colors.associations :as associations]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing run-tests]]))

(def reference
  "Reference colors per flag, with tolerance for OKLAB-distance
   matching. Tiers indicate expected difficulty: simple two-or-three-
   color designs (tier-1), distinctive multi-color (tier-2), or
   culturally specific / unusual (tier-3).

   IMPORTANT: hex values here are approximations of well-known specs.
   Before using as a benchmark, verify each against the flag's
   official documentation. The :source field records where each
   value was confirmed."
  {;; ─── TIER 1: well-known simple designs ────────────────────────
   :flag/japan
   {:expected-colors {"#bc002d" 0.55
                      "#ffffff" 0.45}
    :tolerance 0.04
    :tier :tier-1
    :notes "Hinomaru. Red is the identity even though white covers more area.
            Red specified as Pantone 186C."
    :source "https://encycolorpedia.com/flags/japan"}

   :flag/indonesia
   {:expected-colors {"#ff0000" 0.5
                      "#ffffff" 0.5}
    :tolerance 0.05
    :tier :tier-1
    :notes "Red and white horizontal bands."
    :source "https://encycolorpedia.com/flags/indonesia"}

   :flag/france
   {:expected-colors {"#002395" 0.33
                      "#ffffff" 0.34
                      "#ed2939" 0.33}
    :tolerance 0.05
    :tier :tier-1
    :notes "Tricolore. Blue and red darkened in 2020."
    :source "https://encycolorpedia.com/flags/france"}

   :flag/italy
   {:expected-colors {"#009246" 0.33
                      "#ffffff" 0.34
                      "#ce2b37" 0.33}
    :tolerance 0.05
    :tier :tier-1
    :notes "Italian tricolour. Specific UNI specifications."
    :source "https://encycolorpedia.com/flags/italy"}

   :flag/ireland
   {:expected-colors {"#169b62" 0.33
                      "#ffffff" 0.34
                      "#ff883e" 0.33}
    :tolerance 0.05
    :tier :tier-1
    :notes "Green-white-orange. Orange not red."
    :source "https://encycolorpedia.com/flags/ireland"}

   :flag/belgium
   {:expected-colors {"#000000" 0.33
                      "#fdda24" 0.34
                      "#ed2939" 0.33}
    :tolerance 0.05
    :tier :tier-1
    :notes "Vertical tricolour, equal thirds."
    :source "https://encycolorpedia.com/flags/belgium"}

   :flag/germany
   {:expected-colors {"#000000" 0.33
                      "#dd0000" 0.34
                      "#ffce00" 0.33}
    :tolerance 0.05
    :tier :tier-1
    :notes "Black-red-gold equal horizontal thirds."
    :source "https://encycolorpedia.com/flags/germany"}

   :flag/netherlands
   {:expected-colors {"#ae1c28" 0.33
                      "#ffffff" 0.34
                      "#21468b" 0.33}
    :tolerance 0.05
    :tier :tier-1
    :notes "Red-white-blue equal horizontal thirds."
    :source "https://encycolorpedia.com/flags/netherlands"}

   :flag/russia
   {:expected-colors {"#ffffff" 0.33
                      "#0039a6" 0.34
                      "#d52b1e" 0.33}
    :tolerance 0.05
    :tier :tier-1
    :notes "White-blue-red equal horizontal thirds."
    :source "https://encycolorpedia.com/flags/russia"}

   :flag/poland
   {:expected-colors {"#ffffff" 0.5
                      "#dc143c" 0.5}
    :tolerance 0.05
    :tier :tier-1
    :notes "White over crimson, equal halves."
    :source "https://encycolorpedia.com/flags/poland"}

   ;; ─── TIER 2: distinctive multi-color designs ──────────────────
   :flag/brazil
   {:expected-colors {"#009b3a" 0.55
                      "#fedf00" 0.25
                      "#002776" 0.15
                      "#ffffff" 0.05}
    :tolerance 0.06
    :tier :tier-2
    :notes "Green field, yellow diamond, blue celestial sphere, white stars and ribbon.
            Green field dominates; yellow diamond and blue sphere are the focal hierarchy."
    :source "https://encycolorpedia.com/flags/brazil"}

   :flag/mexico
   {:expected-colors  {"#006847" 0.30
                       "#ffffff" 0.30
                       "#ce1126" 0.30

                       ;; eagle browns
                       "#8b441f" 0.010
                       "#803f1d" 0.006
                       "#904720" 0.004
                       "#8f4620" 0.004
                       "#874f20" 0.003
                       "#8b5122" 0.003
                       "#953220" 0.002
                       "#6c3f18" 0.002
                       "#5c3818" 0.002
                       "#5c3a1d" 0.001
                       "#6c4119" 0.001
                       "#4d2a15" 0.001
                       "#513625" 0.001
                       "#312317" 0.001
                       "#45392d" 0.001
                       "#4b4139" 0.001

                       ;; golds and yellows
                       "#fcca3d" 0.004
                       "#fcca3e" 0.003
                       "#f1a720" 0.003
                       "#f9c83a" 0.002
                       "#f8c83c" 0.002
                       "#f7e204" 0.002
                       "#d2a567" 0.002
                       "#dbad6c" 0.001
                       "#f9aa51" 0.001
                       "#bf802d" 0.001
                       "#ab6d29" 0.001
                       "#b27129" 0.001
                       "#af7029" 0.001
                       "#b07229" 0.001
                       "#fdeaaf" 0.001
                       "#fcf3d8" 0.001

                       ;; olive / cactus / wreath
                       "#404118" 0.003
                       "#717732" 0.002
                       "#78732e" 0.002
                       "#6f5b24" 0.001
                       "#816c2a" 0.001
                       "#977c2e" 0.001
                       "#aa8c30" 0.001
                       "#a8ac71" 0.001
                       "#9ca168" 0.001
                       "#c6c7a6" 0.001
                       "#04534e" 0.001
                       "#016848" 0.001

                       ;; dark outlines
                       "#171717" 0.002
                       "#202020" 0.001
                       "#202220" 0.001
                       "#231f20" 0.001
                       "#1e2121" 0.001
                       "#1c242f" 0.001
                       "#000000" 0.001

                       ;; reds inside emblem
                       "#cd202a" 0.001
                       "#e92736" 0.001
                       "#f15770" 0.0005

                       ;; blue accents
                       "#0872a7" 0.001
                       "#0c8489" 0.001
                       "#30c2dc" 0.0005
                       "#8cbebf" 0.0005

                       ;; pale neutrals
                       "#d5d3ca" 0.0005}
    :tolerance 0.05
    :tier :tier-2
    :notes "Tricolour bands (90% of weight) plus coat of arms detail (10%). 
            The challenge is whether the LLM proposes a simple GWR palette or attempts the eagle."
    :source "https://encycolorpedia.com/flags/mexico"}

   :flag/south-africa
   {:expected-colors {"#007a4d" 0.25
                      "#ffffff" 0.15
                      "#000000" 0.15
                      "#ffb612" 0.10
                      "#de3831" 0.20
                      "#002395" 0.15}
    :tolerance 0.06
    :tier :tier-2
    :notes "Six colors. Y-shaped pall design."
    :source "https://encycolorpedia.com/flags/south-africa"}

   :flag/india
   {:expected-colors {"#ff9933" 0.33
                      "#ffffff" 0.33
                      "#128807" 0.33
                      "#000080" 0.01}
    :tolerance 0.06
    :tier :tier-2
    :notes "Saffron-white-green tricolour, navy chakra."
    :source "https://encycolorpedia.com/flags/india"}

   :flag/south-korea
   {:expected-colors {"#ffffff" 0.55
                      "#cd2e3a" 0.18
                      "#0047a0" 0.18
                      "#000000" 0.09}
    :tolerance 0.06
    :tier :tier-2
    :notes "Taegukgi. White field, central red-blue taegeuk, four black trigrams"
    :source "https://encycolorpedia.com/flags/korea-south"}

   :flag/vietnam
   {:expected-colors {"#da251d" 0.85
                      "#ffff00" 0.15}
    :tolerance 0.05
    :tier :tier-2
    :notes "Red field with yellow star."
    :source "https://encycolorpedia.com/flags/vietnam"}

   :flag/canada
   {:expected-colors {"#ff0000" 0.55
                      "#ffffff" 0.45}
    :tolerance 0.05
    :tier :tier-2
    :notes "Red-white-red with maple leaf."
    :source "https://encycolorpedia.com/flags/canada"}

   :flag/switzerland
   {:expected-colors {"#ff0000" 0.7
                      "#ffffff" 0.3}
    :tolerance 0.05
    :tier :tier-2
    :notes "Square red flag with white cross."
    :source "https://encycolorpedia.com/flags/switzerland"}

   :flag/ethiopia
   {:expected-colors {"#078930" 0.33
                      "#fcdd09" 0.33
                      "#da121a" 0.33
                      "#0f47af" 0.01}
    :tolerance 0.06
    :tier :tier-2
    :notes "Pan-African green-yellow-red with central blue disc."
    :source "https://encycolorpedia.com/flags/ethiopia"}

   :flag/jamaica
   {:expected-colors {"#000000" 0.25
                      "#009b3a" 0.5
                      "#fed100" 0.25}
    :tolerance 0.05
    :tier :tier-2
    :notes "Black-green-gold with diagonal cross. No red or white."
    :source "https://encycolorpedia.com/flags/jamaica"}

   ;; ─── TIER 3: culturally specific or visually unusual ──────────
   :flag/nepal
   {:expected-colors {"#dc143c" 0.55
                      "#003893" 0.30
                      "#ffffff" 0.15}
    :tolerance 0.07
    :tier :tier-3
    :notes "Only non-rectangular national flag. Two pennants stacked with crimson body.."
    :source "https://encycolorpedia.com/flags/nepal"}

   :flag/mozambique
   {:expected-colors {"#007168" 0.30
                      "#000000" 0.15
                      "#fce100" 0.30
                      "#ffffff" 0.05
                      "#d21034" 0.20}
    :tolerance 0.07
    :tier :tier-3
    :notes "Green-black-yellow horizontal, red triangle with AK-47."
    :source "https://encycolorpedia.com/flags/mozambique"}

   :flag/saudi-arabia
   {:expected-colors {"#006c35" 0.85
                      "#ffffff" 0.15}
    :tolerance 0.06
    :tier :tier-3
    :notes "Green with Shahada and sword in white."
    :source "https://encycolorpedia.com/flags/saudi-arabia"}

   :flag/kiribati
   {:expected-colors {"#003f87" 0.35
                      "#ce1126" 0.30
                      "#fcd116" 0.18
                      "#ffffff" 0.12
                      "#bd9c08" 0.05}
    :tolerance 0.07
    :tier :tier-3
    :notes "Red sky upper half, blue ocean lower half, white waves, yellow sun and frigatebird."
    :source "https://encycolorpedia.com/flags/kiribati"}

   :flag/belize
   {:expected-colors {"#003f87" 0.55

                      "#ce1126" 0.12
                      "#ff0018" 0.03
                      "#782121" 0.01
                      "#730000" 0.01

                      "#ffffff" 0.15

                       ;; browns / wood / tools
                      "#9b5f00" 0.024
                      "#952d1a" 0.008
                      "#b34b00" 0.006
                      "#570a00" 0.003
                      "#552300" 0.003

                       ;; gold / yellow
                      "#ffd800" 0.018
                      "#ffd83c" 0.008
                      "#ffef5d" 0.004
                      "#ffa54b" 0.002
                      "#ffb366" 0.002

                       ;; foliage
                      "#005800" 0.016
                      "#006600" 0.007
                      "#007f00" 0.005
                      "#289400" 0.003
                      "#5ac800" 0.002
                      "#004b00" 0.002
                      "#003300" 0.002
                      "#66ff66" 0.001

                       ;; blue details
                      "#006ac8" 0.003
                      "#9dd7ff" 0.002
                      "#6699ff" 0.002

                       ;; dark outlines
                      "#000000" 0.005

                       ;; miscellaneous detail colors
                      "#7e4b7e" 0.001
                      "#ccb8c8" 0.001}
    :tolerance 0.07
    :tier :tier-3
    :notes "Blue field dominates. Red bands and white disc are major elements. 
            Coat of arms contributes roughly 10% of total score and is weighted by visual prominence."
    :source "https://encycolorpedia.com/flags/belize"}

   :flag/bhutan
   {:expected-colors {"#ffd520" 0.43
                      "#ff4e12" 0.43
                      "#ffffff" 0.11
                      "#000000" 0.03}
    :tolerance 0.07
    :tier :tier-3
    :notes  "Yellow and orange fields dominate. White dragon contributes substantial detail, 
             with black outlines and claws carrying a small weight."
    :source "https://encycolorpedia.com/flags/bhutan"}

   :flag/botswana
   {:expected-colors {"#75aadb" 0.6
                      "#000000" 0.25
                      "#ffffff" 0.15}
    :tolerance 0.07
    :tier :tier-3
    :notes "Light blue field with black-white-black horizontal."
    :source "https://encycolorpedia.com/flags/botswana"}

   :flag/trinidad-tobago
   {:expected-colors {"#da1a35" 0.7
                      "#000000" 0.20
                      "#ffffff" 0.10}
    :tolerance 0.06
    :tier :tier-3
    :notes "Red field with diagonal black band edged in white."
    :source "https://encycolorpedia.com/flags/trinidad-and-tobago"}
   
   :flag/saint-vincent-grenadines
   {:expected-colors {"#0072c6" 0.33
                      "#fcd116" 0.34
                      "#009e60" 0.33}
    :tolerance 0.07
    :tier :tier-3
    :notes "Blue-yellow-green vertical with three green diamonds."
    :source "https://encycolorpedia.com/flags/saint-vincent-and-the-grenadines"}

   :flag/north-macedonia
   {:expected-colors {"#d20000" 0.40
                      "#ffe600" 0.60}
    :tolerance 0.06
    :tier :tier-3
    :notes "Red field with yellow sun and eight rays."
    :source "https://encycolorpedia.com/flags/macedonia"}})

;; ── Scoring ────────────────────────────────────────────────────────

(defn- oklab-distance
  "OKLAB distance between two hex strings."
  [hex-a hex-b]
  (let [[la aa ba _] (color/rgba->oklab (color/hex->rgba hex-a))
        [lb ab bb _] (color/rgba->oklab (color/hex->rgba hex-b))
        dl (- la lb) da (- aa ab) db (- ba bb)]
    (Math/sqrt (+ (* dl dl) (* da da) (* db db)))))

(defn- nearest-actual
  "For each expected hex, find the actual hex in the proposed colors
   closest in OKLAB space, with that distance."
  [expected actuals]
  (for [exp expected
        :let [matches (mapv (fn [act] [act (oklab-distance exp act)]) actuals)
              best (apply min-key second matches)]]
    {:expected exp
     :nearest-actual (first best)
     :distance (second best)}))

(defn- weighted-score
  [expected-map actuals tolerance]
  (let [expected-colors (keys expected-map)
        ;; actuals is now {hex weight}
        actual-colors (keys actuals)
        expected-matches
        (for [exp expected-colors
              :let [pairs (mapv (fn [act] [act (oklab-distance exp act)]) actual-colors)
                    best  (when (seq pairs) (apply min-key second pairs))]]
          {:expected exp
           :weight (get expected-map exp)
           :nearest-actual (first best)
           :distance (second best)
           :passes? (when best (<= (second best) tolerance))})

        actual-matches
        (for [[act act-w] actuals
              :let [pairs (mapv (fn [exp] [exp (oklab-distance act exp)]) expected-colors)
                    best  (apply min-key second pairs)]]
          {:actual act
           :actual-weight act-w
           :nearest-expected (first best)
           :distance (second best)
           :matches? (<= (second best) tolerance)})

        passing-weight (reduce + 0.0
                               (map :weight (filter :passes? expected-matches)))
        total-weight (reduce + 0.0 (map :weight expected-matches))
        coverage (if (pos? total-weight)
                   (/ passing-weight total-weight)
                   0.0)
        ;; NEW: weighted hallucination rate
        hallucinated-weight (reduce + 0.0
                                    (map :actual-weight
                                         (remove :matches? actual-matches)))
        total-actual-weight (reduce + 0.0 (map :actual-weight actual-matches))
        hallucination-rate (if (pos? total-actual-weight)
                             (/ hallucinated-weight total-actual-weight)
                             0.0)
        ;; NEW: salience error — for matched pairs, how far off was
        ;; the LLM's weight from the reference's weight?
        salience-error
        (let [matched (filter :passes? expected-matches)]
          (if (seq matched)
            (/ (reduce + 0.0
                       (map (fn [m]
                              (let [exp-w (:weight m)
                                    act-w (get actuals (:nearest-actual m) 0.0)]
                                (Math/abs (- exp-w act-w))))
                            matched))
               (count matched))
            0.0))]
    {:coverage coverage
     :hallucination-rate hallucination-rate
     :salience-error salience-error
     :n-expected (count expected-matches)
     :n-passing (count (filter :passes? expected-matches))
     :n-actual (count actuals)
     :n-hallucinated (count (remove :matches? actual-matches))
     :matches expected-matches
     :hallucinations (filter (complement :matches?) actual-matches)}))

;; ── Convenience wrappers ──────────────────────────────────────────

(defn score-flag
  "Per-flag detail."
  [k]
  (bench/score-spec reference @associations/data k))

(defn score-summary
  "Aggregate breakdown across all flags."
  []
  (bench/summary (bench/score-all reference @associations/data)))

(defn worst-failures
  ([] (worst-failures 10))
  ([n] (bench/worst-failures
        (bench/score-all reference @associations/data)
        n)))

;; ── Tests ─────────────────────────────────────────────────────────

(deftest reference-data-wellformed
  (testing "every reference has the required fields and weights sum to ~1"
    (doseq [[k entry] reference]
      (is (map? (:expected-colors entry)))
      (is (number? (:tolerance entry)))
      (is (keyword? (:tier entry)))
      (is (string? (:source entry)))
      (let [weight-sum (reduce + 0.0 (vals (:expected-colors entry)))]
        (is (and (>= weight-sum 0.99) (<= weight-sum 1.01))
            (format "%s weights sum to %.3f" k weight-sum))))))

(deftest reference-tiers-balanced
  (testing "reference set has roughly 10 flags per tier"
    (let [by-tier (group-by (comp :tier val) reference)]
      (doseq [tier [:tier-1 :tier-2 :tier-3]]
        (is (>= (count (get by-tier tier)) 8))))))

(deftest tier-1-flags-pass
  (testing "simple flag designs should mostly pass"
    (let [scores (->> reference
                      (filter #(= :tier-1 (:tier (val %))))
                      keys
                      (mapv score-flag)
                      (remove #(= :no-proposal (:status %))))
          n-scored (count scores)
          n-passing (count (filter #(= :pass (:status %)) scores))]
      (when (pos? n-scored)
        (is (>= (/ n-passing n-scored) 0.7)
            (format "Tier 1 pass rate %d/%d below 70%%" n-passing n-scored))))))

(deftest tier-2-flags-mostly-pass
  (testing "distinctive multi-color flags should mostly pass"
    (let [scores (->> reference
                      (filter #(= :tier-2 (:tier (val %))))
                      keys
                      (mapv score-flag)
                      (remove #(= :no-proposal (:status %))))
          n-scored (count scores)
          n-passing (count (filter #(= :pass (:status %)) scores))]
      (when (pos? n-scored)
        (is (>= (/ n-passing n-scored) 0.5)
            (format "Tier 2 pass rate %d/%d below 50%%" n-passing n-scored))))))

(comment
  ;; Workflow:

  ;; 1. Author all 30 flags via batch
  (batch/batch-author! {:type :association
                        :namespace :flag
                        :referents (mapv (comp name first) reference)
                        :review-mode :auto})

  (batch/batch-author!
   {:type :association
    :namespace :flag
    :referents (mapv (comp name first) reference)
    :review-mode :auto
    :skip-existing true}) ; skips existing

  (binding [clj-colors.llm.associative/*model* "claude-sonnet-4-6"]
    (batch/batch-author!
     {:type :association
      :namespace :flag
      :referents (mapv (comp name first) flags-test/reference)
      :review-mode :auto
      :skip-existing true
      :checkpoint-usd nil}))

  ;; 2. Score the results
  (score-summary)
  ;; => {:total 30 :passing 22 :partial 6 :failing 2 :missing 0
  ;;     :by-tier {:tier-1 {:total 10 :passing 9 :partial 1 ...}
  ;;               :tier-2 {:total 10 :passing 8 :partial 2 ...}
  ;;               :tier-3 {:total 10 :passing 5 :partial 3 ...}}}

  ;; 3. Run tests
  (run-tests)

  ;; 4. Drill into failures
  (worst-failures 5)

  ;; 5. Modify specific flags via batch-modify!
  (batch/batch-modify!
   {:type :association
    :modifications
    {:flag/india "the saffron should be more orange, less yellow"
     :flag/nepal "the crimson should be deeper, less pink"}
    :review-mode :interactive
    :budget-usd 0.2})

  ;; 6. Re-score
  (score-summary)


  ;; Load current authored entries
  (let [current (clj-colors.authoring/load-authored)
        kept    (into {} (remove (fn [[k _]] (= "flag" (namespace k))) current))]
    (clj-colors.authoring/save-authored! kept)
    (clj-colors.associations/refresh!)
    (println (format "Removed %d flag entries; %d remain."
                     (- (count current) (count kept))
                     (count kept))))


  (binding [clj-colors.llm.associative/*model* "claude-sonnet-4-6"]
    (clj-colors.llm.batch/batch-author!
     {:type :association
      :namespace :flag
      :referents (mapv (comp name first) flags-test/reference)
      :review-mode :auto
      :skip-existing true
      :checkpoint-usd nil}))
  )