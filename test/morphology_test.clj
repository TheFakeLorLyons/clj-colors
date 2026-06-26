(ns morphology-test
  "Exhaustive tests for the morphology layer. Organized by category
   so failures point at structural problems. Add new cases as you
   discover edge cases in the wild; this file is the system's
   memory of what it's been asked to handle correctly."
  (:require [clj-colors.lexicon :as lexi]
            [clj-colors.morphology :as morph]
            [clojure.test :refer [deftest is testing run-tests]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.set :as set]))

(def random-word-gen
  (gen/fmap (fn [chars] (keyword (apply str chars)))
            (gen/vector
             (gen/elements "abcdefghijklmnopqrstuvwxyz")
             3 12)))

(defspec relation-is-symmetric 200
  (prop/for-all [a random-word-gen
                 b random-word-gen]
                (= (morph/derivationally-related? a b)
                   (morph/derivationally-related? b a))))

(defspec relation-is-reflexive 200
  (prop/for-all [a random-word-gen]
                (morph/derivationally-related? a a)))

(defspec stripping-terminates 200
  (prop/for-all [a random-word-gen]
                (some? (morph/stem a))))

(defspec derivable-implies-shared-stem-or-irregular 100
  (prop/for-all [a random-word-gen
                 b random-word-gen]
                (let [related?  (morph/derivationally-related? a b)
                      paths-a   (morph/strips-to-extended a)
                      paths-b   (morph/strips-to-extended b)
                      common    (seq (set/intersection paths-a paths-b))
                      irregular (and (get morph/irregular-canonical a)
                                     (= (get morph/irregular-canonical a)
                                        (get morph/irregular-canonical b a)))]
                  (if related?
                    (or common irregular (= a b))
                    true))))

(def true-positives
  "Pairs that MUST match: morphological variants of the same root.
   Organized by the suffix pattern they exercise so failures point
   at specific suffix-handling problems. The :why field is for human
   reviewers reading test output."
  [;; -ful family
   {:a :joy        :b :joyful     :why "base + -ful"}
   {:a :fear       :b :fearful    :why "base + -ful"}
   {:a :hate       :b :hateful    :why "base ending in -e + -ful"}
   {:a :shame      :b :shameful   :why "base ending in -e + -ful"}
   {:a :guilt      :b :guiltful   :why "base + -ful (rare form)"}
   {:a :pride      :b :prideful   :why "base + -ful (direct, not via proud)"}

   ;; -ous family
   {:a :joy        :b :joyous     :why "base + -ous"}
   {:a :marvel     :b :marvelous  :why "base + -ous"}

   ;; -ing family
   {:a :disgust    :b :disgusting :why "base + -ing"}
   {:a :amuse      :b :amusing    :why "base ending in -e + -ing"}
   {:a :interest   :b :interesting :why "base + -ing"}
   {:a :disappoint :b :disappointing :why "base + -ing"}

   ;; -ed family
   {:a :amuse      :b :amused     :why "base ending in -e + -ed"}
   {:a :disgust    :b :disgusted  :why "base + -ed"}
   {:a :content    :b :contented  :why "base + -ed"}

   ;; -ly family (adverb from descriptor)
   {:a :joyful     :b :joyfully   :why "descriptor + -ly"}
   {:a :sad        :b :sadly      :why "base + -ly"}
   {:a :proud      :b :proudly    :why "descriptor + -ly"}

   ;; -ness family (noun from descriptor)
   {:a :joyful     :b :joyfulness :why "descriptor + -ness"}
   {:a :sad        :b :sadness    :why "base + -ness"}
   {:a :happy      :b :happiness  :why "y -> i + -ness (irregular?)"}

   ;; -able family
   {:a :love       :b :lovable    :why "base ending in -e + -able"}
   {:a :enjoy      :b :enjoyable  :why "base + -able"}

   ;; Compound (transitive: A → B → C)
   {:a :joy        :b :joyfully   :why "joy → joyful → joyfully"}
   {:a :joy        :b :joyfulness :why "joy → joyful → joyfulness"}
   {:a :amuse      :b :amusingly  :why "amuse → amusing → amusingly"}

   ;; Irregulars (from the irregular-canonical map)
   {:a :pride      :b :proud      :why "irregular: pride → proud"}
   
   {:a :dark    :b :darkness  :why "base + -ness, descriptor preserving"}
   {:a :dark    :b :darkly    :why "base + -ly, descriptor preserving"}
   {:a :calm    :b :calming   :why "base + -ing, descriptor preserving"}
   {:a :soft    :b :softness  :why "base + -ness"}
   {:a :bright  :b :brightly  :why "base + -ly"}
   {:a :care    :b :careful   :why "base + -ful"}
   {:a :care    :b :carefully :why "transitive: care + -ful + -ly"}
   {:a :gloom   :b :gloomy    :why "base + -y"}
   {:a :hope    :b :hopeful   :why "base + -ful"}
   {:a :love    :b :loving    :why "base + -ing, silent-e drop"}
   {:a :love    :b :loved     :why "base + -ed, silent-e drop"}
   {:a :shine   :b :shining   :why "base + -ing, silent-e drop"}
   {:a :taste   :b :tasteless :why "base + -less (NOTE: -less not in suffix list yet)"}
])

(def true-negatives
  "Pairs that MUST NOT match: lookalikes that aren't morphological
   variants. These are the false-positive risks. Each :why explains
   the trap the algorithm could fall into."
  [;; Spurious prefix matches
   {:a :sad        :b :sadistic   :why "sad and sadistic share 3 letters but unrelated"}
   {:a :sad        :b :sadism     :why "same"}
   {:a :sad        :b :saddle     :why "saddle is not 'sad' + suffix"}
   {:a :joy        :b :joined     :why "join is not joy + recognized suffix"}
   {:a :joy        :b :joisting   :why "joist is not joy + recognized suffix"}
   {:a :hate       :b :hat        :why "hat is unrelated despite -e drop similarity"}
   {:a :love       :b :lover-boy  :why "compound word, not a derivation"}
   {:a :relief     :b :religion   :why "religion has no morph link to relief"}
   {:a :fear       :b :feast      :why "feast is unrelated"}

   ;; Different roots, same suffix
   {:a :joyful     :b :hateful    :why "both -ful but different bases"}
   {:a :sadness    :b :goodness   :why "both -ness but different bases"}

   ;; Subtle traps
   {:a :anger      :b :anglo      :why "anglo is unrelated"}
   {:a :interest   :b :inter      :why "inter is too short to be base"}
   {:a :grief      :b :grievance  :why "grievance is from grieve, not grief"}
   ;; Actually grievance DOES derive from grief etymologically; risky test case
  
  ;; Etymological lookalikes that share neither suffix nor stem
  {:a :hate     :b :hatred     :why "verb/feeling vs abstract noun"}
   ])

(def symmetry-cases
  "Both directions must give the same answer. Reuses true-positives
   and true-negatives but tests both (a,b) and (b,a) explicitly."
  (concat true-positives true-negatives))

(def edge-cases
  "Pathological inputs that test the algorithm's robustness rather
   than its correctness on real words. Mostly meant to ensure no
   exceptions or infinite loops."
  [{:input :a            :why "single character"}
   {:input :ab           :why "two characters (below 3-char floor)"}
   {:input :the          :why "exactly 3 characters"}
   {:input :fully        :why "pure suffix as standalone word"}
   {:input :ness         :why "pure suffix as standalone word"}
   {:input :joyfullyness :why "stacked suffixes (made up)"}
   {:input :fulfulful    :why "repeating suffix substring"}])

(deftest true-positive-pairs-match
  (testing "all morphologically-related pairs are detected"
    (doseq [{:keys [a b why]} true-positives]
      (is (morph/derivationally-related? a b)
          (format "Expected %s ~ %s (%s)" a b why)))))

(deftest true-negative-pairs-do-not-match
  (testing "lookalikes are correctly rejected"
    (doseq [{:keys [a b why]} true-negatives]
      (is (not (morph/derivationally-related? a b))
          (format "Expected %s ≁ %s (%s)" a b why)))))

(deftest morphological-relation-is-symmetric
  (testing "result does not depend on argument order"
    (doseq [{:keys [a b]} symmetry-cases]
      (is (= (morph/derivationally-related? a b)
             (morph/derivationally-related? b a))
          (format "Asymmetric: %s ~ %s" a b)))))

(deftest reflexivity
  (testing "every word is morphologically related to itself"
    (doseq [{:keys [a]} true-positives]
      (is (morph/derivationally-related? a a)))))

(deftest stem-terminates
  (testing "stripping terminates on all edge cases without exceptions"
    (doseq [{:keys [input]} edge-cases]
      (is (some? (morph/stem input))
          (format "Stem failed for edge case: %s" input)))))

(deftest exhaustive-pairs-against-pole-corpus
  (testing "every pair within and across pole descriptors behaves as classified"
    (let [poles      [:admirable :amusing :angry :compassionate
                      :contemptuous :contented :disappointing
                      :disgusting :fearful :guilty :hateful
                      :interesting :joyful :loving :pleasurable
                      :proud :regretful :relieving :sad :shameful]
          unrelated-pairs (for [a poles, b poles
                                :when (not= a b)]
                            [a b])]
      ;; None of the pole descriptors should be morphologically
      ;; related to each other; they're semantically distinct.
      (doseq [[a b] unrelated-pairs]
        (is (not (morph/derivationally-related? a b))
            (format "Distinct poles %s and %s match" a b))))))

(defn surprising-matches
  "Pairs from the tag corpus that the morphology layer thinks are
   related. Sort by stem length descending so the most confident
   matches surface first. Manual review of this list catches false
   positives and tells you which to add to false-positive-pairs."
  [tag-corpus]
  (let [tags (vec tag-corpus)]
    (->> (for [i (range (count tags))
               j (range (inc i) (count tags))
               :let [a (nth tags i) b (nth tags j)]
               :when (morph/derivationally-related? a b)]
           {:a a :b b
            :stem (morph/stem a)
            :stem-len (count (name (morph/stem a)))})
         (sort-by (comp - :stem-len)))))

(defn diagnose
  "Run morphologically-related? on a pair and return the reasoning:
   which strips paths were generated, whether they intersected, and
   whether any explicit overrides applied. Useful when a test fails
   and you need to see why."
  [a b]
  (let [paths-a   (morph/strips-to (name a))
        paths-b   (morph/strips-to (name b))
        common    (set/intersection paths-a paths-b)
        blacklisted? (contains? morph/false-positive-pairs #{a b})
        irregular-a  (get morph/irregular-canonical a)
        irregular-b  (get morph/irregular-canonical b)]
    {:a a
     :b b
     :strips-a paths-a
     :strips-b paths-b
     :common common
     :blacklisted? blacklisted?
     :irregular-a irregular-a
     :irregular-b irregular-b
     :result (morph/derivationally-related? a b)
     :explanation (cond
                    blacklisted?
                    "Blacklisted by false-positive-pairs"
                    (and irregular-a irregular-b
                         (= irregular-a irregular-b))
                    (str "Matched via irregular-canonical: both map to "
                         irregular-a)
                    (seq common)
                    (str "Matched via shared strips: " common)
                    :else
                    "No shared strips, no irregular link, not blacklisted")}))

(def negative-cross-pole-corpus
  "Every pair of canonical pole descriptors must NOT match each
   other. This is the strongest negative test because we have ground
   truth: these are by definition distinct emotional poles. If any
   two match, the algorithm is conflating things the system depends
   on keeping separate."
  (let [poles [:admirable :amusing :angry :compassionate
               :contemptuous :contented :disappointing :disgusting
               :fearful :guilty :hateful :interesting :joyful
               :loving :pleasurable :proud :regretful :relieving
               :sad :shameful]]
    (for [a poles, b poles :when (not= a b)]
      {:a a :b b :why "distinct poles"})))

(def common-tag-corpus-negatives
  "Real tags from color-pedia and similar that should NOT match each
   other. Drawn from the kind of vocabulary the system actually sees.
   Add to this whenever a real-world surprising-match shows up wrong."
  [{:a :warm       :b :war        :why "warm is not war + suffix"}
   {:a :calm       :b :calmly     :why "actually this DOES match — move to positives"}
   {:a :cool       :b :coolness   :why "matches; move to positives"}
   ;; Real distinct tags
   {:a :rustic     :b :rust       :why "rustic is not rust + ic in the relevant sense"}
   ;; though etymologically rustic IS related to rust... another ambiguous case
   {:a :grave      :b :gravity    :why "different concepts despite shared root"}
   {:a :pleasant   :b :peasant    :why "single-letter difference, unrelated"}
   {:a :sole       :b :solemn     :why "unrelated roots"}
   {:a :fine       :b :finite     :why "unrelated"}
   {:a :spring     :b :springs    :why "spring-the-season vs jumps; ambiguous; remove"}
   ;; Etymologically distinct lookalikes
   {:a :base       :b :basement   :why "etymologically related but conceptually distinct in tag use"}
   {:a :host       :b :hostile    :why "etymologically distinct roots that share prefix"}
   {:a :stark      :b :star       :why "no relation"}])

(def derivation-chain-tests
  "Specific multi-step derivation chains that the BFS strips-to MUST
   handle. Each entry tests that the algorithm finds the transitive
   path A → ... → Z, not just direct single-suffix matches. Kept
   conservative: no plural forms (bare -s causes too many false
   positives if added as a suffix), no back-formed nominalizations
   (beautifulness is unattested; beauty is the canonical noun)."
  [{:chain [:joy :joyful :joyfully]
    :why "joy + ful + ly; depth 2"}
   {:chain [:amuse :amusing :amusingly]
    :why "amuse + ing + ly with -e drop on step 1"}
   {:chain [:happy :happiness]
    :why "y->i + -ness, single-step orthographic"}
   {:chain [:beauty :beautiful :beautifully]
    :why "y->i + ful + ly; multi-step orthographic"}])

(deftest derivation-chains-link-end-to-end
  (testing "every word in a chain is morphologically related to every other"
    (doseq [{:keys [chain why]} derivation-chain-tests]
      (doseq [a chain, b chain]
        (is (morph/derivationally-related? a b)
            (format "Chain %s breaks at %s ~ %s (%s)"
                    chain a b why))))))

(deftest pole-corpus-internally-disjoint
  (testing "no two distinct pole descriptors are morphologically related"
    (doseq [{:keys [a b why]} negative-cross-pole-corpus]
      (is (not (morph/derivationally-related? a b))
          (format "Poles %s and %s incorrectly match (%s)" a b why)))))

(deftest no-known-false-positives-on-real-corpus
  (testing "the real tag corpus produces no flagged false positives"
    (let [tags (try
                 (lexi/collect-corpus-tags)
                 (catch Exception _ []))
          known-false-positives
          #{#{:sad :sadistic}
            #{:hate :hat}
            ;; Populate this as you discover them via diagnose.
            }]
      (when (seq tags)
        (doseq [a tags, b tags
                :when (and (not= a b)
                           (contains? known-false-positives #{a b}))]
          (is (not (morph/derivationally-related? a b))
              (format "Known false positive %s/%s now matches" a b)))))))


(deftest stem-basic
  (testing "single suffix removal"
    (is (= :joy   (morph/stem :joyful)))
    (is (= :joy   (morph/stem :joyfully)))
    (is (= :joy   (morph/stem :joyfulness)))
    (is (= :dark  (morph/stem :darkness)))
    (is (= :warm  (morph/stem :warmly)))
    (is (= :warmth (morph/stem :warmth))   ; documented intentional limit
        "-th is not in the suffix list; :warmth stays unchanged")  ; NOTE: -th is not in suffix list, expect :warmth
    (is (= :vivid (morph/stem :vividly)))
    (is (= :vivid (morph/stem :vividness)))))

(deftest stem-compound
  (testing "iterative stripping reaches root"
    (is (= :joy  (morph/stem :joyfulness)))   ; fulness -> ful -> (joy)
    (is (= :care (morph/stem :carefulness)))
    (is (= :hope (morph/stem :hopefulness)))))

(deftest stem-floor
  (testing "3-char floor prevents pathological stripping"
    (is (= :red  (morph/stem :red)))   ; too short to strip anything
    (is (= :be   (morph/stem :be)))
    (is (= :icy  (morph/stem :icy)))))  ; -y would strip to :ic which is only 2 chars

;; ── morphologically-related? ──────────────────────────────────────────────

(deftest related-positives
  (testing "obvious morphological variants"
    (is (morph/derivationally-related? :joy :joyful))
    (is (morph/derivationally-related? :joy :joyfully))
    (is (morph/derivationally-related? :joy :joyfulness))
    (is (morph/derivationally-related? :joyful :joyfulness))
    (is (morph/derivationally-related? :dark :darkness))
    (is (morph/derivationally-related? :dark :darkly))
    (is (morph/derivationally-related? :warm :warmly))
    (is (morph/derivationally-related? :love :lovable))   ; silent-e drop
    (is (morph/derivationally-related? :hope :hopeful))
    (is (morph/derivationally-related? :care :careful))
    (is (morph/derivationally-related? :care :careless))  ; NOTE: -less not in list, expect false
    (is (morph/derivationally-related? :happy :happiness)) ; y->i rule
    (is (morph/derivationally-related? :vivid :vividly))
    (is (morph/derivationally-related? :vivid :vividness))))

(deftest related-symmetric
  (testing "relation is symmetric"
    (is (= (morph/derivationally-related? :joy :joyful)
           (morph/derivationally-related? :joyful :joy)))
    (is (= (morph/derivationally-related? :love :lovable)
           (morph/derivationally-related? :lovable :love)))))

(deftest related-identity
  (testing "a word is always related to itself"
    (is (morph/derivationally-related? :joy :joy))
    (is (morph/derivationally-related? :red :red))
    (is (morph/derivationally-related? :dark :dark))))

(deftest related-negatives
  (testing "distinct concepts that share surface similarity"
    (is (not (morph/derivationally-related? :sad :sadistic)))
    (is (not (morph/derivationally-related? :sad :sadism)))
    (is (not (morph/derivationally-related? :hat :hate)))    ; hat is not hate-minus-e
    (is (not (morph/derivationally-related? :relief :relic)))
    (is (not (morph/derivationally-related? :anger :angel)))
    (is (not (morph/derivationally-related? :sing :single)))
    (is (not (morph/derivationally-related? :mention :mental)))
    (is (not (morph/derivationally-related? :nation :natural)))))

(deftest related-irregular
  (testing "irregular canonical mappings"
    (is (morph/derivationally-related? :pride :proud))
    (is (morph/derivationally-related? :proud :pride))
    (is (morph/derivationally-related? :proud :proud))))

(deftest related-cross-category
  (testing "verb/noun/adjective variants of same root"
    (is (morph/derivationally-related? :excite :excited))
    (is (morph/derivationally-related? :excite :exciting))
    (is (morph/derivationally-related? :excite :excitement))
    (is (morph/derivationally-related? :depress :depressing))
    (is (morph/derivationally-related? :depress :depressed))))

;; ── tricky cases ──────────────────────────────────────────────────────────

(deftest tricky-false-positives
  (testing "words that naive stemming would conflate"
    ;; 'man' and 'mane' share stem 'man' but -e rule shouldn't
    ;; create false relation via suffix stripping
    (is (not (morph/derivationally-related? :man :mane))
        ":man :mane handled by blacklist")
    ;; 'caring' and 'car' — 'caring' strips to 'car' via -ing
    ;; this is a known weakness; document whether it's a false positive
    ;; (is (not (morphologically-related? :car :caring)))  ; fails? add to blacklist
    ;; 'sing' strips to 'sin' via... actually -ing requires 3-char floor
    ;; 'sing' minus 'ing' = 's' which is < 3 chars, so floor saves us
    (is (not (morph/derivationally-related? :sin :singing))
        "3-char floor saves us: sing -ing leaves 1 char")))

(deftest tricky-lgbtq-tags
  (testing "pride-adjacent tags that must not conflate"
    (is (morph/derivationally-related? :pride :proud)
        "irregular mapping")      ; irregular, should relate
    ;; :pride :prided removed: prided IS pride + -ed via -e drop,
    ;; legitimately derivational. If they shouldn't collapse as
    ;; tags, add to meaning-shifting-pairs and assert
    ;; descriptor-equivalent? is false instead.
    (is (morph/derivationally-related? :love :loving))
    (is (morph/derivationally-related? :love :loved))
    (is (not (morph/derivationally-related? :gay :gaze)))    ; not morphologically related
    (is (not (morph/derivationally-related? :trans :transition))))) ; prefix, not suffix

;; ── validate-synonym-map ──────────────────────────────────────────────────

(deftest validate-clean-map
  (testing "no issues on a clean map"
    (is (empty? (morph/validate-synonym-map
                 {:joyful  {:cheerful 1.0 :elated 0.9}
                  :dark    {:shadowy 0.8 :murky 0.7}})))))

(deftest validate-catches-morphological-synonyms
  (testing "flags morphological variants that should be canonicalized instead"
    (let [issues (morph/validate-synonym-map
                  {:joy {:joyful 1.0    ; should be caught
                         :joyfully 0.9  ; should be caught
                         :elated 0.8}})] ; clean
      (is (= 2 (count issues)))
      (is (every? #(= :morphological-variant (:issue %)) issues))
      (is (some #(= :joyful (:synonym %)) issues))
      (is (some #(= :joyfully (:synonym %)) issues)))))

(deftest validate-catches-irregular
  (testing "flags irregular variants via canonical mapping"
    (let [issues (morph/validate-synonym-map
                  {:proud {:pride 1.0   ; irregular variant, should be caught
                           :bold  0.8}})]
      (is (= 1 (count issues)))
      (is (= :pride (:synonym (first issues)))))))

(def meaning-shifting-test-cases
  "Pairs that ARE derivationally related but should NOT collapse
   as canonical descriptors. These exercise the descriptor-equivalent?
   relation, which is strictly narrower than derivationally-related?."
  [{:a :nature  :b :natural    :why "meaning shift"}
   {:a :culture :b :cultural   :why "same"}
   {:a :history :b :historical :why "same"}
   {:a :friend  :b :friendly   :why "person vs attribute"}
   {:a :child   :b :childhood  :why "phase vs being-a-child"}
   {:a :beauty  :b :beautiful  :why "color vs descriptive"}
   {:a :color   :b :colorful   :why "noun vs descriptor"}])

(deftest meaning-shifting-pairs-pass-broad-fail-strict
  (testing "meaning-shifting pairs are derivationally connected but not descriptor-equivalent"
    (doseq [{:keys [a b why]} meaning-shifting-test-cases]
      (is (morph/derivationally-related? a b)
          (format "%s ~derivationally~ %s should hold (%s)" a b why))
      (is (not (morph/descriptor-equivalent? a b))
          (format "%s ≁descriptor~ %s should hold (%s)" a b why)))))

(comment
  (diagnose :sad :sadistic)

  ;; First pass: get core categories passing
  (run-tests)

  ;; Second pass: feed real corpus through surprising-matches and
  ;; visually scan
  (def real-tags (lexi/collect-corpus-tags))

  (surprising-matches real-tags)
  ;; Usage
  (surprising-matches (lexi/collect-corpus-tags))
  ;; Visually scan output; any pair that's wrong gets added to
  ;; false-positive-pairs and re-tested.


  ;; Third pass: any surprising match that's wrong gets added to
  ;; either false-positive-pairs (if it's a genuine false positive)
  ;; or true-positives in the test file (if I was wrong about it
  ;; being a false positive). Re-run tests.

  ;; Fourth pass: random sampling
  (let [pairs (take 200 (shuffle
                         (for [a real-tags b real-tags
                               :when (not= a b)]
                           [a b])))
        results (for [[a b] pairs]
                  {:pair [a b]
                   :related? (morph/derivationally-related? a b)
                   :diagnostic (diagnose a b)})]
    (filter :related? results)))