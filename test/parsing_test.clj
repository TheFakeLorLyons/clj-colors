(ns parsing-test
  "Behavioral tests for LLM response parsing. These exercise the
   public parse path (strip-fences + extraction + parse) against
   fixture files that capture real LLM response shapes: naked EDN,
   fenced, preamble-wrapped, duplicate-maps, namespaced keywords,
   and deeply nested structures.

   Tests target llm.core directly. The back-compat shims in
   llm.associative are tech debt and not under test here.

   Fixtures live in test-resources/ at the project root and are
   accessed via filesystem path (io/file) so they work whether or
   not the :test alias is on the classpath."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.java.io :as io]
            [clj-colors.llm.core :as llm]))

(defn- fixture
  "Slurp a fixture file from test-resources/."
  [name]
  (slurp (io/file "test-resources" name)))

(defn- parse-via-public-api
  "Run a fixture through the same path one-shot uses: strip fences,
   then parse-llm-edn. Bypasses HTTP so we test parsing in isolation."
  [raw]
  (llm/parse-llm-edn (llm/strip-fences raw) {:source :test}))

(deftest strip-fences-test
  (testing "naked EDN passes unchanged"
    (is (= "{:a 1}" (llm/strip-fences "{:a 1}"))))

  (testing "edn fence removed"
    (is (= "{:a 1}" (llm/strip-fences "```edn\n{:a 1}\n```"))))

  (testing "clojure fence removed"
    (is (= "{:a 1}" (llm/strip-fences "```clojure\n{:a 1}\n```"))))

  (testing "bare fence removed"
    (is (= "{:a 1}" (llm/strip-fences "```\n{:a 1}\n```"))))

  (testing "leading and trailing whitespace trimmed"
    (is (= "{:a 1}" (llm/strip-fences "\n\n  {:a 1}  \n\n")))))

(deftest parse-naked-edn-test
  (testing "obsidian fixture parses to expected shape"
    (let [parsed (parse-via-public-api (fixture "obsidian.edn"))]
      (is (= :mineral (:category parsed)))
      (is (contains? (:tags parsed) :obsidian))
      (is (= 0.04 (:sigma parsed)))
      (is (= 5 (count (:colors parsed)))))))

(deftest parse-with-preamble-test
  (testing "preamble before map is stripped, full outer map returned"
    ;; The fixture has nested {:weight 1.0} maps inside :tags.
    ;; The new scanner advances past the outer map, so we get
    ;; the full outer map, not the deepest nested one.
    (let [parsed (parse-via-public-api (fixture "with-preamble.edn"))]
      (is (= :nature.flora (:category parsed)))
      (is (contains? (:tags parsed) :cherry-blossom))
      (is (= 4 (count (:colors parsed)))))))

(deftest parse-fenced-test
  (testing "edn-fenced response parses correctly"
    (let [parsed (parse-via-public-api (fixture "fenced.edn"))]
      (is (= :test (:category parsed)))
      (is (= "Fenced response." (:rationale parsed))))))

(deftest parse-duplicate-maps-test
  (testing "duplicate maps: corrected (last top-level) version wins"
    (let [parsed (parse-via-public-api (fixture "duplicate-maps.edn"))]
      (is (= 0.04 (:sigma parsed)))
      (is (= 2 (count (:colors parsed))))
      (is (= "Better version." (:rationale parsed))))))

(deftest parse-namespaced-keywords-test
  (testing "namespaced keywords with digit-containing names parse"
    (let [parsed (parse-via-public-api (fixture "namespaced-keys.edn"))]
      (is (contains? (:tags parsed) :kaggle-emotional/k0445))
      (is (contains? (:tags parsed) :nature.flora/cherry)))))

(deftest parse-palette-shape-test
  (testing "palette response (different shape) parses correctly"
    (let [parsed (parse-via-public-api (fixture "palette-simple.edn"))]
      (is (= 4 (count (:hex parsed))))
      (is (vector? (:hex parsed)))
      (is (set? (:tags parsed))))))

(deftest parse-malformed-throws
  (testing "no parseable map produces ex-info with diagnostic context"
    (try
      (parse-via-public-api (fixture "malformed.edn"))
      (is false "should have thrown")
      (catch clojure.lang.ExceptionInfo e
        (is (re-find #"No EDN map" (.getMessage e)))
        (is (= :test (:source (ex-data e))))
        (is (string? (:raw (ex-data e))))))))

(deftest extract-last-map-direct-test
  (testing "extract-last-map-from-string returns the LAST top-level map"
    ;; The new scanner finds top-level maps only. For two
    ;; sequential maps separated by prose, the second wins.
    (let [input "Try one: {:a 1}\nTry two: {:b 2 :c 3}"
          result (llm/extract-last-map-from-string input)]
      (is (some? result))
      (is (= "{:b 2 :c 3}" (:string result)))))

  (testing "extract-last-map-from-string returns the OUTER map for nested"
    ;; The scanner advances past the outer map's end, so inner
    ;; maps are NOT separately reported.
    (let [input "{:a {:b 1 :c {:d 2}}}"
          result (llm/extract-last-map-from-string input)]
      (is (some? result))
      (is (= "{:a {:b 1 :c {:d 2}}}" (:string result))))))

(deftest deep-nesting-protection-test
  (testing "obsidian-style entries with nested :weight maps return outer"
    ;; This is the regression test for the bug we just fixed.
    ;; Without the advance-past-end scanner, this would return
    ;; the deepest {:weight ...} map instead of the outer one.
    (let [parsed (parse-via-public-api (fixture "obsidian.edn"))]
      (is (contains? parsed :colors))
      (is (contains? parsed :tags))
      (is (contains? parsed :sigma))
      (is (contains? parsed :rationale))
      ;; specifically NOT a {:weight ...} map
      (is (not (contains? parsed :weight))))))

(deftest empty-map-not-substantive-test
  (testing "empty map is not treated as a candidate"
    ;; substantive-map-node? rejects {}. So a response with an
    ;; empty placeholder followed by a real map should pick the
    ;; real one.
    (let [input "Draft: {}\nFinal: {:a 1 :b 2}"
          result (llm/extract-last-map-from-string input)]
      (is (= "{:a 1 :b 2}" (:string result))))))

(comment
  (run-tests))