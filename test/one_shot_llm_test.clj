(ns one-shot-llm-test
  "Smoke test for llm.core/one-shot, the composed
   request+strip+parse path that every propose function uses. Stubs
   request-completion so the test runs offline against a fixed
   response."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clj-colors.llm.core :as llm]))

(defn- fixture [name]
  (slurp (io/file "test-resources" name)))

(deftest one-shot-stubs-and-parses
  (testing "one-shot composes request, strip-fences, and parse"
    (with-redefs [llm/request-completion
                  (fn [_system-prompt _user-text]
                    {:text  (fixture "obsidian.edn")
                     :usage {:input_tokens 1000 :output_tokens 500}})]
      (let [{:keys [entry usage raw]}
            (llm/one-shot "test-system" "test-user" {:source :test})]
        (is (= :mineral (:category entry)))
        (is (contains? (:tags entry) :obsidian))
        (is (= 1000 (:input_tokens usage)))
        (is (= 500 (:output_tokens usage)))
        (is (string? raw))))))

(deftest one-shot-handles-fenced-response
  (testing "one-shot strips fences before parsing"
    (with-redefs [llm/request-completion
                  (fn [_system-prompt _user-text]
                    {:text  (fixture "fenced.edn")
                     :usage {:input_tokens 100 :output_tokens 50}})]
      (let [{:keys [entry]} (llm/one-shot "sys" "user" {})]
        (is (= :test (:category entry)))))))

(deftest one-shot-handles-preamble
  (testing "one-shot tolerates LLM preamble before the map"
    (with-redefs [llm/request-completion
                  (fn [_system-prompt _user-text]
                    {:text  (fixture "with-preamble.edn")
                     :usage {:input_tokens 100 :output_tokens 50}})]
      (let [{:keys [entry]} (llm/one-shot "sys" "user" {})]
        (is (= :nature.flora (:category entry)))
        (is (contains? (:tags entry) :cherry-blossom))))))

(deftest one-shot-propagates-context-on-failure
  (testing "parse failure preserves context info in ex-info"
    (with-redefs [llm/request-completion
                  (fn [_system-prompt _user-text]
                    {:text  (fixture "malformed.edn")
                     :usage {:input_tokens 50 :output_tokens 20}})]
      (try
        (llm/one-shot "sys" "user" {:referent "test-ref" :batch :flora-1})
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (let [data (ex-data e)]
            (is (= "test-ref" (:referent data)))
            (is (= :flora-1 (:batch data)))))))))