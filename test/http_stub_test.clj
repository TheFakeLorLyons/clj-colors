(ns http-stub-test
  "Integration tests with the HTTP layer stubbed. Verifies that
   propose-association and propose-palette correctly compose the
   request, parse the response, and stage the draft. After refactor,
   these tests should pass identically — the stub points at the new
   llm.core/request-completion location."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [clj-colors.llm.associative :as la]
            [clj-colors.llm.core :as llm]
            [clj-colors.llm.palettes :as lp]))

(defn- fixture [name]
  (slurp (io/file "test-resources" name)))

(def ^:dynamic *stub-response* nil)

(defn- stub-request-completion
  "Return a canned response instead of calling the API."
  [_system-prompt _user-text]
  (if-let [text *stub-response*]
    {:text text
     :usage {:input_tokens 1000 :output_tokens 500}}
    (throw (ex-info "Stub response not set; test setup incomplete" {}))))

(defn- with-stub
  "Redefine request-completion to return the canned response. Pre-
   refactor this stubs the symbol in la; post-refactor it stubs in
   llm.core. The dispatch happens at one place per refactor."
  [f]
  (with-redefs [llm/request-completion stub-request-completion]
    (f)))

(use-fixtures :each
  (fn [f]
    (la/discard-draft!)
    (lp/discard-draft!)
    (f)
    (la/discard-draft!)
    (lp/discard-draft!)))

(deftest propose-association-stub-test
  (testing "propose-association parses fixture and stages draft"
    (binding [*stub-response* (fixture "obsidian.edn")]
      (with-stub
        (fn []
          (let [entry (la/propose-association "obsidian")]
            (is (= :mineral (:category entry)))
            (is (= "obsidian" (:referent (la/draft))))
            (is (contains? (get-in (la/draft) [:entry :tags]) :obsidian))))))))

(deftest propose-association-preamble-stub-test
  (testing "propose-association handles LLM preamble"
    (binding [*stub-response* (fixture "with-preamble.edn")]
      (with-stub
        (fn []
          (let [entry (la/propose-association "cherry-blossom")]
            (is (= :nature.flora (:category entry)))))))))

(deftest propose-palette-stub-test
  (testing "propose-palette parses simple palette response"
    (binding [*stub-response* "{:hex [\"#102030\" \"#506070\" \"#a0b0c0\" \"#d0e0f0\"]
                               :weights [0.3 0.3 0.2 0.2]
                               :tags #{\"calm\" \"oceanic\"}}"]
      (with-stub
        (fn []
          (let [entry (lp/propose-palette "calm-ocean")]
            (is (= 4 (count (:hex entry))))
            (is (= "calm-ocean" (:referent (lp/draft))))))))))

(deftest stub-request-failure-propagates
  (testing "parse failure produces actionable ex-info"
    (binding [*stub-response* "this is not edn at all"]
      (with-stub
        (fn []
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"No EDN map"
                                (la/propose-association "test"))))))))

(deftest usage-data-preserved-test
  (testing "usage data flows through to the draft for cost tracking"
    (binding [*stub-response* (fixture "obsidian.edn")]
      (with-stub
        (fn []
          (la/propose-association "obsidian")
          (is (= 1000 (get-in (la/draft) [:usage :input_tokens])))
          (is (= 500 (get-in (la/draft) [:usage :output_tokens]))))))))

(comment
  (run-tests))