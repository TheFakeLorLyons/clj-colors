(ns clj-colors.llm.batch
  "Batch authoring of associations or palettes via LLM. Generates
   a sequence of proposals (optionally letting the LLM pick referent
   names), tracks token spend against a budget, and supports auto-
   accept, interactive review, or stage-for-later modes.

   State is persisted to resources/batch-state.edn after every
   proposal so you can resume after an exit (intentional or via
   budget exhaustion). Resume by passing :resume true to the same
   namespace; the state file's referents-remaining list is consumed.

   Pricing assumes Claude Sonnet 4.6 rates (roughly $3/$15 per MTok
   input/output as of Jan 2026). Pricing is rebindable via
   *pricing-per-mtok* for when rates change or you use a different
   model."
  (:require [clj-colors.authoring :as authoring]
            [clj-colors.llm.associative :as la]
            [clj-colors.llm.core :as llm]
            [clj-colors.llm.palettes :as lp]
            [clj-colors.main :as main]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(def author-state-path "resources/batch-author-state.edn")
(def modify-state-path "resources/batch-modify-state.edn")

(defn- read-state [path]
  (when (.exists (io/file path))
    (-> path slurp edn/read-string)))

(defn- write-state! [path state]
  (with-open [w (io/writer path)]
    (binding [*out* w]
      (pp/pprint state))))

(defn- clear-state! [path]
  (when (.exists (io/file path))
    (.delete (io/file path))))

(defn- read-state-checked
  "Read state from path and verify the :operation field matches
   expected-op. Throws if the file is for a different operation."
  [path expected-op]
  (when-let [state (read-state path)]
    (let [actual-op (:operation state)]
      (when (and actual-op (not= actual-op expected-op))
        (throw (ex-info "Resume state has mismatched operation"
                        {:path path
                         :expected expected-op
                         :found actual-op}))))
    state))

(def ^:dynamic *pricing-per-mtok*
  "Cost per million tokens for the current model. Adjust if pricing
   changes or you use a different model. The HTTP response from
   Anthropic includes usage stats; we estimate based on those plus
   this dict."
  {:input  3.0
   :output 15.0})

(def ^:dynamic *coverage-system-prompt*
  "You are choosing a set of referent names for a batch authoring
session. Given a namespace and a coverage description, respond with
a Clojure EDN vector of referent name strings.

The names should be:
  - Specific enough that an authoring step can produce real colors
    (not 'beauty' but 'cherry blossoms in spring')
  - Distinct from each other (don't propose 'tundra' and 'arctic')
  - Real-world or culturally established (not invented)
  - Aligned with the namespace (don't propose materials in :scene)

Respond with a single EDN vector of strings. No preamble, no
markdown fences, no commentary. Just the vector.

Example:
  Namespace: :flag
  Coverage: 'the 10 most visually distinctive flags'
  Output: [\"japan\" \"jamaica\" \"south-korea\" \"botswana\"
           \"north-macedonia\" \"kiribati\" \"trinidad-tobago\"
           \"swaziland\" \"belize\" \"saint-vincent\"]")

(def ^:dynamic *pricing-per-mtok* {:input 3.0 :output 15.0})

(defn estimate-cost [usage]
  (let [in-mtok (/ (:input_tokens usage 0) 1e6)
        out-mtok (/ (:output_tokens usage 0) 1e6)]
    (+ (* in-mtok (:input *pricing-per-mtok*))
       (* out-mtok (:output *pricing-per-mtok*)))))

(defn- generate-coverage
  [namespace-kw coverage-prompt]
  (let [user-msg (str "Namespace: " namespace-kw
                      "\nCoverage: " coverage-prompt)
        {:keys [entry]} (llm/one-shot *coverage-system-prompt*
                                      user-msg
                                      {:namespace namespace-kw})]
    entry))

(defn- propose-and-key
  [type namespace-kw referent]
  (case type
    :association
    (let [entry (la/propose-association referent)
          usage (:usage (la/draft))
          slug (-> referent
                   (str/replace #"\s+" "-")
                   str/lower-case)
          target-key (keyword (name namespace-kw) slug)]
      [target-key entry usage])

    :palette
    (let [entry (lp/propose-palette referent)
          usage (:usage (lp/draft))
          slug (-> referent
                   (str/replace #"\s+" "-")
                   str/lower-case)
          target-key (keyword (name namespace-kw) slug)]
      [target-key entry usage])))

(defn- accept-entry!
  "Persist a proposed entry under target-key. Returns the persisted
   form."
  [type target-key]
  (case type
    :association (la/accept! target-key)
    :palette     (lp/accept-palette! target-key)))

(defn- discard-draft! [type]
  (case type
    :association (la/discard-draft!)
    :palette     (lp/discard-draft!)))

(defn- interactive-prompt
  "Show the current draft, ask the user to accept/skip/refine/quit.
   Returns one of :accept :skip :quit, or recurses on :refine."
  [type target-key referent]
  (println "\n=== Draft for" target-key "(" referent ") ===")
  (case type
    :association (la/show-draft)
    :palette     (lp/show-draft))
  (println "\n[a]ccept  [s]kip  [r]efine  [q]uit")
  (print "> ") (flush)
  (let [input (clojure.string/trim (or (read-line) ""))]
    (case (clojure.string/lower-case input)
      "a" :accept
      ""  :accept
      "s" :skip
      "q" :quit
      "r" (do (print "Refinement request: ") (flush)
              (let [feedback (clojure.string/trim (or (read-line) ""))]
                (when (seq feedback)
                  (case type
                    :association (la/refine! feedback)
                    :palette     (lp/modify-palette target-key feedback)))
                (interactive-prompt type target-key referent)))
      (do (println "Unrecognized; try a/s/r/q")
          (interactive-prompt type target-key referent)))))

(defn batch-author!
  "Author a batch of associations or palettes.

   Required:
     :type           :association or :palette
     :namespace      keyword for the key namespace (e.g. :scene)
     :referents      vector of referent strings
                     OR :coverage-prompt + the LLM picks names

   Optional:
     :coverage-prompt   when :referents not supplied, ask LLM to
                        generate referent names from this description
     :review-mode       :auto (accept all), :stage-only (no accept),
                        :interactive (default, prompt per entry)
     :checkpoint-usd    interval in dollars between confirmation
                        prompts. Default 5.0. Pass nil to disable.
     :resume            true to resume from saved state

   Behavior:
     - Persists state after each proposal so resume works
     - Tracks spend from actual API usage when available, falling
       back to a $0.013-per-call heuristic
     - Single propose failures (parse errors, etc.) are logged and
       the batch continues; the failed entry appears in :results
       with :status :propose-failed
     - On Ctrl-C or exception, state is saved with progress so far
     - Returns a summary map of what was done"
  [{:keys [type namespace referents coverage-prompt review-mode
           checkpoint-usd resume skip-existing authored-file]
    :or   {review-mode    :interactive
           checkpoint-usd 5.0
           skip-existing  false}}]
  (when-not (#{:association :palette} type)
    (throw (ex-info "Type must be :association or :palette" {:type type})))
  (binding [clj-colors.authoring/*authored-path*
            (if authored-file
              (clj-colors.authoring/resolve-path authored-file)
              clj-colors.authoring/*authored-path*)]
    (let [existing-state (when resume (read-state author-state-path))
          referents
          (if skip-existing
            (let [existing-keys (set (keys (case type
                                             :association @clj-colors.associations/data
                                             :palette     @main/registry)))
                  ns-name (name namespace)
                  kept (remove (fn [ref]
                                 (let [slug (-> ref
                                                (clojure.string/replace #"\s+" "-")
                                                clojure.string/lower-case)
                                       k    (keyword ns-name slug)]
                                   (contains? existing-keys k)))
                               referents)
                  skipped (- (count referents) (count kept))]
              (when (pos? skipped)
                (println (format "Skipping %d already-existing entries; authoring %d new."
                                 skipped (count kept))))
              (vec kept))
            referents)
          starting-spend (or (:spent-usd existing-state) 0.0)
          results-so-far (or (:results existing-state) [])
          spend          (atom starting-spend)
          last-checkpoint-multiple (atom (long (/ starting-spend (or checkpoint-usd 1e9))))
          todo-atom      (atom referents)
          done-atom      (atom results-so-far)]
      (println (format "Batch authoring: %d referents in :%s%s (already spent: $%.2f)"
                       (count referents) (name namespace)
                       (if checkpoint-usd
                         (format ", checkpoint every $%.2f" checkpoint-usd)
                         ", no checkpoints")
                       starting-spend))
      (try
        (loop [todo referents
               done results-so-far]
          (reset! todo-atom todo)
          (reset! done-atom done)
          (cond
            (empty? todo)
            (do (clear-state! author-state-path)
                {:status    :complete
                 :authored  done
                 :spent-usd @spend
                 :remaining []})

            (and checkpoint-usd
                 (>= @spend (* checkpoint-usd (inc @last-checkpoint-multiple))))
            (do (println (format "\n--- Checkpoint: spent $%.2f on this batch ---" @spend))
                (println "Continue? [y]es / [q]uit and save state")
                (print "> ") (flush)
                (let [response (clojure.string/lower-case
                                (clojure.string/trim (or (read-line) "")))]
                  (case response
                    "q" (do (write-state! author-state-path
                                          {:type                :author
                                           :operation           :author
                                           :namespace           namespace
                                           :referents-remaining todo
                                           :results             done
                                           :spent-usd           @spend
                                           :saved-at            (str (java.time.Instant/now))})
                            {:status      :user-quit-at-checkpoint
                             :authored    done
                             :spent-usd   @spend
                             :remaining   todo
                             :state-saved author-state-path})
                    (do (swap! last-checkpoint-multiple inc)
                        (recur todo done)))))

            :else
            (let [ref (first todo)
                  _   (println (format "\n[%d/%d] %s (spent $%.2f)"
                                       (- (count referents) (count todo) -1)
                                       (count referents)
                                       ref @spend))
                  result (try
                           (let [[k _ usage] (propose-and-key type namespace ref)
                                 cost        (if usage (estimate-cost usage) 0.013)
                                 _           (swap! spend + cost)
                                 action (case review-mode
                                          :auto        :accept
                                          :stage-only  :skip
                                          :interactive (interactive-prompt type k ref))
                                 res    (case action
                                          :accept
                                          (let [ok (try
                                                     (accept-entry! type k)
                                                     (println (format "  ✓ accepted as %s" k))
                                                     true
                                                     (catch Exception e
                                                       (println (format "  ✗ accept failed: %s" (.getMessage e)))
                                                       {:error (.getMessage e)}))]
                                            (if (= ok true)
                                              {:key k :referent ref :status :accepted}
                                              {:key k :referent ref :status :failed :error (:error ok)}))
                                          :skip
                                          (do (discard-draft! type)
                                              (println "  - skipped")
                                              {:key k :referent ref :status :skipped})
                                          :quit
                                          (do (write-state! author-state-path
                                                            {:type                :author
                                                             :operation           :author
                                                             :namespace           namespace
                                                             :referents-remaining todo
                                                             :results             done
                                                             :spent-usd           @spend
                                                             :saved-at            (str (java.time.Instant/now))})
                                              (println "Quitting; state saved.")
                                              ::quit))]
                             res)
                           (catch Exception e
                             (println (format "  ✗ propose failed: %s" (.getMessage e)))
                             (swap! spend + 0.013)
                             {:referent ref :status :propose-failed :error (.getMessage e)}))]
              (if (= result ::quit)
                {:status      :user-quit
                 :authored    done
                 :spent-usd   @spend
                 :remaining   todo
                 :state-saved author-state-path}
                (recur (rest todo) (conj done result))))))
        (catch Exception e
          (write-state! author-state-path
                        {:type                :author
                         :operation           :author
                         :namespace           namespace
                         :referents-remaining @todo-atom
                         :results             @done-atom
                         :spent-usd           @spend
                         :error               (.getMessage e)
                         :saved-at            (str (java.time.Instant/now))})
          (throw e))))))

(defn- modify-and-stage
  "Load an existing entry, apply LLM modification with the feedback,
   leave the result as the current draft. Returns [target-key
   draft-entry] for review."
  [type k feedback]
  (case type
    :association
    (do (la/load-as-draft! k)
        (la/refine! feedback)
        [k (:entry (la/draft))])

    :palette
    (do (lp/modify-palette k feedback)
        [k (:entry (lp/draft))])))

(defn- resolve-modifications
  [type {keys-list :keys
         :keys [modifications namespace selector feedback]}]
  (let [data (case type
               :association @clj-colors.associations/data
               :palette     @main/registry)]
    (cond
      modifications
      (vec modifications)

      keys-list
      (do (when-not feedback
            (throw (ex-info ":keys requires :feedback" {})))
          (mapv (fn [k] [k feedback]) keys-list))

      namespace
      (do (when-not feedback
            (throw (ex-info ":namespace requires :feedback" {})))
          (let [matching-keys
                (->> (clojure.core/keys data)
                     (filter #(= (name namespace)
                                 (clojure.core/namespace %))))]
            (mapv (fn [k] [k feedback]) matching-keys)))

      selector
      (do (when-not feedback
            (throw (ex-info ":selector requires :feedback" {})))
          (->> data
               (filter (fn [[k v]] (selector k v)))
               (mapv (fn [[k _]] [k feedback]))))

      :else
      (throw (ex-info "Must supply :modifications, :keys, :namespace, or :selector"
                      {})))))

(defn batch-modify!
  "Modify a batch of existing associations or palettes via LLM.

   Required:
     :type           :association or :palette

   Selection (provide ONE of):
     :modifications  {:key1 \"feedback1\" :key2 \"feedback2\"}
                     Per-entry feedback strings
     :keys + :feedback
                     Apply the same feedback to specific keys
     :namespace + :feedback
                     Apply the same feedback to all keys with this
                     namespace (e.g. :flag matches :flag/japan,
                     :flag/france, etc.)
     :selector + :feedback
                     (fn [key entry] ...) predicate over the data;
                     applies feedback to all matching entries

   Optional:
     :review-mode    :auto (accept all), :stage-only (no accept),
                     :interactive (default, prompt per entry)
     :checkpoint-usd interval in dollars between confirmation
                     prompts. Default 5.0. Pass nil to disable.
     :resume         true to resume from saved state

   Examples:
     ;; Uniform refinement across a namespace, no checkpoint
     (batch-modify!
      {:type :association
       :namespace :flag
       :feedback \"add :iconic tag at weight 0.7\"
       :review-mode :auto
       :checkpoint-usd nil})

     ;; Different feedback per entry, reviewed individually
     (batch-modify!
      {:type :association
       :modifications {:mineral/obsidian \"tighten sigma to 0.03\"
                       :mineral/jade \"shift the green slightly cooler\"
                       :mineral/amber \"add :preserved tag at weight 0.6\"}
       :review-mode :interactive})

     ;; Predicate-based selection
     (batch-modify!
      {:type :association
       :selector (fn [k entry]
                   (and (= (some-> k namespace keyword) :scene)
                        (> (count (:colors entry)) 6)))
       :feedback \"reduce to 5 most representative colors\"
       :review-mode :interactive})

     ;; Resume an interrupted modify session
     (batch-modify!
      {:type :association
       :resume true})"
  [{:keys [type review-mode checkpoint-usd resume authored-file]
    :or   {review-mode    :interactive
           checkpoint-usd 5.0}
    :as   opts}]
  (when-not (#{:association :palette} type)
    (throw (ex-info "Type must be :association or :palette" {:type type})))
  (binding [clj-colors.authoring/*authored-path*
            (if authored-file
              (clj-colors.authoring/resolve-path authored-file)
              clj-colors.authoring/*authored-path*)]
    (let [existing-state (when resume (read-state modify-state-path))
          pairs          (cond
                           existing-state (:remaining existing-state)
                           :else          (resolve-modifications type opts))]
      (if (empty? pairs)
        (do (println "Warning: no entries matched the selection criteria.")
            (println "  Type:" type)
            (println "  Namespace:" (:namespace opts))
            (println "  Keys:" (:keys opts))
            (println "  Modifications:" (count (:modifications opts)))
            (println "Nothing to do; exiting.")
            {:status    :no-matches
             :modified  []
             :spent-usd 0.0
             :remaining []})
        (let [starting-spend (or (:spent-usd existing-state) 0.0)
              results-so-far (or (:results existing-state) [])
              spend          (atom starting-spend)
              last-checkpoint-multiple (atom (long (/ starting-spend (or checkpoint-usd 1e9))))
              todo-atom      (atom pairs)
              done-atom      (atom results-so-far)]
          (println (format "Batch modify: %d entries%s (already spent: $%.2f)"
                           (count pairs)
                           (if checkpoint-usd
                             (format ", checkpoint every $%.2f" checkpoint-usd)
                             ", no checkpoints")
                           starting-spend))
          (try
            (loop [todo pairs
                   done results-so-far]
              (reset! todo-atom todo)
              (reset! done-atom done)
              (cond
                (empty? todo)
                (do (clear-state! modify-state-path)
                    {:status    :complete
                     :modified  done
                     :spent-usd @spend
                     :remaining []})

                (and checkpoint-usd
                     (>= @spend (* checkpoint-usd (inc @last-checkpoint-multiple))))
                (do (println (format "\n--- Checkpoint: spent $%.2f on this batch ---" @spend))
                    (println "Continue? [y]es / [q]uit and save state")
                    (print "> ") (flush)
                    (let [response (clojure.string/lower-case
                                    (clojure.string/trim (or (read-line) "")))]
                      (case response
                        "q" (do (write-state! modify-state-path
                                              {:type      type
                                               :operation :modify
                                               :remaining todo
                                               :results   done
                                               :spent-usd @spend
                                               :saved-at  (str (java.time.Instant/now))})
                                {:status      :user-quit-at-checkpoint
                                 :modified    done
                                 :spent-usd   @spend
                                 :remaining   todo
                                 :state-saved modify-state-path})
                        (do (swap! last-checkpoint-multiple inc)
                            (recur todo done)))))

                :else
                (let [[k feedback] (first todo)
                      _ (println (format "\n[%d/%d] %s :: %s (spent $%.2f)"
                                         (- (count pairs) (count todo) -1)
                                         (count pairs)
                                         k feedback
                                         @spend))
                      _ (modify-and-stage type k feedback)
                      usage (:usage (la/draft))      ;; or lp/draft for palettes
                      cost  (if usage (estimate-cost usage) 0.013)
                      _ (swap! spend + cost)
                      action   (case review-mode
                                 :auto        :accept
                                 :stage-only  :skip
                                 :interactive (interactive-prompt type k (str k)))
                      result   (case action
                                 :accept
                                 (let [ok (try
                                            (accept-entry! type k)
                                            (println (format "  ✓ updated %s" k))
                                            true
                                            (catch Exception e
                                              (println (format "  ✗ accept failed: %s" (.getMessage e)))
                                              {:error (.getMessage e)}))]
                                   (if (= ok true)
                                     {:key k :feedback feedback :status :accepted}
                                     {:key k :feedback feedback :status :failed :error (:error ok)}))
                                 :skip
                                 (do (discard-draft! type)
                                     (println "  - skipped, no change")
                                     {:key k :feedback feedback :status :skipped})
                                 :quit
                                 (do (write-state! modify-state-path
                                                   {:type      type
                                                    :operation :modify
                                                    :remaining todo
                                                    :results   done
                                                    :spent-usd @spend
                                                    :saved-at  (str (java.time.Instant/now))})
                                     (println "Quitting; state saved.")
                                     ::quit))]
                  (if (= result ::quit)
                    {:status      :user-quit
                     :modified    done
                     :spent-usd   @spend
                     :remaining   todo
                     :state-saved modify-state-path}
                    (recur (rest todo) (conj done result))))))
            (catch Exception e
              (write-state! modify-state-path
                            {:type      type
                             :operation :modify
                             :remaining @todo-atom
                             :results   @done-atom
                             :spent-usd @spend
                             :error     (.getMessage e)
                             :saved-at  (str (java.time.Instant/now))})
              (throw e))))))))

(comment
  (batch-author!
   {:type :association
    :namespace :flag
    :referents ["japan" "indonesia" "france" "italy" "ireland"
                "belgium" "germany" "netherlands" "russia" "poland"
                "brazil" "mexico" "south-africa" "india" "south-korea"
                "vietnam" "canada" "switzerland" "ethiopia" "jamaica"
                "nepal" "mozambique" "saudi-arabia" "kiribati" "belize"
                "bhutan" "botswana" "trinidad-tobago"
                "saint-vincent-grenadines" "north-macedonia"]
    :review-mode :auto})
  
  (batch-author!
   {:type :association
    :namespace :flag
    :referents (mapv (comp name first) reference)
    :review-mode :auto})

  ;; Run with default checkpoint every $5
  (batch-author!
   {:type :association
    :namespace :flag
    :referents [...]
    :review-mode :auto})

  ;; Run with no checkpoint at all (full send)
  (batch-author!
   {:type :association
    :namespace :flag
    :referents [...]
    :review-mode :auto
    :checkpoint-usd nil})

  ;; Run with $2 checkpoints if you're paranoid
  (batch-author!
   {:type :association
    :namespace :flag
    :referents [...]
    :review-mode :auto
    :checkpoint-usd 2.0})
  
  (batch/batch-author!
   {:type :association
    :namespace :flag
    :referents (mapv (comp name first) reference)
    :review-mode :auto
    :skip-existing true})
  )