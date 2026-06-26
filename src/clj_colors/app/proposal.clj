(ns clj-colors.app.proposal
  "HTTP handlers for the proposal/refinement interface. Browser-facing
   wrapper around clj-colors.llm.associative and clj-colors.llm.palettes.
   The browser calls propose, refine, accept, discard; the server returns
   the current draft state so the page can render it."
  (:require [clj-colors.app.server :as server]
            [clj-colors.authoring :as authoring]
            [clj-colors.main :as main]
            [clj-colors.llm.associative :as la]
            [clj-colors.llm.batch :as lb]
            [clj-colors.llm.palettes :as lp]))

(defn- handle-load-association-entry
  "Stage an arbitrary entry as the current association draft.
   Used by batch modify so we can refine a not-yet-persisted entry.
   Body: {:entry {...} :referent \"...\"}"
  [req]
  (let [{:keys [entry referent]} (server/read-body req)]
    (la/discard-draft!)
    (la/stage-entry! referent entry)
    (server/edn-response {:loaded true :draft (la/draft)})))

(defn- handle-load-palette-entry
  [req]
  (let [{:keys [entry referent]} (server/read-body req)]
    (lp/discard-draft!)
    (lp/stage-entry! referent entry)
    (server/edn-response {:loaded true :draft (lp/draft)})))

;; --- Association handlers ---

(defn- handle-propose-association
  "Propose an association from a referent name with optional extra
   context. The context is appended to the referent prompt as a
   clarifying note so the LLM can disambiguate.

   Body:
     {:referent \"rick\"
      :context \"the character from Rick and Morty\"}  ; optional

   Returns:
     {:draft {:referent ... :entry {...} :usage {...}}
      :entry {...}}
   so the page can render the staged draft immediately."
  [req]
  (let [{:keys [referent context]} (server/read-body req)
        entry (la/propose-association referent context)]
    (server/edn-response
     {:draft (la/draft)
      :entry entry})))

(defn- handle-batch-propose-associations
  [req]
  (let [{:keys [referents contexts namespace]} (server/read-body req)
        ctxs   (or contexts (repeat (count referents) nil))
        pairs  (map vector referents ctxs)
        drafts (mapv (fn [[ref ctx]]
                       (let [entry (la/propose-association ref ctx)
                             draft (la/draft)
                             slug  (-> ref
                                       (clojure.string/replace #"\s+" "-")
                                       clojure.string/lower-case)
                             key   (keyword (name namespace) slug)]
                         (la/discard-draft!)
                         {:referent ref :context ctx :proposed-key key
                          :entry entry :usage (:usage draft)}))
                     pairs)]
    (server/edn-response {:drafts drafts})))

(defn- handle-refine-association
  "Refine the currently staged association draft. Body: {:feedback T}.
   Returns the new draft, same shape as propose."
  [req]
  (let [{:keys [feedback]} (server/read-body req)
        entry (la/refine! feedback)]
    (server/edn-response
     {:draft (la/draft)
      :entry entry})))

(defn- handle-accept-association
  "Persist the staged association under the given key.
   Body: {:key :ns/name :authored-file F?}"
  [req]
  (let [{:keys [key]} (server/read-body req)
        persisted (la/accept! key)]
    (server/edn-response
     {:accepted key :entry persisted})))

(defn- handle-discard-association
  "Discard the staged association draft."
  [_req]
  (la/discard-draft!)
  (server/edn-response {:discarded true}))

(defn- handle-current-association-draft [_req]
  (server/edn-response {:draft (la/draft)}))

;; --- Palette handlers ---

(defn- handle-propose-palette
  "Propose a palette. Body:
     {:referent \"sunset\"
      :context \"over the ocean\"     ; optional
      :seed-hexes [\"#ff8000\" \"#001a4e\"]}  ; optional
   Returns: {:draft {...} :entry {...}}"
  [req]
  (let [{:keys [referent context seed-hexes]} (server/read-body req)
        entry (lp/propose-palette referent context seed-hexes)]
    (server/edn-response
     {:draft (lp/draft)
      :entry entry})))

(defn- handle-refine-palette
  "Refine the staged palette draft. Body: {:feedback T}."
  [req]
  (let [{:keys [feedback]} (server/read-body req)
        entry (lp/refine! feedback)]
    (server/edn-response
     {:draft (lp/draft)
      :entry entry})))

(defn- handle-accept-palette
  "Persist the staged palette under the given key.
   Body: {:key :ns/name :path P?}"
  [req]
  (let [{:keys [key path]} (server/read-body req)
        persisted (lp/accept-palette! key)
        save-result (when path (clj-colors.main/save-registry! path))]
    (server/edn-response
     (cond-> {:accepted key :entry persisted}
       save-result (assoc :saved save-result)))))

(defn- handle-discard-palette
  [_req]
  (lp/discard-draft!)
  (server/edn-response {:discarded true}))

(defn- handle-current-palette-draft [_req]
  (server/edn-response {:draft (lp/draft)}))

;; --- Modify-existing handlers ---

(defn- handle-load-association-for-modify
  "Load an existing association into the draft slot so the user can
   modify it via refine. Body: {:key :ns/name}.
   Returns the loaded entry plus the draft state."
  [req]
  (let [{:keys [key]} (server/read-body req)
        entry (la/load-as-draft! key)]
    (server/edn-response
     {:loaded key :entry entry :draft (la/draft)})))

(defn- handle-load-palette-for-modify
  [req]
  (let [{:keys [key]} (server/read-body req)
        entry (lp/load-as-draft! key)]
    (server/edn-response
     {:loaded key :entry entry :draft (lp/draft)})))

;; --- Batch handlers  ----------------------------------------------

(defn- entry-key
  "Build a namespaced key from a namespace string and a referent."
  [ns-str referent]
  (let [slug (-> referent
                 clojure.string/trim
                 clojure.string/lower-case
                 (clojure.string/replace #"\s+" "-")
                 (clojure.string/replace #"[^a-z0-9-]" ""))]
    (keyword (clojure.string/replace ns-str #"^:" "") slug)))

(defn- gather-explicit-entries
  "From explicit-mode entries, build a vector of
   {:proposed-key :referent :context :seed-hexes}.
   Per-entry :namespace overrides the batch-level namespace-str."
  [namespace-str entries]
  (mapv (fn [{:keys [referent context seed-hexes namespace]}]
          {:proposed-key (entry-key (or namespace namespace-str) referent)
           :referent     referent
           :context      context
           :seed-hexes   seed-hexes})
        entries))

(defn- gather-category-entries
  "Call brainstorm, then build the same shape as gather-explicit-entries."
  [category-counts extra-context]
  (let [{:keys [categories]} (la/brainstorm-referents category-counts extra-context)]
    (vec
     (for [[cat-kw refs] categories
           ref refs]
       {:proposed-key (entry-key (name cat-kw) ref)
        :referent     ref
        :context      extra-context
        :seed-hexes   nil}))))


;; --- Association batch ---

(defn- handle-batch-brainstorm
  "Brainstorm referents for category-mode batches.
   Returns planned entries WITHOUT proposing them.
   Body: {:category-counts [...] :extra-context \"...\"}
   Returns: {:entries [{:proposed-key :referent :context} ...]
             :brainstorm-usage {...}}"
  [req]
  (let [{:keys [category-counts extra-context]} (server/read-body req)
        {:keys [categories usage]} (la/brainstorm-referents category-counts extra-context)]
    (server/edn-response
     {:entries (vec
                (for [[cat-kw refs] categories
                      ref refs]
                  {:proposed-key (entry-key (name cat-kw) ref)
                   :referent ref
                   :context extra-context
                   :seed-hexes nil}))
      :brainstorm-usage usage})))

(defn- handle-batch-propose-associations
  "Body:
     ;; explicit mode
     {:mode :explicit
      :namespace \"ocean\"
      :extra-context \"...\"   ; optional, applied to all
      :entries [{:referent \"coral-reef\" :context \"...\"} ...]}

     ;; category mode
     {:mode :category
      :extra-context \"...\"   ; optional
      :category-counts [{:category \"ocean\" :count 5}
                        {:category \"forest\" :count 3}]}

   Returns: {:drafts [{:proposed-key :referent :entry :usage}...]
             :brainstorm-usage {...}}  ; only present in category mode"
  [req]
  (let [{:keys [mode namespace extra-context entries category-counts]}
        (server/read-body req)
        ;; Resolve the planned entries before any LLM proposals
        {:keys [planned brainstorm-usage]}
        (case (keyword mode)
          :explicit {:planned (gather-explicit-entries namespace entries)
                     :brainstorm-usage nil}
          :category (let [{:keys [categories usage]}
                          (la/brainstorm-referents category-counts extra-context)]
                      {:planned (vec
                                 (for [[cat-kw refs] categories
                                       ref refs]
                                   {:proposed-key (entry-key (name cat-kw) ref)
                                    :referent ref
                                    :context extra-context
                                    :seed-hexes nil}))
                       :brainstorm-usage usage}))
        drafts
        (mapv (fn [{:keys [proposed-key referent context]}]
                (try
                  (let [entry (la/propose-association referent context)
                        usage (:usage (la/draft))]
                    (la/discard-draft!)
                    {:proposed-key proposed-key
                     :referent     referent
                     :context      context
                     :entry        entry
                     :usage        usage})
                  (catch Exception e
                    (la/discard-draft!)
                    {:proposed-key proposed-key
                     :referent     referent
                     :context      context
                     :error        (str (.getSimpleName (class e)) ": "
                                        (ex-message e))})))
              planned)]
    (server/edn-response
     (cond-> {:drafts drafts}
       brainstorm-usage (assoc :brainstorm-usage brainstorm-usage)))))

(defn- handle-batch-accept-associations
  "Body: {:entries [{:key :ns/name :entry {...}} ...]}
   Persists each entry under its key. Returns counts."
  [req]
  (let [{:keys [entries]} (server/read-body req)
        results (mapv (fn [{:keys [key entry]}]
                        (try
                          (let [persisted (authoring/add-association!
                                           key (assoc entry :source :llm-generated))]
                            {:key key :ok true})
                          (catch Exception e
                            {:key key :ok false
                             :error (ex-message e)})))
                      entries)]
    (server/edn-response
     {:accepted (count (filter :ok results))
      :failed   (filterv (complement :ok) results)})))

;; --- Palette batch ---

(defn- handle-batch-propose-palettes
  "Same body shape as associations batch. Seed hexes are honored in
   explicit mode."
  [req]
  (let [{:keys [mode namespace extra-context entries category-counts]}
        (server/read-body req)
        {:keys [planned brainstorm-usage]}
        (case (keyword mode)
          :explicit {:planned (gather-explicit-entries namespace entries)
                     :brainstorm-usage nil}
          :category (let [{:keys [categories usage]}
                          (la/brainstorm-referents category-counts extra-context)]
                      {:planned (vec
                                 (for [[cat-kw refs] categories
                                       ref refs]
                                   {:proposed-key (entry-key (name cat-kw) ref)
                                    :referent ref
                                    :context extra-context
                                    :seed-hexes nil}))
                       :brainstorm-usage usage}))
        drafts
        (mapv (fn [{:keys [proposed-key referent context seed-hexes]}]
                (try
                  (let [entry (lp/propose-palette referent context seed-hexes)
                        usage (:usage (lp/draft))]
                    (lp/discard-draft!)
                    {:proposed-key proposed-key
                     :referent     referent
                     :context      context
                     :seed-hexes   seed-hexes
                     :entry        entry
                     :usage        usage})
                  (catch Exception e
                    (lp/discard-draft!)
                    {:proposed-key proposed-key
                     :referent     referent
                     :context      context
                     :error        (str (.getSimpleName (class e)) ": "
                                        (ex-message e))})))
              planned)]
    (server/edn-response
     (cond-> {:drafts drafts}
       brainstorm-usage (assoc :brainstorm-usage brainstorm-usage)))))

(defn- handle-batch-accept-palettes
  "Body: {:entries [{:key :ns/name :entry {...}} ...]
          :path P?}
   Registers each palette. Optionally syncs the registry to a file."
  [req]
  (let [{:keys [entries path]} (server/read-body req)
        results (mapv (fn [{:keys [key entry]}]
                        (try
                          (let [hexes   (:hex entry)
                                weights (:weights entry)]
                            (main/register-palette!
                             key hexes
                             (cond-> {}
                               (seq weights) (assoc :weights weights))))
                          {:key key :ok true}
                          (catch Exception e
                            {:key key :ok false
                             :error (ex-message e)})))
                      entries)
        save-result (when path (main/save-registry! path))]
    (server/edn-response
     (cond-> {:accepted (count (filter :ok results))
              :failed   (filterv (complement :ok) results)}
       save-result (assoc :saved save-result)))))

;; Routes

(defn register-routes! []
  ;; Load existing entries
  (server/register-route! :post "/api/proposal/association/load-entry"
                          #(server/safely handle-load-association-entry %))
  (server/register-route! :post "/api/proposal/palette/load-entry"
                          #(server/safely handle-load-palette-entry %))

  (server/register-route! :post "/api/proposal/brainstorm"
                          #(server/safely handle-batch-brainstorm %))

  ;; Association proposal
  (server/register-route! :post "/api/proposal/association/propose"
                          #(server/safely handle-propose-association %))
  (server/register-route! :post "/api/proposal/association/refine"
                          #(server/safely handle-refine-association %))
  (server/register-route! :post "/api/proposal/association/accept"
                          #(server/safely handle-accept-association %))
  (server/register-route! :post "/api/proposal/association/discard"
                          #(server/safely handle-discard-association %))
  (server/register-route! :get  "/api/proposal/association/draft"
                          handle-current-association-draft)

 ;; Palette proposal
  (server/register-route! :post "/api/proposal/palette/propose"
                          #(server/safely handle-propose-palette %))
  (server/register-route! :post "/api/proposal/palette/refine"
                          #(server/safely handle-refine-palette %))
  (server/register-route! :post "/api/proposal/palette/accept"
                          #(server/safely handle-accept-palette %))
  (server/register-route! :post "/api/proposal/palette/discard"
                          #(server/safely handle-discard-palette %))
  (server/register-route! :get  "/api/proposal/palette/draft"
                          handle-current-palette-draft)

  ;; Modify existing
  (server/register-route! :post "/api/proposal/association/load"
                          #(server/safely handle-load-association-for-modify %))
  (server/register-route! :post "/api/proposal/palette/load"
                          #(server/safely handle-load-palette-for-modify %))

  ;; Add to register-routes!:
  (server/register-route! :post "/api/proposal/association/batch"
                          #(server/safely handle-batch-propose-associations %))
  (server/register-route! :post "/api/proposal/association/batch-propose"
                          #(server/safely handle-batch-propose-associations %))
  (server/register-route! :post "/api/proposal/association/batch-accept"
                          #(server/safely handle-batch-accept-associations %))
  (server/register-route! :post "/api/proposal/palette/batch"
                          #(server/safely handle-batch-propose-palettes %))
  (server/register-route! :post "/api/proposal/palette/batch-accept"
                          #(server/safely handle-batch-accept-palettes %)))