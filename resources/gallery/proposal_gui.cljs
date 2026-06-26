(ns proposal-gui
  "Proposal builder: drives clj-colors.llm.associative and
   clj-colors.llm.palettes from the browser. Three subtabs:
   - Association: propose new associations with optional context
   - Palette: propose new palettes with optional context + seed colors
   - Modify Existing: load an association or palette and refine it"
  (:require [reagent.core :as r]
            [clojure.edn :as edn]
            [clojure.set]
            [clojure.string :as str]))

;; --- State ---

(defonce sub-tab (r/atom :association))
(defonce modify-kind (r/atom :association))
(defonce modify-suggestions-open? (r/atom false))

(defonce proposal-state
  (r/atom {:referent     ""
           :context      ""
           :seed-input   ""           ; staging area for one color before adding
           :seed-hexes   []           ; confirmed list of seed colors
           :draft        nil
           :draft-kind   nil          ; :association or :palette
           :log          []
           :pending?     false
           :accept-key   ""
           :refine-text  ""
           :modify-key-input ""}))    ; what the user typed into modify-load

(defonce batch-state
  (r/atom {:mode        :explicit       ; :explicit or :category
           :namespace   ""              ; explicit-mode namespace
           :extra-context ""            ; shared optional context
           :entries     [{:referent ""
                          :context ""
                          :seed-input ""
                          :bulk-input ""
                          :seed-hexes []}]
           :category-counts [{:category "" :count 3}]
           :drafts      []              ; results
           :keep-set    #{}             ; indices of drafts to keep
           :pending?    false
           :modify-set #{}            ; indices whose modify checkbox is checked
           :modify-feedback {}        ; index -> feedback string
           :refining-indices #{}}))      ; indices currently being refined (in-flight)

(defn- blank-entry []
  {:referent "" :context "" :seed-input "" :seed-hexes [] :namespace nil})

(defn- blank-category []
  {:category "" :count 3})

;; --- Helpers ---

(defn log-entry! [entry]
  (swap! proposal-state update :log conj
         (assoc entry :timestamp (.toISOString (js/Date.)))))

(defn- normalize-hex
  "Take any reasonable representation (#rrggbb, rrggbb, rgb(r,g,b),
   r,g,b) and convert to a normalized uppercase #RRGGBB. Returns nil
   if the input can't be parsed as a color."
  [s]
  (let [trimmed (str/trim s)]
    (cond
      (re-matches #"#?[0-9a-fA-F]{6}" trimmed)
      (str "#" (str/upper-case (str/replace trimmed #"^#" "")))

      (re-matches #"#?[0-9a-fA-F]{3}" trimmed)
      (let [h (str/replace trimmed #"^#" "")]
        (str "#" (str/upper-case
                  (apply str (mapcat (fn [c] [c c]) h)))))

      :else
      (let [nums (re-seq #"\d+" trimmed)]
        (when (= 3 (count nums))
          (let [[r g b] (map #(js/parseInt % 10) nums)]
            (when (and (every? #(<= 0 % 255) [r g b]))
              (gallery/rgb->hex r g b))))))))

(defn- toggle-modify! [idx]
  (swap! batch-state update :modify-set
         (fn [s] (if (contains? s idx) (disj s idx) (conj s idx)))))

(defn- set-modify-feedback! [idx text]
  (swap! batch-state assoc-in [:modify-feedback idx] text))

(defn- refine-card!
  "Apply the modify feedback for a single card. The refine endpoint
   uses the draft atom on the server side, so we need to LOAD the
   batch entry as the current draft first, then refine, then capture
   the result back into the batch :drafts vector."
  [kind idx]
  (let [{:keys [drafts modify-feedback]} @batch-state
        feedback (get modify-feedback idx)
        draft (get drafts idx)
        entry (:entry draft)]
    (when (and feedback (not (str/blank? feedback)) entry)
      (swap! batch-state update :refining-indices conj idx)
      (let [load-endpoint (case kind
                            :association "/api/proposal/association/load-entry"
                            :palette     "/api/proposal/palette/load-entry")
            refine-endpoint (case kind
                              :association "/api/proposal/association/refine"
                              :palette     "/api/proposal/palette/refine")]
        ;; Stage the existing entry as the server-side draft, then refine.
        (gallery/post-edn! load-endpoint
                           {:entry entry
                            :referent (:referent draft)}
                           (fn [load-resp]
                             (if (:error load-resp)
                               (do (swap! batch-state update :refining-indices disj idx)
                                   (reset! gallery/status
                                           (str "modify failed: " (:error load-resp))))
                               (gallery/post-edn! refine-endpoint
                                                  {:feedback feedback}
                                                  (fn [refine-resp]
                                                    (swap! batch-state update :refining-indices disj idx)
                                                    (if (:error refine-resp)
                                                      (reset! gallery/status
                                                              (str "refine failed: " (:error refine-resp)))
                                                      (let [new-entry (:entry refine-resp)]
                                                        (swap! batch-state
                                                               (fn [s]
                                                                 (-> s
                                                                     (assoc-in [:drafts idx :entry] new-entry)
                                                                     (assoc-in [:drafts idx :usage]
                                                                               (:usage (:draft refine-resp)))
                                                                     (update :modify-feedback dissoc idx)
                                                                     (update :modify-set disj idx))))
                                                        (reset! gallery/status
                                                                (str "refined entry #" (inc idx))))))))))))))

;; --- Operations: association ---

(defn propose-association! []
  (let [{:keys [referent context]} @proposal-state]
    (when (str/blank? referent)
      (reset! gallery/status "referent is required")
      (throw (js/Error. "referent required")))
    (swap! proposal-state assoc :pending? true)
    (log-entry! {:type :request :kind :association :label "propose"
                 :referent referent
                 :context (when-not (str/blank? context) context)})
    (gallery/post-edn! "/api/proposal/association/propose"
                       {:referent referent
                        :context  (when-not (str/blank? context) context)}
                       (fn [resp]
                         (swap! proposal-state assoc :pending? false)
                         (if (:error resp)
                           (do (log-entry! {:type :error :message (:error resp)})
                               (reset! gallery/status (str "error: " (:error resp))))
                           (do (swap! proposal-state assoc
                                      :draft (:draft resp)
                                      :draft-kind :association
                                      :accept-key (str (or (:referent (:draft resp))
                                                           referent)))
                               (log-entry! {:type :response :kind :association
                                            :label "proposed"
                                            :entry (:entry resp)
                                            :usage (:usage (:draft resp))})
                               (reset! gallery/status
                                       (str "proposed for: " referent))))))))

;; --- Operations: palette ---

(defn propose-palette! []
  (let [{:keys [referent context seed-hexes]} @proposal-state]
    (when (str/blank? referent)
      (reset! gallery/status "referent is required")
      (throw (js/Error. "referent required")))
    (swap! proposal-state assoc :pending? true)
    (log-entry! {:type :request :kind :palette :label "propose"
                 :referent referent
                 :context (when-not (str/blank? context) context)
                 :seed-hexes (when (seq seed-hexes) seed-hexes)})
    (gallery/post-edn! "/api/proposal/palette/propose"
                       {:referent referent
                        :context  (when-not (str/blank? context) context)
                        :seed-hexes (when (seq seed-hexes) seed-hexes)}
                       (fn [resp]
                         (swap! proposal-state assoc :pending? false)
                         (if (:error resp)
                           (do (log-entry! {:type :error :message (:error resp)})
                               (reset! gallery/status (str "error: " (:error resp))))
                           (do (swap! proposal-state assoc
                                      :draft (:draft resp)
                                      :draft-kind :palette
                                      :accept-key (str "studio/" referent))
                               (log-entry! {:type :response :kind :palette
                                            :label "proposed"
                                            :entry (:entry resp)
                                            :usage (:usage (:draft resp))})
                               (reset! gallery/status
                                       (str "proposed palette for: " referent))))))))

(defn add-seed-hex!
  "Validate and add a hex/rgb input to the seed-hexes list."
  []
  (let [{:keys [seed-input seed-hexes]} @proposal-state
        hex (normalize-hex seed-input)]
    (if hex
      (do
        (swap! proposal-state assoc
               :seed-hexes (vec (distinct (conj seed-hexes hex)))
               :seed-input "")
        (reset! gallery/status (str "added seed " hex)))
      (reset! gallery/status (str "unrecognized color: " seed-input)))))

(defn remove-seed-hex! [hex]
  (swap! proposal-state update :seed-hexes
         (fn [hexes] (vec (remove #(= hex %) hexes)))))

;; --- Operations: refine/accept/discard (shared) ---

(defn refine! []
  (let [{:keys [refine-text draft-kind]} @proposal-state
        endpoint (case draft-kind
                   :association "/api/proposal/association/refine"
                   :palette     "/api/proposal/palette/refine")]
    (when (str/blank? refine-text)
      (reset! gallery/status "refinement feedback is required")
      (throw (js/Error. "feedback required")))
    (swap! proposal-state assoc :pending? true)
    (log-entry! {:type :request :kind draft-kind :label "refine"
                 :feedback refine-text})
    (gallery/post-edn! endpoint
                       {:feedback refine-text}
                       (fn [resp]
                         (swap! proposal-state assoc :pending? false)
                         (if (:error resp)
                           (do (log-entry! {:type :error :message (:error resp)})
                               (reset! gallery/status (str "error: " (:error resp))))
                           (do (swap! proposal-state assoc
                                      :draft (:draft resp)
                                      :refine-text "")
                               (log-entry! {:type :response :kind draft-kind
                                            :label "refined"
                                            :entry (:entry resp)
                                            :usage (:usage (:draft resp))})
                               (reset! gallery/status "refined")))))))

(defn accept! []
  (let [{:keys [accept-key draft-kind]} @proposal-state
        endpoint (case draft-kind
                   :association "/api/proposal/association/accept"
                   :palette     "/api/proposal/palette/accept")]
    (when (str/blank? accept-key)
      (reset! gallery/status "key is required")
      (throw (js/Error. "key required")))
    (swap! proposal-state assoc :pending? true)
    (let [k (keyword (str/replace accept-key #"^:" ""))
          body (case draft-kind
                 :association {:key k}
                 :palette     {:key k :path "palettes.edn"})]
      (log-entry! {:type :request :kind draft-kind :label "accept" :key k})
      (gallery/post-edn! endpoint body
                         (fn [resp]
                           (swap! proposal-state assoc :pending? false)
                           (if (:error resp)
                             (do (log-entry! {:type :error :message (:error resp)})
                                 (reset! gallery/status (str "error: " (:error resp))))
                             (do (swap! proposal-state assoc :draft nil :draft-kind nil)
                                 (log-entry! {:type :response :kind draft-kind
                                              :label "accepted"
                                              :key (:accepted resp)})
                                 (reset! gallery/status (str "accepted " (:accepted resp)))
                                 (case draft-kind
                                   :association (when (resolve 'association-manager/fetch!)
                                                  ((resolve 'association-manager/fetch!)))
                                   :palette (when (resolve 'palette-manager/fetch-palettes!)
                                              ((resolve 'palette-manager/fetch-palettes!)))))))))))

(defn discard! []
  (let [{:keys [draft-kind]} @proposal-state
        endpoint (case draft-kind
                   :association "/api/proposal/association/discard"
                   :palette     "/api/proposal/palette/discard")]
    (gallery/post-edn! endpoint {}
                       (fn [_]
                         (swap! proposal-state assoc :draft nil :draft-kind nil)
                         (log-entry! {:type :response :label "discarded"})
                         (reset! gallery/status "discarded")))))

;; --- Operations: modify existing ---

(defn load-for-modify! [kind key-str]
  (when (str/blank? key-str)
    (reset! gallery/status "key is required")
    (throw (js/Error. "key required")))
  (let [k (keyword (str/replace key-str #"^:" ""))
        endpoint (case kind
                   :association "/api/proposal/association/load"
                   :palette     "/api/proposal/palette/load")]
    (swap! proposal-state assoc :pending? true)
    (log-entry! {:type :request :kind kind :label "load" :key k})
    (gallery/post-edn! endpoint {:key k}
                       (fn [resp]
                         (swap! proposal-state assoc :pending? false)
                         (if (:error resp)
                           (do (log-entry! {:type :error :message (:error resp)})
                               (reset! gallery/status (str "error: " (:error resp))))
                           (do (swap! proposal-state assoc
                                      :draft (:draft resp)
                                      :draft-kind kind
                                      :accept-key key-str)
                               (log-entry! {:type :response :kind kind
                                            :label "loaded"
                                            :entry (:entry resp)})
                               (reset! gallery/status (str "loaded " (:loaded resp)))))))))

(defn- parse-bulk-input
  "Parse a bulk-paste string into a vector of
   {:referent :context :category} maps.

   Accepts FIVE formats, tried in order from most structured to least:

   1. Vector of category-grouped maps:
        [{:category :ocean :referents [\"coral\" \"reef\"]
          :contexts {\"coral\" \"the marine animal\"}}
         {:category :forest :referents [\"oak\" \"pine\"]}]

   2. Vector of {:namespace :referents [...] :contexts {}} maps with
      :referents and per-name :contexts (authoring-batches shape).

   3. A single map of referent->context strings:
        {\"coral-reef\" \"warm tropical\"
         \"kelp-forest\" \"cold pacific\"}

   4. A single map with a :category key plus referent->context pairs:
        {:category :ocean
         \"coral-reef\" \"warm tropical\"
         \"kelp-forest\" \"cold pacific\"}

   5. Plain delimited string:
        \"coral-reef, kelp-forest, abyssal-trench\"

   Returns nil if nothing parseable was found."
  [s]
  (when (and (string? s) (not (str/blank? s)))
    (let [trimmed (str/trim s)
          parsed (try
                   (edn/read-string trimmed)
                   (catch :default _ nil))]
      (cond
        ;; Format 1+2: vector of category maps
        (and (vector? parsed)
             (every? map? parsed)
             (every? (fn [m] (and (or (:category m) (:namespace m))
                                  (:referents m))) parsed))
        (vec
         (for [{:keys [category namespace referents contexts]} parsed
               r referents]
           {:referent r
            :context  (get contexts r)
            :category (when-let [c (or category namespace)]
                        (cond
                          (keyword? c) (name c)
                          :else (str c)))}))

        ;; Format 4: map with :category and referent->context pairs
        (and (map? parsed)
             (or (:category parsed) (:namespace parsed)))
        (let [cat (or (:category parsed) (:namespace parsed))
              cat-str (cond
                        (keyword? cat) (name cat)
                        :else (str cat))
              pairs (dissoc parsed :category :namespace)]
          (vec
           (for [[k v] pairs
                 :when (string? k)]
             {:referent k :context v :category cat-str})))

        ;; Format 3: map of referent->context
        (and (map? parsed)
             (every? string? (keys parsed)))
        (vec
         (for [[k v] parsed]
           {:referent k :context v :category nil}))

        ;; Format 5: plain delimited string — split on any whitespace,
        ;; comma, or semicolon (one or more in a row)
        :else
        (let [names (->> (str/split trimmed #"[,;\s]+")
                         (map str/trim)
                         (remove str/blank?))]
          (when (seq names)
            (mapv (fn [r] {:referent r :context nil :category nil}) names)))))))

(defn- bulk-add-entries! []
  (let [raw    (:bulk-input @batch-state)
        parsed (parse-bulk-input raw)]
    (if (seq parsed)
      (let [uses-per-entry-cats? (every? :category parsed)
            shared-category (when uses-per-entry-cats?
                              (let [cats (set (map :category parsed))]
                                (when (= 1 (count cats))
                                  (first cats))))
            current-namespace (:namespace @batch-state)
            new-entries
            (mapv (fn [{:keys [referent context category]}]
                    (-> (blank-entry)
                        (assoc :referent referent)
                        (assoc :context (or context ""))
                        (assoc :namespace
                               (or category
                                   (when-not uses-per-entry-cats?
                                     current-namespace)))))
                  parsed)]
        (swap! batch-state
               (fn [s]
                 (let [existing (:entries s)
                       trimmed (if (and (= 1 (count existing))
                                        (str/blank? (:referent (first existing))))
                                 []
                                 existing)]
                   (cond-> (-> s
                               (assoc :entries (vec (concat trimmed new-entries)))
                               (assoc :bulk-input "")
                               (assoc :namespace-error nil))
                     shared-category (assoc :namespace shared-category)))))
        (reset! gallery/status
                (str "imported " (count parsed) " entries"
                     (when uses-per-entry-cats?
                       " with per-entry categories"))))
      (reset! gallery/status
              "couldn't parse — check your EDN syntax"))))

(defn- usage-cost-cents
  "Approximate cost in cents for a single Sonnet 4.6 call.
   $3/MTok input, $15/MTok output."
  [usage]
  (let [in  (or (:input_tokens usage) 0)
        out (or (:output_tokens usage) 0)]
    (* 100.0 (+ (* in 0.000003) (* out 0.000015)))))

(defn- entry-key-from
  "Build a :ns/referent keyword from a namespace string and referent."
  [ns-str referent]
  (let [slug (-> referent
                 (str/trim)
                 (str/lower-case)
                 (str/replace #"\s+" "-")
                 (str/replace #"[^a-z0-9-]" ""))]
    (keyword (str/replace ns-str #"^:" "") slug)))

(defn- plan-explicit-entries
  "From explicit-mode entries, build planned entries with proposed keys.
   Respects per-entry namespace if present, falls back to form namespace."
  [form-namespace entries]
  (mapv (fn [{:keys [referent context seed-hexes namespace]}]
          {:proposed-key (entry-key-from (or namespace form-namespace)
                                         referent)
           :referent     referent
           :context      context
           :seed-hexes   seed-hexes})
        entries))

;; --- Components ---

(defn sub-tabs []
  [:div.sg-tabs.sg-subtabs
   [:button.sg-tab
    {:class (when (= :association @sub-tab) "active")
     :on-click (fn [_]
                 (swap! proposal-state assoc :draft nil :draft-kind nil)
                 (reset! sub-tab :association))}
    "Association"]
   [:button.sg-tab
    {:class (when (= :palette @sub-tab) "active")
     :on-click (fn [_]
                 (swap! proposal-state assoc :draft nil :draft-kind nil)
                 (reset! sub-tab :palette))}
    "Palette"]
   [:button.sg-tab
    {:class (when (= :modify @sub-tab) "active")
     :on-click (fn [_]
                 (swap! proposal-state assoc :draft nil :draft-kind nil)
                 (reset! sub-tab :modify))}
    "Modify existing"]
   [:button.sg-tab
    {:class (when (= :batch-association @sub-tab) "active")
     :on-click (fn [_]
                 (reset! sub-tab :batch-association))}
    "Batch associations"]
   [:button.sg-tab
    {:class (when (= :batch-palette @sub-tab) "active")
     :on-click (fn [_]
                 (reset! sub-tab :batch-palette))}
    "Batch palettes"]])

(defn seed-color-input
  "Input + add button, with the current seed list rendered as
   swatches. Validates hex or rgb on add."
  []
  (let [{:keys [seed-input seed-hexes pending?]} @proposal-state]
    [:div.sg-seed-section
     [:label "seed colors (optional) "
      [:input {:type "text" :size 22 :value seed-input
               :placeholder "e.g. #5881d8 or 88,193,71"
               :disabled pending?
               :on-change (fn [e] (swap! proposal-state assoc :seed-input
                                         (gallery/target-value e)))
               :on-key-down (fn [e]
                              (when (and (= "Enter" (.-key e)) (not pending?))
                                (add-seed-hex!)))}]
      [:input {:type "color"
               :value (or (normalize-hex seed-input) "#808080")
               :disabled pending?
               :on-change (fn [e]
                            (swap! proposal-state assoc :seed-input
                                   (gallery/target-value e)))}]
      [:button {:on-click add-seed-hex! :disabled pending?}
       "+ add"]]
     (when (seq seed-hexes)
       [:div.sg-seed-swatches
        (for [hex seed-hexes]
          ^{:key hex}
          [:span.sg-seed-swatch-item
           [:span.sg-seed-swatch {:style {:background hex}}]
           [:span.sg-seed-swatch-hex hex]
           [:button.sg-seed-swatch-remove
            {:on-click (fn [_] (remove-seed-hex! hex))
             :title "remove"} "x"]])])]))

(defn association-propose-form []
  (let [{:keys [referent context pending?]} @proposal-state]
    [:div.sg-propose-form
     [:h3 "Generate an association"]
     [:div.sg-tools
      [:label "referent "
       [:input {:type "text" :size 30 :value referent
                :placeholder "e.g. obsidian, rick, golden-hour"
                :disabled pending?
                :on-change (fn [e] (swap! proposal-state assoc :referent
                                          (gallery/target-value e)))
                :on-key-down (fn [e]
                               (when (and (= "Enter" (.-key e))
                                          (not pending?))
                                 (propose-association!)))}]]]
     [:div.sg-tools
      [:label "extra context (optional) "
       [:input {:type "text" :size 60 :value context
                :placeholder "e.g. the character from Rick and Morty"
                :disabled pending?
                :on-change (fn [e] (swap! proposal-state assoc :context
                                          (gallery/target-value e)))}]]]
     [:div.sg-tools
      [:button {:on-click propose-association! :disabled pending?}
       (if pending? "proposing..." "Propose")]]]))

(defn- bulk-paste-bar []
  (let [{:keys [bulk-input pending?]} @batch-state]
    [:div.sg-batch-bulk
     [:div.sg-batch-bulk-head
      [:span.sg-batch-bulk-title "Bulk import"]
      [:span.sg-batch-bulk-hint
       "comma-list, {\"name\" \"ctx\" ...}, or [{:category :x :referents [...]} ...]"]]
     [:textarea.sg-batch-bulk-input
      {:rows 3
       :value (or bulk-input "")
       :placeholder
       "coral-reef, kelp-forest, abyssal-trench
or {\"coral-reef\" \"warm tropical\" \"kelp-forest\" \"cold pacific\"}
or [{:category :ocean :referents [\"coral\" \"reef\"]}]"
       :disabled pending?
       :on-change (fn [e]
                    (swap! batch-state assoc :bulk-input
                           (gallery/target-value e)))
       :on-key-down (fn [e]
                      ;; Ctrl+Enter to import (regular Enter inserts newline)
                      (when (and (= "Enter" (.-key e))
                                 (.-ctrlKey e)
                                 (not pending?))
                        (bulk-add-entries!)))}]
     [:div.sg-batch-bulk-actions
      [:button {:on-click bulk-add-entries! :disabled pending?}
       "Import entries"]
      [:span.sg-batch-bulk-shortcut "Ctrl+Enter"]]]))

(defn palette-propose-form []
  (let [{:keys [referent context pending?]} @proposal-state]
    [:div.sg-propose-form
     [:h3 "Generate a palette"]
     [:div.sg-tools
      [:label "referent "
       [:input {:type "text" :size 30 :value referent
                :placeholder "e.g. autumn-forest, deep-sea, cyberpunk"
                :disabled pending?
                :on-change (fn [e] (swap! proposal-state assoc :referent
                                          (gallery/target-value e)))
                :on-key-down (fn [e]
                               (when (and (= "Enter" (.-key e))
                                          (not pending?))
                                 (propose-palette!)))}]]]
     [:div.sg-tools
      [:label "extra context (optional) "
       [:input {:type "text" :size 60 :value context
                :placeholder "e.g. warm and inviting, after sunset"
                :disabled pending?
                :on-change (fn [e] (swap! proposal-state assoc :context
                                          (gallery/target-value e)))}]]]
     [seed-color-input]
     [:div.sg-tools
      [:button {:on-click propose-palette! :disabled pending?}
       (if pending? "proposing..." "Propose")]]]))

(defn- modify-candidates
  "Return all candidate keys (as strings) for the current modify kind.
   Reads from the manager namespaces' state atoms."
  [kind]
  (case kind
    :association
    (when-let [data-atom (resolve 'association-manager/associations)]
      (->> @@data-atom
           (map (comp str :key))
           sort))
    :palette
    (when-let [data-atom (resolve 'palette-manager/palettes)]
      (->> @@data-atom
           (map (comp str :key))
           sort))
    nil))

(defn- filtered-candidates
  "Candidates that match the input substring (case-insensitive)."
  [kind input]
  (let [all (modify-candidates kind)
        q   (str/lower-case (or input ""))]
    (if (str/blank? q)
      (take 30 all)
      (->> all
           (filter (fn [s] (str/includes? (str/lower-case s) q)))
           (take 30)))))

(defn modify-existing-form []
  (let [{:keys [modify-key-input pending?]} @proposal-state
        kind @modify-kind
        suggestions (when @modify-suggestions-open?
                      (filtered-candidates kind modify-key-input))]
    [:div.sg-propose-form
     [:h3 "Modify an existing entry"]
     [:div.sg-tools
      [:span.sg-modify-kind
       [:button.sg-modify-kind-btn
        {:class (when (= :association kind) "active")
         :on-click (fn [_]
                     (reset! modify-kind :association)
                     (swap! proposal-state assoc :modify-key-input ""))}
        "Association"]
       [:button.sg-modify-kind-btn
        {:class (when (= :palette kind) "active")
         :on-click (fn [_]
                     (reset! modify-kind :palette)
                     (swap! proposal-state assoc :modify-key-input ""))}
        "Palette"]]]
     [:div.sg-tools.sg-modify-row
      [:label "key "
       [:span.sg-autocomplete
        [:input {:type "text" :size 40 :value modify-key-input
                 :placeholder (case kind
                                :association "e.g. mineral/obsidian"
                                :palette     "e.g. ocean/storm-sea")
                 :disabled pending?
                 :on-focus (fn [_] (reset! modify-suggestions-open? true))
                 :on-blur (fn [_]
                            ;; Delay so click on suggestion fires first
                            (js/setTimeout
                             #(reset! modify-suggestions-open? false)
                             150))
                 :on-change (fn [e]
                              (swap! proposal-state assoc
                                     :modify-key-input
                                     (gallery/target-value e))
                              (reset! modify-suggestions-open? true))
                 :on-key-down (fn [e]
                                (when (and (= "Enter" (.-key e)) (not pending?))
                                  (reset! modify-suggestions-open? false)
                                  (load-for-modify! kind modify-key-input)))}]
        (when (and (seq suggestions) @modify-suggestions-open?)
          [:ul.sg-autocomplete-list
           (for [s suggestions]
             ^{:key s}
             [:li.sg-autocomplete-item
              {:on-mouse-down
               (fn [_]
                 (swap! proposal-state assoc :modify-key-input s)
                 (reset! modify-suggestions-open? false))}
              s])])]
       [:button {:on-click (fn [_]
                             (reset! modify-suggestions-open? false)
                             (load-for-modify! kind modify-key-input))
                 :disabled pending?}
        "Load"]]]]))

(defn association-meta-display [entry]
  (let [sorted-tags (->> (:tags entry)
                         (sort-by (fn [[_ d]] (- (or (:weight d) 0)))))]
    [:div
     (when (:rationale entry)
       [:div.sg-draft-rationale
        [:h4 "Rationale"]
        [:p (:rationale entry)]])
     (when (seq sorted-tags)
       [:div.sg-draft-tags
        [:h4 (str "Tags (" (count sorted-tags) ")")]
        [:ul.sg-draft-tag-list
         (for [[k {:keys [weight specificity]}] sorted-tags]
           ^{:key k}
           [:li.sg-draft-tag-row
            [:span.sg-draft-tag-name (str k)]
            [:span.sg-draft-tag-meta
             (str "w=" (.toFixed (or weight 0) 2)
                  "  s=" (.toFixed (or specificity 1.0) 2))]])]])]))

(defn palette-meta-display [entry]
  [:div
   (when (seq (:tags entry))
     [:div.sg-draft-tags
      [:h4 (str "Tags (" (count (:tags entry)) ")")]
      [:ul.sg-draft-tag-list
       (for [t (sort (:tags entry))]
         ^{:key t}
         [:li.sg-draft-tag-row
          [:span.sg-draft-tag-name (str t)]])]])])

(defn draft-meta-panel []
  (let [{:keys [draft draft-kind pending? accept-key refine-text]} @proposal-state]
    (when draft
      (let [entry (:entry draft)]
        [:div.sg-draft-panel
         [:div.sg-draft-head
          [:h3 (str "Draft for: " (or (:referent draft)
                                      (str (:key draft))
                                      "(no referent)"))]
          [:span.sg-draft-kind (name draft-kind)]
          (when-let [c (:category entry)]
            [:span.sg-draft-category (name c)])
          (when-let [s (:sigma entry)]
            [:span.sg-draft-sigma (str "σ=" (.toFixed s 3))])]

         (case draft-kind
           :association [association-meta-display entry]
           :palette     [palette-meta-display entry]
           nil)

         [:div.sg-tools
          [:label "save as "
           [:input {:type "text" :size 30 :value accept-key
                    :placeholder (case draft-kind
                                   :association "e.g. mineral/obsidian"
                                   :palette     "e.g. studio/sunset"
                                   "")
                    :on-change (fn [e] (swap! proposal-state assoc :accept-key
                                              (gallery/target-value e)))}]]
          [:button {:on-click accept! :disabled pending?} "Accept"]
          [:button {:on-click discard! :disabled pending?} "Discard"]]

         [:div.sg-tools
          [:label "refine "
           [:input {:type "text" :size 60 :value refine-text
                    :placeholder "describe what to change"
                    :on-change (fn [e] (swap! proposal-state assoc :refine-text
                                              (gallery/target-value e)))
                    :on-key-down (fn [e]
                                   (when (and (= "Enter" (.-key e)) (not pending?))
                                     (refine!)))}]]
          [:button {:on-click refine! :disabled pending?}
           (if pending? "refining..." "Refine")]]]))))

(defn color-preview-panel
  "Right-half panel. For associations: weighted colors map.
   For palettes: hex vector with parallel weights."
  []
  (let [{:keys [draft draft-kind]} @proposal-state
        entry (:entry draft)
        colors (case draft-kind
                 :association
                 (when (and entry (map? (:colors entry)))
                   (->> (:colors entry)
                        (sort-by (comp - second))
                        (mapv (fn [[hex w]]
                                {:hex   (if (str/starts-with? hex "#") hex (str "#" hex))
                                 :weight w
                                 :alpha 1}))))
                 :palette
                 (when (and entry (vector? (:hex entry)))
                   (let [hexes   (:hex entry)
                         weights (or (:weights entry)
                                     (vec (repeat (count hexes) (/ 1.0 (count hexes)))))]
                     (mapv (fn [h w]
                             {:hex (if (str/starts-with? h "#") h (str "#" h))
                              :weight w
                              :alpha 1})
                           hexes weights)))
                 nil)]
    [:div.sg-proposal-colors
     [:h3 "Colors"]
     (if (seq colors)
       [:div
        [:div.sg-proposal-gradient
         {:style {:background (gallery/gradient-css colors "to right")}}]
        [:ul.sg-proposal-color-list
         (for [{:keys [hex weight]} colors]
           ^{:key hex}
           [:li.sg-proposal-color-row
            [:span.sg-proposal-swatch {:style {:background hex}}]
            [:span.sg-proposal-hex hex]
            [:span.sg-proposal-weight (.toFixed (or weight 0) 3)]])]]
       [:p.sg-proposal-empty
        "No colors yet. Propose to see them here."])]))

(defn log-entry-view [entry]
  (case (:type entry)
    :request
    [:div.sg-log-entry.sg-log-request
     [:span.sg-log-label "> " (:label entry)]
     (when (:kind entry)
       [:span.sg-log-detail (str " [" (name (:kind entry)) "]")])
     (when (:referent entry)
       [:span.sg-log-detail (str " " (:referent entry))])
     (when (:context entry)
       [:span.sg-log-detail (str " (context: " (:context entry) ")")])
     (when (:seed-hexes entry)
       [:span.sg-log-detail (str " seeds: " (str/join "," (:seed-hexes entry)))])
     (when (:feedback entry)
       [:span.sg-log-detail (str " \"" (:feedback entry) "\"")])
     (when (:key entry)
       [:span.sg-log-detail (str " " (:key entry))])]

    :response
    [:div.sg-log-entry.sg-log-response
     [:span.sg-log-label "✓ " (:label entry)]
     (when (:kind entry)
       [:span.sg-log-detail (str " [" (name (:kind entry)) "]")])
     (when (:key entry)
       [:span.sg-log-detail (str " " (:key entry))])
     (when (:usage entry)
       [:span.sg-log-detail
        (str " (in " (:input_tokens (:usage entry))
             " / out " (:output_tokens (:usage entry)) ")")])]

    :error
    [:div.sg-log-entry.sg-log-error
     [:span.sg-log-label "✗ "]
     [:span.sg-log-detail (:message entry)]]

    [:div.sg-log-entry (pr-str entry)]))

(defn transcript-panel []
  (let [log (:log @proposal-state)]
    [:div.sg-proposal-transcript
     [:h3 "Transcript"]
     (if (seq log)
       (for [[i entry] (map-indexed vector log)]
         ^{:key i} [log-entry-view entry])
       [:p.sg-proposal-empty
        "No activity yet."])]))

;; --- Batch form components ---

(defn- update-entry! [idx k v]
  (swap! batch-state update-in [:entries idx] assoc k v))

(defn- add-entry! []
  (swap! batch-state update :entries conj (blank-entry)))

(defn- remove-entry! [idx]
  (swap! batch-state update :entries
         (fn [es] (vec (concat (subvec es 0 idx) (subvec es (inc idx)))))))

(defn- add-seed-to-entry! [idx]
  (let [{:keys [seed-input seed-hexes]} (get-in @batch-state [:entries idx])
        hex (normalize-hex seed-input)]
    (if hex
      (swap! batch-state update-in [:entries idx]
             (fn [e] (-> e
                         (assoc :seed-hexes
                                (vec (distinct (conj seed-hexes hex))))
                         (assoc :seed-input ""))))
      (reset! gallery/status (str "unrecognized color: " seed-input)))))

(defn- remove-seed-from-entry! [idx hex]
  (swap! batch-state update-in [:entries idx :seed-hexes]
         (fn [hexes] (vec (remove #(= hex %) hexes)))))

(defn- update-category! [idx k v]
  (swap! batch-state update-in [:category-counts idx] assoc k v))

(defn- add-category! []
  (swap! batch-state update :category-counts conj (blank-category)))

(defn- remove-category! [idx]
  (swap! batch-state update :category-counts
         (fn [cs] (vec (concat (subvec cs 0 idx) (subvec cs (inc idx)))))))

(defn- mode-toggle []
  (let [mode (:mode @batch-state)]
    [:div.sg-tools
     [:label.sg-checkbox-label
      [:input {:type "checkbox"
               :checked (= mode :category)
               :on-change (fn [_]
                            (swap! batch-state assoc :mode
                                   (if (= mode :category) :explicit :category)))}]
      " category-based generation"]]))

(defn- explicit-entry-row [kind idx entry]
  (let [{:keys [referent context seed-input seed-hexes namespace]} entry]
    [:div.sg-batch-entry
     [:div.sg-batch-entry-head
      [:span.sg-batch-entry-idx (str "#" (inc idx))]
      (when-not (str/blank? namespace)
        [:span.sg-batch-entry-ns (str ":" namespace "/")])
      [:button.sg-batch-entry-remove
       {:on-click (fn [_] (remove-entry! idx))
        :title "remove this entry"} "x"]]
     [:div.sg-tools
      [:label "referent "
       [:input {:type "text" :size 22 :value referent
                :placeholder "e.g. coral-reef"
                :on-change (fn [e] (update-entry! idx :referent
                                                  (gallery/target-value e)))}]]
      [:label "context (optional) "
       [:input {:type "text" :size 32 :value context
                :placeholder "disambiguating note"
                :on-change (fn [e] (update-entry! idx :context
                                                  (gallery/target-value e)))}]]]
     (when (= kind :palette)
       [:div.sg-tools
        [:label "seed colors "
         [:input {:type "text" :size 16 :value seed-input
                  :placeholder "#hex or rgb"
                  :on-change (fn [e] (update-entry! idx :seed-input
                                                    (gallery/target-value e)))
                  :on-key-down (fn [e]
                                 (when (= "Enter" (.-key e))
                                   (add-seed-to-entry! idx)))}]
         [:input {:type "color"
                  :value (or (normalize-hex seed-input) "#808080")
                  :on-change (fn [e] (update-entry! idx :seed-input
                                                    (gallery/target-value e)))}]
         [:button {:on-click (fn [_] (add-seed-to-entry! idx))} "+"]]
        (when (seq seed-hexes)
          [:span.sg-seed-swatches
           (for [hex seed-hexes]
             ^{:key hex}
             [:span.sg-seed-swatch-item
              [:span.sg-seed-swatch {:style {:background hex}}]
              [:span.sg-seed-swatch-hex hex]
              [:button.sg-seed-swatch-remove
               {:on-click (fn [_] (remove-seed-from-entry! idx hex))} "x"]])])])]))

(defn- explicit-form [kind]
  (let [{:keys [namespace extra-context entries pending? namespace-error]} @batch-state
        ;; Live-computed: does this form currently need a namespace?
        needs-ns? (and (str/blank? namespace)
                       (some (fn [{:keys [referent]}]
                               (not (str/blank? referent)))
                             entries)
                       (not (every? (fn [e] (not (str/blank? (:namespace e))))
                                    entries)))]
    [:div
     [:div.sg-tools
      [:label "namespace "
       [:input {:type "text" :size 16 :value namespace
                :class (when (or needs-ns? namespace-error) "sg-input-error")
                :placeholder "e.g. ocean, forest, mineral"
                :disabled pending?
                :on-change (fn [e]
                             (swap! batch-state assoc
                                    :namespace (gallery/target-value e)
                                    :namespace-error nil))}]]
      [:label "extra context (optional) "
       [:input {:type "text" :size 40 :value extra-context
                :placeholder "applied to every entry"
                :disabled pending?
                :on-change (fn [e] (swap! batch-state assoc :extra-context
                                          (gallery/target-value e)))}]]]
     (when namespace-error
       [:div.sg-validation-error namespace-error])
     [bulk-paste-bar]
     [:div.sg-batch-entries
      (for [[i entry] (map-indexed vector entries)]
        ^{:key i} [explicit-entry-row kind i entry])]
     [:div.sg-tools
      [:button {:on-click add-entry! :disabled pending?}
       "+ add entry"]]]))

(defn- category-row [idx cat]
  (let [{:keys [category count]} cat]
    [:div.sg-batch-entry.sg-batch-cat-row
     [:button.sg-batch-entry-remove
      {:on-click (fn [_] (remove-category! idx))
       :title "remove this category"} "x"]
     [:label "category "
      [:input {:type "text" :size 20 :value category
               :placeholder "e.g. ocean"
               :on-change (fn [e] (update-category! idx :category
                                                    (gallery/target-value e)))}]]
     [:label "count "
      [:input {:type "number" :min 1 :max 20 :value count
               :on-change (fn [e]
                            (let [n (js/parseInt (gallery/target-value e) 10)]
                              (update-category! idx :count
                                                (if (js/isNaN n) 1 n))))}]]]))

(defn- category-form []
  (let [{:keys [extra-context category-counts pending?]} @batch-state]
    [:div
     [:div.sg-tools
      [:label "extra context (optional) "
       [:input {:type "text" :size 40 :value extra-context
                :placeholder "applied to brainstorm + each entry"
                :disabled pending?
                :on-change (fn [e] (swap! batch-state assoc :extra-context
                                          (gallery/target-value e)))}]]]
     [:div.sg-batch-entries
      (for [[i cat] (map-indexed vector category-counts)]
        ^{:key i} [category-row i cat])]
     [:div.sg-tools
      [:button {:on-click add-category! :disabled pending?}
       "+ add category"]]]))

;; --- Batch propose ---

(defn- batch-form-valid?
  [kind]
  (let [{:keys [mode namespace entries category-counts]} @batch-state]
    (case mode
      :explicit (and (seq entries)
                     (every? (fn [{:keys [referent]}]
                               (not (str/blank? referent)))
                             entries)
                     ;; namespace required only if any entry lacks one
                     (or (not (str/blank? namespace))
                         (every? :namespace entries)))
      :category (and (seq category-counts)
                     (every? (fn [{:keys [category count]}]
                               (and (not (str/blank? category))
                                    (pos? count)))
                             category-counts)))))

(defn- propose-next!
  "Process planned[idx] via the single-propose endpoint. Updates the
   draft, cost, and progress. Recurses to idx+1 when done."
  [kind planned idx]
  (if (>= idx (count planned))
    ;; Done
    (do (swap! batch-state assoc :pending? false :current-index nil)
        (reset! gallery/status
                (str "batch complete: " (count planned) " entries")))
    (let [{:keys [referent context seed-hexes proposed-key]} (get planned idx)
          propose-endpoint (case kind
                             :association "/api/proposal/association/propose"
                             :palette     "/api/proposal/palette/propose")
          discard-endpoint (case kind
                             :association "/api/proposal/association/discard"
                             :palette     "/api/proposal/palette/discard")
          body (cond-> {:referent referent}
                 (not (str/blank? context))
                 (assoc :context context)
                 (seq seed-hexes)
                 (assoc :seed-hexes seed-hexes))]
      (swap! batch-state assoc :current-index idx)
      (gallery/post-edn!
       propose-endpoint body
       (fn [resp]
         (if (:error resp)
           (swap! batch-state
                  (fn [s]
                    (-> s
                        (assoc-in [:drafts idx]
                                  {:proposed-key proposed-key
                                   :referent referent
                                   :context context
                                   :error (:error resp)})
                        (update :keep-set disj idx))))
           (let [entry (:entry resp)
                 usage (:usage (:draft resp))
                 cost  (usage-cost-cents usage)]
             (swap! batch-state
                    (fn [s]
                      (-> s
                          (assoc-in [:drafts idx]
                                    {:proposed-key proposed-key
                                     :referent referent
                                     :context context
                                     :seed-hexes seed-hexes
                                     :entry entry
                                     :usage usage})
                          (update :total-cost-cents + cost)
                          (update :keep-set conj idx))))))
         ;; Clear server-side draft, then move on
         (gallery/post-edn! discard-endpoint {}
                            (fn [_]
                              (propose-next! kind planned (inc idx)))))))))

(defn propose-batch! [kind]
  (let [{:keys [mode namespace extra-context entries category-counts]} @batch-state
        needs-ns? (and (= mode :explicit)
                       (str/blank? namespace)
                       (or (empty? entries)
                           (not (every? (fn [e]
                                          (not (str/blank? (:namespace e))))
                                        entries))))]
    (when needs-ns?
      (swap! batch-state assoc :namespace-error
             "Batch generation requires at least one namespace")
      (reset! gallery/status "namespace required")
      (throw (js/Error. "missing namespace")))
    (when-not (batch-form-valid? kind)
      (reset! gallery/status "fill in all required fields")
      (throw (js/Error. "invalid form")))
    ;; Initialize batch state for the run
    (case mode
      :explicit
      (let [planned (plan-explicit-entries namespace entries)
            placeholders (vec (for [p planned]
                                {:proposed-key (:proposed-key p)
                                 :referent (:referent p)
                                 :status :pending}))]
        (swap! batch-state assoc
               :pending? true
               :drafts placeholders
               :planned planned
               :keep-set #{}
               :total-cost-cents 0.0
               :current-index 0)
        (log-entry! {:type :request :kind kind :label "batch-propose"
                     :mode mode :count (count planned)})
        (propose-next! kind planned 0))

      :category
      (do
        (swap! batch-state assoc
               :pending? true
               :drafts []
               :planned []
               :keep-set #{}
               :total-cost-cents 0.0
               :brainstorming? true)
        (log-entry! {:type :request :kind kind :label "brainstorm"
                     :mode mode})
        (gallery/post-edn!
         "/api/proposal/brainstorm"
         {:category-counts category-counts
          :extra-context   (when-not (str/blank? extra-context)
                             extra-context)}
         (fn [resp]
           (if (:error resp)
             (do (swap! batch-state assoc :pending? false :brainstorming? false)
                 (log-entry! {:type :error :message (:error resp)})
                 (reset! gallery/status (str "brainstorm failed: " (:error resp))))
             (let [planned (:entries resp)
                   brainstorm-cost (usage-cost-cents (:brainstorm-usage resp))
                   placeholders (vec (for [p planned]
                                       {:proposed-key (:proposed-key p)
                                        :referent (:referent p)
                                        :status :pending}))]
               (swap! batch-state assoc
                      :brainstorming? false
                      :planned planned
                      :drafts placeholders
                      :total-cost-cents brainstorm-cost
                      :current-index 0)
               (log-entry! {:type :response :kind kind
                            :label "brainstormed"
                            :count (count planned)})
               (propose-next! kind planned 0)))))))))

;; --- Batch results ---

(defn- toggle-keep! [idx]
  (swap! batch-state update :keep-set
         (fn [s] (if (contains? s idx) (disj s idx) (conj s idx)))))

(defn- keep-all! []
  (let [drafts (:drafts @batch-state)]
    (swap! batch-state assoc :keep-set
           (set (keep-indexed (fn [i d] (when-not (:error d) i)) drafts)))))

(defn- keep-none! []
  (swap! batch-state assoc :keep-set #{}))

(defn- batch-card [kind idx draft]
  (let [{:keys [proposed-key referent entry error status]} draft
        state @batch-state
        keep?     (contains? (:keep-set state) idx)
        modify?   (contains? (:modify-set state) idx)
        refining? (contains? (:refining-indices state) idx)
        feedback  (get-in state [:modify-feedback idx] "")
        stops (when entry
                (case kind
                  :association
                  (when (and (:colors entry) (map? (:colors entry)))
                    (->> (:colors entry)
                         (sort-by (comp - second))
                         (mapv (fn [[h w]]
                                 {:hex (normalize-hex h) :weight w :alpha 1}))))
                  :palette
                  (when (vector? (:hex entry))
                    (let [hexes (:hex entry)
                          weights (or (:weights entry)
                                      (vec (repeat (count hexes)
                                                   (/ 1.0 (count hexes)))))]
                      (mapv (fn [h w]
                              {:hex (normalize-hex h) :weight w :alpha 1})
                            hexes weights)))))]
    (cond
      ;; --- Pending placeholder ---
      (= status :pending)
      [:div.sg-batch-card.sg-batch-card-pending
       [:div.sg-batch-card-head
        [:span.sg-batch-card-key (str proposed-key)]]
       [:div.sg-batch-card-pending-body
        [:span.sg-batch-pending-spinner]
        [:span "waiting..."]]]

      ;; --- Error ---
      error
      [:div.sg-batch-card.sg-batch-card-error
       [:div.sg-batch-card-head
        [:span.sg-batch-card-controls
         [:label.sg-batch-card-keep
          [:input {:type "checkbox" :checked false :disabled true}]
          " keep"]
         [:label.sg-batch-card-keep
          [:input {:type "checkbox" :checked false :disabled true}]
          " modify"]]
        [:span.sg-batch-card-key (str proposed-key)]]
       [:div.sg-batch-card-error-body
        [:span.sg-batch-error-label "error: "]
        [:span error]]]

      ;; --- Success (entry present, ready to keep/modify) ---
      :else
      [:div.sg-batch-card
       {:class (when refining? "sg-batch-card-refining")}
       [:div.sg-batch-card-head
        [:span.sg-batch-card-controls
         [:label.sg-batch-card-keep
          [:input {:type "checkbox"
                   :checked keep?
                   :on-change (fn [_] (toggle-keep! idx))}]
          " keep"]
         [:label.sg-batch-card-keep
          [:input {:type "checkbox"
                   :checked modify?
                   :disabled refining?
                   :on-change (fn [_] (toggle-modify! idx))}]
          " modify"]]
        [:span.sg-batch-card-key (str proposed-key)]]
       (when stops
         [:div.sg-batch-card-bar
          {:style {:background (gallery/gradient-css stops "to right")}}])
       (when modify?
         [:div.sg-batch-card-modify
          [:input {:type "text"
                   :value feedback
                   :placeholder "what to change?"
                   :disabled refining?
                   :on-change (fn [e] (set-modify-feedback! idx
                                                            (gallery/target-value e)))
                   :on-key-down (fn [e]
                                  (when (and (= "Enter" (.-key e)) (not refining?))
                                    (refine-card! kind idx)))}]
          [:button {:on-click (fn [_] (refine-card! kind idx))
                    :disabled (or refining? (str/blank? feedback))}
           (if refining? "refining..." "Apply")]])
       (when (:rationale entry)
         [:div.sg-batch-card-rationale (:rationale entry)])
       [:div.sg-batch-card-meta
        (when-let [c (:category entry)]
          [:span.sg-batch-meta-pill (str (name c))])
        (when-let [s (:sigma entry)]
          [:span.sg-batch-meta-pill (str "σ=" (.toFixed s 3))])
        (when stops
          [:span.sg-batch-meta-pill (str (count stops) " colors")])]])))

(defn accept-batch! [kind]
  (let [{:keys [drafts keep-set]} @batch-state
        kept (->> drafts
                  (map-indexed vector)
                  (filter (fn [[i _]] (contains? keep-set i)))
                  (map second)
                  (filter (complement :error)))
        endpoint (case kind
                   :association "/api/proposal/association/batch-accept"
                   :palette     "/api/proposal/palette/batch-accept")
        body (case kind
               :association
               {:entries (mapv (fn [{:keys [proposed-key entry]}]
                                 {:key proposed-key :entry entry})
                               kept)}
               :palette
               {:entries (mapv (fn [{:keys [proposed-key entry]}]
                                 {:key proposed-key :entry entry})
                               kept)
                :path "palettes.edn"})]
    (when (empty? kept)
      (reset! gallery/status "nothing selected to keep")
      (throw (js/Error. "nothing to accept")))
    (swap! batch-state assoc :pending? true)
    (log-entry! {:type :request :kind kind :label "batch-accept"
                 :count (count kept)})
    (gallery/post-edn! endpoint body
                       (fn [resp]
                         (swap! batch-state assoc :pending? false)
                         (if (:error resp)
                           (do (log-entry! {:type :error :message (:error resp)})
                               (reset! gallery/status (str "error: " (:error resp))))
                           (do (swap! batch-state assoc :drafts [] :keep-set #{})
                               (log-entry! {:type :response :kind kind
                                            :label "batch-accepted"
                                            :count (:accepted resp)})
                               (reset! gallery/status
                                       (str "accepted " (:accepted resp)
                                            (when-let [f (seq (:failed resp))]
                                              (str " (" (count f) " failed)"))))
                               ;; refresh the relevant gallery
                               (case kind
                                 :association
                                 (when (resolve 'association-manager/fetch!)
                                   ((resolve 'association-manager/fetch!)))
                                 :palette
                                 (when (resolve 'palette-manager/fetch-palettes!)
                                   ((resolve 'palette-manager/fetch-palettes!))))))))))

(defn discard-batch! []
  (swap! batch-state assoc :drafts [] :keep-set #{})
  (reset! gallery/status "discarded all"))

(defn batch-results-panel [kind]
  (let [{:keys [drafts keep-set]} @batch-state]
    (when (seq drafts)
      (let [eligible (set (keep-indexed
                           (fn [i d] (when-not (:error d) i)) drafts))
            kept-count (count (clojure.set/intersection keep-set eligible))
            error-count (count (filter :error drafts))]
        [:div.sg-batch-results
         [:div.sg-batch-results-head
          [:h3 (str "Batch results — " (count drafts) " proposed"
                    (when (pos? error-count)
                      (str ", " error-count " errored")))]
          [:div.sg-tools
           [:span.sg-batch-keep-count (str kept-count " selected")]
           [:button {:on-click keep-all!}    "select all"]
           [:button {:on-click keep-none!}   "deselect all"]
           [:button.sg-primary
            {:on-click (fn [_] (accept-batch! kind))
             :disabled (zero? kept-count)}
            (str "Accept " kept-count)]
           [:button.sg-danger
            {:on-click discard-batch!}
            "Discard all"]]]
         [:div.sg-batch-cards
          (for [[i draft] (map-indexed vector drafts)]
            ^{:key i} [batch-card kind i draft])]]))))

(defn- progress-overlay [kind]
  (let [{:keys [pending? brainstorming? current-index planned total-cost-cents]} @batch-state]
    (when pending?
      (let [total (count planned)
            current (or current-index 0)
            pct (if (pos? total)
                  (* 100.0 (/ current total))
                  0)]
        [:div.sg-batch-progress
         [:div.sg-batch-progress-head
          [:span.sg-batch-progress-label
           (cond
             brainstorming?
             "Brainstorming referents..."

             (pos? total)
             (str "Generating " (inc current) " of " total
                  (when-let [next (get planned current)]
                    (str " — " (:referent next))))

             :else
             "Preparing...")]
          [:span.sg-batch-progress-cost
           (str "¢" (.toFixed (or total-cost-cents 0) 2) " so far")]]
         [:div.sg-batch-progress-bar
          [:div.sg-batch-progress-fill
           {:style {:width (str pct "%")}}]]]))))

;; --- Batch screens ---

(defn- reset-batch-state! [kind]
  (reset! batch-state
          {:mode :explicit
           :namespace ""
           :extra-context ""
           :entries [(blank-entry)]
           :category-counts [(blank-category)]
           :drafts []
           :bulk-input ""
           :keep-set #{}
           :pending? false}))

(defn batch-screen [kind]
  (let [{:keys [mode pending?]} @batch-state]
    [:div {:class (when pending? "sg-batch-running")}
     [progress-overlay kind]
     [:div.sg-propose-form
      [:h3 (case kind
             :association "Batch generate associations"
             :palette     "Batch generate palettes")]
      [mode-toggle]
      (case mode
        :explicit [explicit-form kind]
        :category [category-form])
      [:div.sg-tools
       [:button.sg-primary
        {:on-click (fn [_] (propose-batch! kind))
         :disabled pending?}
        (if pending? "running..." (case mode
                                    :explicit "Propose batch"
                                    :category "Brainstorm + propose"))]
       [:button {:on-click (fn [_] (reset-batch-state! kind))
                 :disabled pending?}
        "Reset form"]]]
     [batch-results-panel kind]]))

(defn batch-association-screen [] [batch-screen :association])
(defn batch-palette-screen []     [batch-screen :palette])

(defn screen []
  [:div
   [sub-tabs]
   (case @sub-tab
     :association       [:div [association-propose-form]]
     :palette           [:div [palette-propose-form]]
     :modify            [:div [modify-existing-form]]
     :batch-association [batch-association-screen]
     :batch-palette     [batch-palette-screen]
     nil)
   (when (#{:association :palette :modify} @sub-tab)
     [:div
      [draft-meta-panel]
      [:div.sg-proposal-split
       [transcript-panel]
       [color-preview-panel]]])])

(gallery/register-screen! :proposals "Proposal Builder" screen)