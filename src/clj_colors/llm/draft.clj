(ns clj-colors.llm.draft
  "Draft-state helpers. Each domain namespace owns its own draft
   atom; these helpers operate on whatever atom is passed in.
   Pure structural manipulation — no IO, no parsing.")

(defn stage!
  "Replace the draft with a new {:referent/key... :entry... :usage...}
   map. Returns the staged entry."
  [draft-atom info entry usage]
  (reset! draft-atom (assoc info :entry entry :usage usage))
  entry)

(defn current
  "Current draft or nil."
  [draft-atom]
  @draft-atom)

(defn modify!
  "Apply f to the current draft's :entry. Throws if no draft staged."
  [draft-atom f]
  (if-let [d @draft-atom]
    (let [entry' (f (:entry d))]
      (reset! draft-atom (assoc d :entry entry'))
      entry')
    (throw (ex-info "No draft staged" {}))))

(defn discard!
  "Clear the draft."
  [draft-atom]
  (reset! draft-atom nil))