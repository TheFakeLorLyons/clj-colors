(ns clj-colors.llm.core
  "HTTP, parsing, and request infrastructure for LLM bridges.
   Parsing uses rewrite-clj for robustness: handles Clojure's full
   syntax (not just strict EDN), copes with whitespace, comments,
   fences, and recovers gracefully from messy responses. Domain
   namespaces build context and draft logic on top."
  (:require [clojure.string :as str]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [rewrite-clj.parser :as rp]
            [rewrite-clj.node :as rn]))

(def ^:dynamic *api-key*
  (System/getenv "ANTHROPIC_API_KEY"))

(def ^:dynamic *model* "claude-sonnet-4-6")

(def ^:dynamic *max-tokens* 64000)

(defn request-completion
  "Send one Anthropic Messages request. Returns {:text ... :usage ...}.
   System prompt is passed explicitly so callers can compose
   specialized prompts without rebinding dynamic state across
   threads."
  [system-prompt user-text]
  (when-not *api-key*
    (throw (ex-info "ANTHROPIC_API_KEY not set" {})))
  (let [resp @(http/post
               "https://api.anthropic.com/v1/messages"
               {:headers {"x-api-key" *api-key*
                          "anthropic-version" "2023-06-01"
                          "content-type" "application/json"}
                :body (json/generate-string
                       {:model      *model*
                        :max_tokens *max-tokens*
                        :system     system-prompt
                        :messages   [{:role "user" :content user-text}]})})]
    (if (= 200 (:status resp))
      (let [body (json/parse-string (:body resp) true)]
        {:text  (-> body :content first :text)
         :usage (:usage body)})
      (throw (ex-info "Claude API error"
                      {:status (:status resp)
                       :body   (:body resp)})))))

(defn strip-fences
  "Remove optional ```edn or ```clojure markdown fences if present.
   Kept for cases where the response has trailing prose after the
   fence; rewrite-clj would otherwise refuse to parse the
   surrounding fence syntax."
  [s]
  (-> s
      str/trim
      (str/replace #"^```(?:edn|clojure|edn-clojure)?\s*" "")
      (str/replace #"\s*```$" "")
      str/trim))

(defn- substantive-map-node?
  "True if node is a non-empty map literal. Filters out the LLM's
   empty-map placeholders and non-map top-level forms when scanning
   candidates."
  [node]
  (and (= :map (rn/tag node))
       (try
         (let [val (rn/sexpr node)]
           (and (map? val) (seq val)))
         (catch Exception _ false))))

(defn- extract-top-level-maps
  "Scan for top-level map literals. After parsing one successfully,
   advance past its end so nested maps inside it aren't reported as
   separate candidates. Returns vector of {:string :node} entries in
   source order.
   
   Invariants:
   - Each returned map is non-empty and substantive.
   - Returned maps are in source order; consumers wanting the last
     one (typical LLM use case) use (last result).
   - Nested maps inside a returned map are NOT separately reported.
   - Malformed braces (unclosed, etc.) are silently skipped."
  [s]
  (loop [pos 0 acc []]
    (if-let [brace-idx (str/index-of s "{" pos)]
      (let [substring (subs s brace-idx)
            result (try
                     (let [node (rp/parse-string substring)]
                       (when (substantive-map-node? node)
                         (let [str-form (rn/string node)]
                           {:string str-form
                            :node   node
                            :end    (+ brace-idx (count str-form))})))
                     (catch Exception _ nil))]
        (if result
          (recur (:end result) (conj acc (dissoc result :end)))
          (recur (inc brace-idx) acc)))
      acc)))

(defn extract-last-map-from-string
  "Return the last top-level map literal as {:string :node}, or nil
   if none. Matches the LLM's draft-then-corrected pattern: when
   multiple top-level maps appear, the corrected (later) one wins.
   Nested maps inside a top-level map are NOT considered separate
   candidates."
  [s]
  (last (extract-top-level-maps s)))

(defn parse-llm-edn
  "Parse a string of LLM-returned Clojure-EDN. Scans for top-level
   map literals via rewrite-clj and returns the LAST one. Throws
   ex-info with diagnostic context if no map is found.
   
   Uses rewrite-clj rather than clojure.edn/read-string for
   robustness: handles namespaced keywords with digit-leading names
   like :kaggle-emotional/k0445, comments, set literals, and the
   full Clojure reader syntax. The parser is lexical-only; no code
   is evaluated."
  [s context]
  (if-let [extracted (extract-last-map-from-string s)]
    (try
      (rn/sexpr (:node extracted))
      (catch Exception e
        (throw (ex-info "Found map node but sexpr conversion failed"
                        (assoc context
                               :extracted (:string extracted)
                               :error (.getMessage e))))))
    (throw (ex-info "No EDN map found in response"
                    (assoc context :raw s)))))

(defn one-shot
  "Compose request + strip + parse into a single call. Returns
   {:entry ... :usage ... :raw ...}. The raw text is preserved
   for diagnostics. context passes through to ex-info on parse
   failure so callers can identify which referent or modification
   request produced bad output."
  [system-prompt user-text context]
  (let [{:keys [text usage]} (request-completion system-prompt user-text)
        cleaned (strip-fences text)
        entry   (parse-llm-edn cleaned context)]
    {:entry entry :usage usage :raw text}))