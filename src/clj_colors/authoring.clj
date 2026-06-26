(ns clj-colors.authoring
  "CRUD layer for authored associations. Each entry is hand-curated
   (by a human or LLM) to ground a specific real-world referent in
   OKLAB space with semantic tags. Stored separately from kaggle data
   in resources/authored.edn so the two don't get tangled.

   Authored entries carry an optional :sigma controlling the influence
   radius of the association's field. Small sigma (0.04) = tight
   region, for narrow referents like :mineral/obsidian. Large sigma
   (0.15) = broad region, for diffuse referents like :sky/twilight."
  (:require [clj-colors.associations :as associations]
            [clj-colors.compatibility :as cc]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(def ^:dynamic *authored-path*
  "Filesystem path for the active authored EDN file. Bind to write
   to a different file for testing, A/B experiments, or temporary
   collections. The canonical production path is the default."
  "resources/extensions/associations/llm-generated.edn")

(def authored-path *authored-path*)

(defn resolve-path
  "Convert a logical name to a full file path. Bare names like
   \"test_flags_1\" resolve under resources/extensions/associations/
   with .edn appended. Paths containing / or \\ are used as-is."
  [name-or-path]
  (cond
    (nil? name-or-path)
    *authored-path*

    (or (str/includes? name-or-path "/")
        (str/includes? name-or-path "\\"))
    name-or-path

    :else
    (str "resources/extensions/associations/" name-or-path ".edn")))

(def default-sigma 0.08)

(defn- valid-hex? [s]
  (and (string? s)
       (re-matches #"#[0-9a-fA-F]{6}" s)))

(defn- valid-tag-map? [tm]
  (and (map? tm)
       (seq tm)
       (every? keyword? (keys tm))
       (every? (fn [v]
                 (and (map? v)
                      (number? (:weight v))
                      (>= (:weight v) 0.0)))
               (vals tm))))

(defn- color-errors
  "Return a sequence of human-readable error strings for a :colors
   value. Empty seq means valid. Provides specific diagnostics
   instead of one generic 'invalid' message."
  [cm]
  (cond
    (not (map? cm))
    [":colors must be a map; got a " (.getName (class cm))]

    (empty? cm)
    [":colors must be non-empty"]

    :else
    (let [bad-hex (remove valid-hex? (keys cm))
          bad-weights (remove (fn [w]
                                (and (number? w) (>= w 0.0) (<= w 1.0)))
                              (vals cm))
          sum (reduce + 0.0 (vals cm))
          weight-issue (when-not (and (>= sum 0.95) (<= sum 1.05))
                         (format ":colors weights sum to %.3f, not ~1.0" sum))]
      (cond-> []
        (seq bad-hex)
        (conj (str ":colors has invalid hex keys: "
                   (str/join ", " bad-hex)))

        (seq bad-weights)
        (conj (str ":colors has invalid weights: "
                   (str/join ", " bad-weights)))

        weight-issue
        (conj weight-issue)))))

(defn- validate-entry
  [k entry]
  (let [errors
        (->> [(when-not (keyword? k) ["key must be a keyword"])
              (color-errors (:colors entry))
              (when-not (valid-tag-map? (:tags entry))
                [":tags must be a non-empty map of keyword -> {:weight n}"])
              (when (and (some? (:sigma entry))
                         (not (and (number? (:sigma entry))
                                   (pos? (:sigma entry)))))
                [":sigma must be a positive number when provided"])]
             (apply concat)
             (remove nil?))]
    (if (seq errors)
      (throw (ex-info "Invalid authored entry"
                      {:key k :entry entry :errors (vec errors)}))
      entry)))

(defn- normalize-hex [s]
  (let [s (str/lower-case (str s))]
    (if (str/starts-with? s "#") s (str "#" s))))

(defn- migrate-colors
  "Legacy support: a bare set gets converted to equal-weight map.
   Use this ONLY at load time for old data. New entries must arrive
   with weights from the start."
  [colors]
  (cond
    (map? colors) (into {} (map (fn [[h w]] [(normalize-hex h) (double w)])) colors)
    (set? colors) (let [n (count colors)
                        w (double (/ 1.0 n))]
                    (into {} (map (fn [h] [(normalize-hex h) w])) colors))
    :else
    (throw (ex-info ":colors must be a map of hex -> weight" {:colors colors}))))

(defn- normalize-entry [entry]
  (-> entry
      (assoc :source :llm-generated)
      (update :sigma #(or % default-sigma))
      (update :colors migrate-colors)))

(defn load-authored
  "Read an authored map from disk. With no args, uses *authored-path*.
   With a name-or-path, resolves to resources/extensions/associations/
   <name>.edn (or uses the path as-is if it contains separators).
   Returns {} if the file doesn't exist."
  ([] (load-authored nil))
  ([name-or-path]
   (let [path (resolve-path name-or-path)
         f    (io/file path)]
     (if (.exists f)
       (with-open [r (java.io.PushbackReader. (io/reader f))]
         (edn/read r))
       {}))))

(defn save-authored!
  "Write an authored map to disk, pretty-printed. With one arg, writes
   to *authored-path*. With two args, the first is a name or path."
  ([entries] (save-authored! nil entries))
  ([name-or-path entries]
   (let [path (resolve-path name-or-path)
         f    (io/file path)
         parent (.getParentFile f)]
     (when (and parent (not (.exists parent)))
       (.mkdirs parent))
     (spit path (with-out-str (pp/pprint entries)))
     entries)))

(defn add-association!
  "Add or replace an authored association. Validates, writes the
   file, refreshes the associations cache."
  ([k entry] (add-association! nil k entry))
  ([name-or-path k entry]
   (let [normalized (normalize-entry entry)]
     (validate-entry k normalized)
     (let [current (load-authored name-or-path)
           updated (assoc current k normalized)]
       (save-authored! name-or-path updated)
       (associations/refresh!)
       (cc/refresh!)
       normalized))))

(defn update-association!
  ([k changes] (update-association! nil k changes))
  ([name-or-path k changes]
   (let [current (load-authored name-or-path)
         existing (get current k)]
     (when-not existing
       (throw (ex-info "No such authored entry"
                       {:key k :file (resolve-path name-or-path)})))
     (let [merged (normalize-entry (merge existing changes))]
       (validate-entry k merged)
       (save-authored! name-or-path (assoc current k merged))
       (associations/refresh!)
       (cc/refresh!)
       merged))))

(defn remove-association!
  ([k] (remove-association! nil k))
  ([name-or-path k]
   (let [current (load-authored name-or-path)
         removed (get current k)]
     (when removed
       (save-authored! name-or-path (dissoc current k))
       (associations/refresh!)
       (cc/refresh!)
       removed))))

(defn remove-by-namespace!
  ([ns-str] (remove-by-namespace! nil ns-str))
  ([name-or-path ns-str]
   (let [current (load-authored name-or-path)
         kept    (into {} (remove (fn [[k _]]
                                    (= ns-str (namespace k)))
                                  current))
         removed (- (count current) (count kept))]
     (save-authored! name-or-path kept)
     (associations/refresh!)
     (cc/refresh!)
     {:removed removed :remaining (count kept) :file (resolve-path name-or-path)})))

(defn list-authored
  "All authored entries from the named file. With one arg that's a
   keyword, filters by :category in the default file. With two args,
   first is name-or-path, second is optional category filter."
  ([] (load-authored nil))
  ([name-or-category]
   (if (keyword? name-or-category)
     (into {} (filter (fn [[_ e]] (= name-or-category (:category e)))
                      (load-authored nil)))
     (load-authored name-or-category)))
  ([name-or-path category]
   (into {} (filter (fn [[_ e]] (= category (:category e)))
                    (load-authored name-or-path)))))