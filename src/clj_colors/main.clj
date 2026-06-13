(ns clj-colors.main
  "Load, query, mutate, and persist color palettes.

   A palette is stored with its colors, an optional set of semantic tags, and
   an optional weight distribution:

     :forest/jungle {:hex [\"#0C1A12\" ...] :tags #{\"deep\"} :weights [0.5 0.3 0.2]}

   On load each palette is enriched with derived fields computed from its colors:

     :rgba        vector of [r g b a] vectors
     :weights     normalized prominence per color, when given (else even)
     :count       number of colors
     :name        bare keyword, e.g. :jungle
     :category    keyword from the key's namespace, e.g. :forest
     :family      computed color family, e.g. :green
     :brightness :temperature :saturation :contrast and friends
     :tags        the stored tags merged with the computed descriptors

   Colors are the source of truth: everything except the stored tags and
   weights is recomputed from :hex on load, so stored metadata can never
   drift. Weights make the computed metadata prominence-aware: a speck of
   yellow in a sea of blue no longer pulls the family, warmth, or
   brightness the way a 50/50 split would.

   The registry file is machine-owned. save-registry! renders the whole
   registry deterministically, grouped by category with a '; Category'
   comment above each group. The human-facing index lives outside the data
   file in swatch_index.md and is regenerated on save."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-colors.color :as color]
            [clj-colors.meta :as meta]
            [clj-colors.svg :as svg])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(def ^:private timestamp-format
  (DateTimeFormatter/ofPattern "MM/dd/yyyy HH:mm:ss"))

(def default-resource
  "Classpath resource read by load-palettes when no source is given."
  "palettes.edn")

(defn- round3
  "Rounds x to three decimal places."
  [x]
  (/ (Math/round (* (double x) 1000.0)) 1000.0))

(defn- normalize-colors
  "Return a vector of [r g b a] vectors from a palette's :hex or :rgba."
  [{:keys [hex rgba]}]
  (cond
    hex (mapv color/hex->rgba hex)
    rgba (mapv (fn [c] (if (= 4 (count c)) (vec c) (conj (vec c) 255))) rgba)
    :else (throw (ex-info "Palette needs :hex or :rgba" {}))))

(defn- normalize-weights
  "Validate a weight vector against n colors and scale it to sum to 1.
   nil or empty weights mean an even distribution and return nil, so even
   palettes carry no :weights key at all."
  [ws n]
  (when (seq ws)
    (when (not= (count ws) n)
      (throw (ex-info "Weight count must match color count"
                      {:weights ws :color-count n})))
    (let [total (reduce + 0.0 ws)]
      (when-not (pos? total)
        (throw (ex-info "Weights must sum to a positive number" {:weights ws})))
      (mapv #(round3 (/ (double %) total)) ws))))

(defn- enrich
  "Expand a stored palette under key k into its full runtime form. Metadata is
   recomputed from the colors, weighted by prominence when :weights are given;
   the stored :tags are treated as semantic and merged with the computed
   descriptors."
  [k palette]
  (let [rgbas    (normalize-colors palette)
        hexes    (or (:hex palette) (mapv color/rgba->hex rgbas))
        weights  (normalize-weights (:weights palette) (count rgbas))
        computed (meta/metadata rgbas weights)
        semantic (set (:tags palette))
        category (when (namespace k) (keyword (namespace k)))
        tags     (into (sorted-set) (into semantic (:tags computed)))]
    (merge (dissoc computed :tags)
           (cond-> {:hex            (vec hexes)
                    :rgba           rgbas
                    :count          (count rgbas)
                    :name           (keyword (name k))
                    :category       category
                    :tags           tags
                    :brightness     (round3 (:brightness computed))
                    :temperature    (round3 (:temperature computed))
                    :saturation     (round3 (:saturation computed))
                    :contrast       (round3 (:contrast computed))
                    :mean-lightness (round3 (:mean-lightness computed))
                    :hue-concentration (round3 (:hue-concentration computed))}
             weights (assoc :weights weights)))))

(defn enrich-registry
  "Enrich every entry in a raw key->palette map."
  [raw]
  (reduce-kv (fn [m k v] (assoc m k (enrich k v))) {} raw))

(defn load-palettes
  "Read and enrich palettes. With no argument, reads the default-resource from
   the classpath. With a path string or File, reads that file directly."
  ([] (load-palettes (io/resource default-resource)))
  ([source]
   (let [source (or source default-resource)
         raw    (edn/read-string (slurp source))]
     (enrich-registry raw))))

(defonce ^{:doc "Runtime palette store. Mutated by the bang functions below."}
  registry
  (atom (load-palettes)))

; Access
(defn all-palettes
  "The full key->palette map in the registry."
  []
  @registry)

(defn palette-keys
  "All fully-qualified palette keys, e.g. :forest/jungle."
  []
  (keys @registry))

(defn palette-names
  "All bare palette names, e.g. :jungle."
  []
  (mapv :name (vals @registry)))

(defn- name-index
  "Map of bare name keyword -> full key, for bare lookups."
  []
  (reduce (fn [m k] (assoc m (keyword (name k)) k)) {} (keys @registry)))

(defn get-palette
  "Look up a palette by full key (:forest/jungle) or bare name (:jungle).
   Returns nil when absent."
  [k]
  (or (get @registry k)
      (when-let [full (get (name-index) (keyword (name k)))]
        (get @registry full))))

(defn palette-weights
  "Effective weight distribution for a palette: its stored :weights, or an
   even split when none were given. Returns nil for unknown palettes."
  [k]
  (when-let [p (get-palette k)]
    (or (:weights p)
        (let [n (:count p)]
          (vec (repeat n (round3 (/ 1.0 n))))))))

(defn categories
  "Sorted vector of distinct category keywords."
  []
  (->> (vals @registry) (keep :category) distinct sort vec))

(defn palettes-in-category
  "key->palette map of every palette in the given category keyword."
  [category]
  (into {} (filter (fn [[_ p]] (= category (:category p))) @registry)))

; Tag queries
(defn palettes-with-tags
  "Strict key->palette map of palettes carrying every given tag.
   With no tags, returns the whole registry; returns an empty map when nothing matches.

   Strict in the sense that a palette must carry all the tags specified to be returned."
  [& tags]
  (let [wanted (set tags)]
    (into {} (filter (fn [[_ p]] (every? (:tags p) wanted)) @registry))))

(defn palettes-with-any-tags
  "Relaxed key->palette map of palettes carrying at least one of the provided tags.
   Returns an empty map when nothing matches. (With no tags, nothing can match:
   prefer get-tagged-palettes or all-palettes for that intent.)

   Relaxed in the sense that it will return any palette that contains any provided tag."
  [& tags]
  (let [wanted (set tags)]
    (into {} (filter (fn [[_ p]] (some wanted (:tags p))) @registry))))

(defn palettes-with-tag
  "key->palette map of palettes carrying the given tag."
  [tag]
  (palettes-with-tags tag))

(defn palettes-by
  "key->palette map of palettes for which (pred palette) is truthy."
  [pred]
  (into {} (filter (fn [[_ p]] (pred p)) @registry)))

; Random
(defn random-palette
  "A random [key palette] entry. With tags, restricts to palettes carrying all
   of them and returns nil when none match."
  [& tags]
  (let [pool (if (seq tags) (apply palettes-with-tags tags) @registry)]
    (when (seq pool) (rand-nth (vec pool)))))

(defn random-color
  "A random hex string from the named palette, or nil when absent."
  [k]
  (when-let [p (get-palette k)] (rand-nth (:hex p))))

; Mutation (pure)
(defn add-palette
  "Return a registry map with palette under key k added or replaced. colors is a
   vector of hex strings or [r g b (a)] vectors. opts may carry :tags and
   :weights (one number per color, any positive scale; normalized on enrich)."
  ([registry-map k colors] (add-palette registry-map k colors {}))
  ([registry-map k colors opts]
   (let [hex? (string? (first colors))
         stored (cond-> (if hex?
                          {:hex (vec colors)}
                          {:hex (mapv color/rgba->hex colors)})
                  (seq (:tags opts))    (assoc :tags (vec (:tags opts)))
                  (seq (:weights opts)) (assoc :weights (vec (:weights opts))))]
     (assoc registry-map k (enrich k stored)))))

(defn remove-palette
  "Return a registry map with key k removed."
  [registry-map k]
  (dissoc registry-map k))

; Mutation (registry side effects)
(defn register-palette!
  "Add or replace a palette in the registry. Returns the enriched palette."
  ([k colors] (register-palette! k colors {}))
  ([k colors opts]
   (get (swap! registry add-palette k colors opts) k)))

(defn unregister-palette!
  "Remove a palette from the registry. Returns the removed key."
  [k]
  (swap! registry remove-palette k)
  k)

(defn reset-registry!
  "Reload the registry from a source, or from the default resource."
  ([] (reset! registry (load-palettes)))
  ([source] (reset! registry (load-palettes source))))

; SVG helpers
(defn palette-swatch-svg
  "SVG of discrete color blocks for a named palette. See clj-colors.svg/swatch-svg."
  ([k] (palette-swatch-svg k {}))
  ([k opts] (svg/swatch-svg (:hex (get-palette k)) opts)))

(defn palette-gradient-svg
  "Smooth gradient SVG for a named palette. See clj-colors.svg/gradient-svg."
  ([k] (palette-gradient-svg k {}))
  ([k opts] (svg/gradient-svg (:hex (get-palette k)) opts)))

; Persistence
;
; The registry file is machine-owned: saving renders the entire registry
; deterministically and writes it out, so adding and removing palettes are
; the same operation (save a registry with or without the entry). Entries
; are grouped by category, sorted within each group, with a '; Category'
; comment above every group for ctrl-f navigation and one blank line
; between groups. The human-facing index of categories and names lives
; outside the data file, in swatch_index.md, and is regenerated wholesale
; whenever that file exists.

(def index-file
  "Markdown index of categories and palette names, regenerated by
   save-registry! whenever this file exists in the working directory."
  "swatch_index.md")

(def ^:private field-order
  [:hex :rgba :weights :count :name :category :family :brightness :temperature
   :saturation :contrast :mean-lightness :hue-concentration])

(defn- str-vec [xs]
  (str "[" (str/join " " (map pr-str xs)) "]"))

(defn- str-set [xs]
  (str "#{" (str/join " " (map pr-str xs)) "}"))

(defn- rgb-vec [rgb]
  (str "[" (str/join " " (map (fn [c] (str "[" (str/join " " c) "]")) rgb)) "]"))

(defn- field-str [k v]
  (case k
    :hex     (str-vec v)
    :weights (str-vec v)
    :rgba    (rgb-vec v)
    (pr-str v)))

(defn- palette->lines
  "Render one palette as materialized EDN lines, :tags written as a set."
  [k palette]
  (let [present (filter #(contains? palette %) field-order)
        body    (map (fn [field] (str "   " field " " (field-str field (get palette field))))
                     present)
        body    (cons (str "  {" (subs (first body) 3)) (rest body))
        tags    (str "   :tags " (str-set (vec (:tags palette))) "}")]
    (concat [(str " " (pr-str k))] body [tags])))

(defn- category-label [cat]
  (str/capitalize (or cat "uncategorized")))

(defn registry->edn
  "Serialize a registry to EDN text: categories sorted, a '; Category'
   comment above each group, entries sorted by key within their group, and
   one blank line between groups."
  [registry-map]
  (let [cats (group-by (fn [[k _]] (namespace k)) (sort-by key registry-map))]
    (str "{"
         (str/join "\n"
                   (for [cat (sort-by str (keys cats))]
                     (str "\n ; " (category-label cat) "\n"
                          (str/join "\n"
                                    (for [[k v] (get cats cat)]
                                      (str/join "\n" (palette->lines k v)))))))
         "\n}\n")))

(defn- resolve-file
  "A writable File for path: a same-named classpath resource when it lives
   on disk (so \"palettes.edn\" finds resources/palettes.edn when running
   from source), otherwise the path itself. Resources inside jars are not
   writable and are ignored."
  ^java.io.File [path]
  (if (instance? java.io.File path)
    path
    (or (when-let [r (io/resource (str path))]
          (when (= "file" (.getProtocol r))
            (io/file r)))
        (io/file path))))

(defn write-index!
  "Write a markdown index of a registry: every category as a heading with
   its palette names beneath it. Regenerated wholesale, so additions and
   removals are reflected automatically. Returns the path as a string."
  ([path] (write-index! path @registry))
  ([path registry-map]
   (let [cats (group-by (fn [[k _]] (namespace k)) (sort-by key registry-map))]
     (spit path
           (str "# Swatch Index (Updated as of " (.format (LocalDateTime/now) timestamp-format) ")\n\n"
                (str/join "\n"
                          (for [cat (sort-by str (keys cats))]
                            (str "## " (category-label cat) "\n"
                                 (str/join "\n"
                                           (for [[k _] (get cats cat)]
                                             (str "- " (name k))))
                                 "\n")))))
     (str path))))

(defn- index-path-for
  "Where swatch_index.md belongs for a given registry file: the project
   root when the file lives in a resources directory, otherwise right
   next to the file."
  ^java.io.File [^java.io.File registry-file]
  (let [parent (.getParentFile (.getAbsoluteFile registry-file))]
    (if (and parent
             (= "resources" (.getName parent))
             (.getParentFile parent))
      (io/file (.getParentFile parent) index-file)
      (io/file (or parent (io/file ".")) index-file))))

(defn save-registry!
  "Render the registry and write it to path, replacing the file's previous
   contents, then regenerate swatch_index.md so the index tracks additions
   and removals automatically. The index lands in the project root when
   the registry file lives under resources/, otherwise next to the
   registry file itself.

   Resolution: a same-named classpath resource is used when it lives on
   disk, so palettes.edn reaches resources/palettes.edn when running from
   the clj-colors source tree; otherwise the path is taken as given,
   relative to the working directory. Removal is just unregister-palette!
   followed by this."
  ([path] (save-registry! path @registry))
  ([path registry-map]
   (let [file (resolve-file path)]
     (spit file (registry->edn registry-map))
     (write-index! (index-path-for file) registry-map)
     (str file))))