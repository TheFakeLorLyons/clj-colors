(ns clj-colors.main
  "Load, query, mutate, and persist color palettes.

   A palette is stored with its colors plus an optional set of semantic tags:

     :forest/jungle {:hex [\"#0C1A12\" ...] :tags #{\"deep\"}}

   On load each palette is enriched with derived fields computed from its colors:

     :rgba        vector of [r g b a] vectors
     :count       number of colors
     :name        bare keyword, e.g. :jungle
     :category    keyword from the key's namespace, e.g. :forest
     :family      computed color family, e.g. :green
     :brightness :temperature :saturation :contrast and friends
     :tags        the stored tags merged with the computed descriptors

   Colors are the source of truth: everything except the stored tags is
   recomputed from :hex on load, so stored metadata can never drift. There is no
   separate :authored-tags key; :tags carries both your semantic tags and the
   computed ones, and is what gets written back out."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clj-colors.color :as color]
            [clj-colors.meta :as meta]
            [clj-colors.svg :as svg]))

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

(defn- enrich
  "Expand a stored palette under key k into its full runtime form. Metadata is
   recomputed from the colors; the stored :tags are treated as semantic and
   merged with the computed descriptors."
  [k palette]
  (let [rgbas    (normalize-colors palette)
        hexes    (or (:hex palette) (mapv color/rgba->hex rgbas))
        computed (meta/metadata rgbas)
        semantic (set (:tags palette))
        category (when (namespace k) (keyword (namespace k)))
        tags     (into (sorted-set) (into semantic (:tags computed)))]
    (merge (dissoc computed :tags)
           {:hex            (vec hexes)
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
            :hue-concentration (round3 (:hue-concentration computed))})))

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
   With no tags, returns the whole registry. Returns an empty map when nothing matches. 
   (With no tags, nothing can match: prefer get-tagged-palettes or all-palettes for that intent.)
 
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
   vector of hex strings or [r g b (a)] vectors. opts may carry :tags."
  ([registry-map k colors] (add-palette registry-map k colors {}))
  ([registry-map k colors opts]
   (let [hex? (string? (first colors))
         stored (cond-> (if hex?
                          {:hex (vec colors)}
                          {:hex (mapv color/rgba->hex colors)})
                  (seq (:tags opts)) (assoc :tags (vec (:tags opts))))]
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
; The registry file is the user's document: it carries an index block,
; category comments, and hand-kept formatting that the library must never
; destroy. save-registry! therefore only rewrites a file wholesale when it
; does not exist yet. Otherwise the file's text is scanned for its
; top-level entries (everything between them is opaque bytes), new
; palettes are inserted after the last entry of their category or in a
; fresh category section before the closing brace, and entries whose
; colors or tags changed are replaced in place. Map ordering never comes
; into it because the file is never round-tripped through a map; the
; file's own text supplies the order.
 
(def ^:private field-order
  [:hex :rgba :count :name :category :family :brightness :temperature
   :saturation :contrast :mean-lightness :hue-concentration])
 
(defn- str-vec [xs]
  (str "[" (str/join " " (map pr-str xs)) "]"))
 
(defn- str-set [xs]
  (str "#{" (str/join " " (map pr-str xs)) "}"))
 
(defn- rgb-vec [rgb]
  (str "[" (str/join " " (map (fn [c] (str "[" (str/join " " c) "]")) rgb)) "]"))
 
(defn- field-str [k v]
  (case k
    :hex (str-vec v)
    :rgba (rgb-vec v)
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
 
(defn- render-entry [k palette]
  (str/join "\n" (palette->lines k palette)))
 
(defn registry->edn
  "Serialize a whole registry to a materialized EDN string grouped by
   category. Used only when creating a registry file from nothing; for
   existing files see sync-registry-file!."
  [registry-map]
  (let [sorted (sort-by key registry-map)
        cats   (group-by (fn [[k _]] (namespace k)) sorted)]
    (str "{\n"
         (str/join "\n\n"
                   (for [cat (sort (keys cats))]
                     (str " ;; " cat "\n"
                          (str/join "\n"
                                    (for [[k v] (get cats cat)]
                                      (str/join "\n" (palette->lines k v)))))))
         "\n}\n")))
 
;; EDN text scanning ----------------------------------------------------------
;; A small structural walker over the registry file's source text. It
;; understands just enough EDN to find entry boundaries: strings (with
;; escapes), line comments, character literals, and bracket nesting.
;; Nothing is evaluated and nothing between entries is interpreted.
 
(defn- skip-line [^String src i]
  (let [nl (str/index-of src "\n" i)]
    (if nl (inc nl) (count src))))
 
(defn- skip-ws-comments [^String src i]
  (loop [j i]
    (if (>= j (count src))
      j
      (let [c (.charAt src j)]
        (cond
          (or (Character/isWhitespace c) (= c \,)) (recur (inc j))
          (= c \;) (recur (long (skip-line src j)))
          :else j)))))
 
(defn- read-string-lit [^String src i]
  (loop [j (inc i)]
    (case (.charAt src j)
      \\ (recur (+ j 2))
      \" (inc j)
      (recur (inc j)))))
 
(defn- opener? [c] (contains? #{\( \[ \{} c))
(defn- closer? [c] (contains? #{\) \] \}} c))
 
(defn- read-coll [^String src i]
  (loop [j (inc i) depth 1]
    (let [c (.charAt src j)]
      (cond
        (= c \;)    (recur (long (skip-line src j)) depth)
        (= c \")    (recur (long (read-string-lit src j)) depth)
        (= c \\)    (recur (+ j 2) depth)
        (opener? c) (recur (inc j) (inc depth))
        (closer? c) (if (= 1 depth) (inc j) (recur (inc j) (dec depth)))
        :else       (recur (inc j) depth)))))
 
(defn- read-form
  "Index just past one complete form starting at i."
  [^String src i]
  (let [c (.charAt src i)]
    (cond
      (= c \")    (read-string-lit src i)
      (= c \#)    (read-form src (inc i))
      (opener? c) (read-coll src i)
      (= c \\)    (+ i 2)
      :else
      (loop [j (inc i)]
        (if (or (>= j (count src))
                (let [d (.charAt src j)]
                  (or (Character/isWhitespace d) (= d \,) (= d \;)
                      (opener? d) (closer? d))))
          j
          (recur (inc j)))))))
 
(defn- scan-entries
  "Locate every top-level key/value entry in the source text of a single
   top-level map. Returns {:entries [{:key kw :start i :v-start i :end i}]
   :close i} where each span runs from the key through its value and
   :close is the index of the map's closing brace. Comments and formatting
   between entries belong to no span, so edits leave them untouched."
  [^String src]
  (let [open (skip-ws-comments src 0)]
    (when-not (= \{ (.charAt src open))
      (throw (ex-info "Registry file must contain a single top-level map"
                      {:index open})))
    (loop [i (inc open) entries []]
      (let [i (long (skip-ws-comments src i))]
        (when (>= i (count src))
          (throw (ex-info "Unbalanced registry file: no closing brace" {})))
        (if (= \} (.charAt src i))
          {:entries entries :close i}
          (let [k-end   (long (read-form src i))
                k       (edn/read-string (subs src i k-end))
                v-start (long (skip-ws-comments src k-end))
                v-end   (long (read-form src v-start))]
            (recur v-end
                   (conj entries {:key k :start i :v-start v-start :end v-end}))))))))
 
;; Patching --------------------------------------------------------------------
 
(defn- apply-edits
  "Apply {:start :end :text} edits to src. Applied back to front so
   indices stay valid; ties at the same index keep generation order."
  [src edits]
  (let [edits (map-indexed (fn [i e] (assoc e :ord i)) edits)]
    (reduce (fn [s {:keys [start end text]}]
              (str (subs s 0 start) text (subs s end)))
            src
            (sort-by (juxt :start :ord) #(compare %2 %1) edits))))
 
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
 
(defn sync-registry-file!
  "Update an existing registry EDN file from a registry map.
 
   Palettes missing from the file are inserted after the last entry of
   their category, or in a new ';; category' section just before the
   closing brace when the category is new. Entries already in the file are
   replaced in place only when their :hex or :tags differ from the
   registry. Everything else, including the index block, category
   comments, and formatting, is preserved byte for byte. Entries in the
   file but absent from the registry are deliberately left alone; prune
   those by hand.
 
   Returns the file path as a string."
  ([path] (sync-registry-file! path @registry))
  ([path registry-map]
   (let [file (resolve-file path)
         src  (slurp file)
         {:keys [entries close]} (scan-entries src)
         file-keys (set (map :key entries))
         replacements
         (for [{:keys [key start v-start end]} entries
               :let [reg (get registry-map key)]
               :when reg
               :let [stored (edn/read-string (subs src v-start end))]
               :when (or (not= (:hex stored) (:hex reg))
                         (not= (set (:tags stored)) (set (:tags reg))))]
           {:start start :end end :text (subs (render-entry key reg) 1)})
         missing (sort (remove file-keys (keys registry-map)))
         insertions
         (for [[cat ks] (sort-by key (group-by namespace missing))
               :let [block  (str/join "\n"
                                      (map #(render-entry % (get registry-map %))
                                           (sort ks)))
                     anchor (let [ends (keep (fn [e]
                                               (when (= cat (namespace (:key e)))
                                                 (:end e)))
                                             entries)]
                              (when (seq ends) (apply max ends)))]]
           (if anchor
             {:start anchor :end anchor :text (str "\n" block)}
             {:start close :end close
              :text (str "\n ;; " (or cat "uncategorized") "\n" block "\n")}))
         edits (concat replacements insertions)]
     (when (seq edits)
       (spit file (apply-edits src edits)))
     (str file))))
 
(defn save-registry!
  "Write the registry to a path. When the file already exists the update
   is textual... which is not ideal, but for now will do (see sync-registry-file!): 
   only new and changed palettes touch the file and all other text is preserved. 
   When the file does not exist it is created with the full materialized rendering.
 
   Resolution: a same-named classpath resource is used when it lives on
   disk, so \"palettes.edn\" reaches resources/palettes.edn when running
   from the clj-colors source tree; otherwise the path is taken as given,
   relative to the working directory."
  ([path] (save-registry! path @registry))
  ([path registry-map]
   (let [file (resolve-file path)]
     (if (.exists file)
       (sync-registry-file! file registry-map)
       (do (spit file (registry->edn registry-map))
           (str file))))))

(defn- line-start-of
  "Index of the first character of the line containing index i."
  [^String src i]
  (if-let [nl (str/last-index-of src "\n" (dec (long i)))]
    (inc (long nl))
    0))

(defn- strip-category-header
  "Walk upward from line-start over contiguous comment lines. When one of
   them mentions the category name, return the start index of the topmost
   such line, absorbing one blank line above the block; otherwise return
   line-start unchanged. Conservative on purpose: comments that do not
   name the category, or that share a line with other content, survive."
  [^String src line-start cat]
  (let [cat-name (str/lower-case (or cat "uncategorized"))]
    (loop [ls (long line-start) matched nil]
      (if (zero? ls)
        (or matched line-start)
        (let [prev-ls (long (line-start-of src (dec ls)))
              trimmed (str/trim (subs src prev-ls (dec ls)))]
          (cond
            (str/starts-with? trimmed ";")
            (recur prev-ls
                   (if (str/includes? (str/lower-case trimmed) cat-name)
                     prev-ls
                     matched))

            (and matched (str/blank? trimmed))
            prev-ls

            :else (or matched line-start)))))))

(defn remove-palette-from-file!
  "Delete one palette from the registry EDN file, removing exactly the lines 
   the entry occupies. When the entry was the last of its category, comment lines 
   directly above it that mention the category name (e.g. an ';; ocean' header) 
   are removed with it, along with one preceding blank line, so no orphan header 
   remains (This is just for human readability; the library does not interpret 
   category comments in any way).
   
   Comments that do not name the category, or that share a line with other 
   text (the '{; Arctic' style), are left alone. Other entries and all other
   formatting are untouched. 
   
   Returns the path string, or nil when the key is not present in the file. 
   
   Registry untouched; pair with unregister-palette!."
  [path k]
  (let [file (resolve-file path)]
    (when (.exists file)
      (let [src (slurp file)
            {:keys [entries]} (scan-entries src)]
        (when-let [{:keys [start end]} (first (filter #(= k (:key %)) entries))]
          (let [cat          (namespace k)
                last-of-cat? (not-any? #(and (not= k (:key %))
                                             (= cat (namespace (:key %))))
                                       entries)
                line-start   (line-start-of src start)
                line-start   (if last-of-cat?
                               (strip-category-header src line-start cat)
                               line-start)
                line-end     (loop [j (long end)]
                               (cond
                                 (>= j (count src)) j
                                 (= \newline (.charAt ^String src j)) (inc j)
                                 (or (= \space (.charAt ^String src j))
                                     (= \tab (.charAt ^String src j))) (recur (inc j))
                                 :else j))]
            (spit file (str (subs src 0 line-start) (subs src line-end)))
            (str file)))))))