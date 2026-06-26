(ns clj-colors.extensions
  "Discover and load extension data files from resources/extensions/.

   Convention:
     resources/extensions/palettes/**/*.edn       → palette extensions
     resources/extensions/color_tags/**/*.edn     → color-tag extensions
     resources/extensions/associations/**/*.edn   → association extensions

   Subdirectories under each type are walked recursively in
   path-sorted order, so a collection can be a single file
   (resources/extensions/palettes/holidays.edn) or a nested tree
   (resources/extensions/palettes/characters/anime/sailor-moon.edn).
   All EDN files at any depth are merged into a single map per type
   and handed to the corresponding base loader at delay-build time.

   Each extension file is just an EDN map matching the schema for
   its type, exactly as a base data file would. Conflicts (same key
   in base and extension) resolve later-wins, so extensions can
   override base entries if they want; in practice, namespacing
   keys under extension-specific prefixes (:flags.national/japan
   rather than :flag/japan) keeps the choice the author's, not the
   merger's."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(def ^:dynamic *extensions-root*
  "Filesystem root for extension discovery. Dynamic so tests and
   integrations can rebind. Resolved relative to the JVM working
   directory, same as the existing data files."
  "resources/extensions")

(defn- tag-source
  "Mark every entry's :source so downstream code can distinguish
   base entries from extension entries. Doesn't overwrite an
   existing :source so authored entries keep their declared source."
  [entries source-tag]
  (reduce-kv (fn [m k v]
               (assoc m k (update v :source #(or % source-tag))))
             {}
             entries))

(defn read-edn-safely
  "Read EDN from a file path. Returns default-val (empty map by
   default) if the file doesn't exist or fails to parse, logging
   a warning to stderr on parse failure. Used so a corrupt or
   missing extension file doesn't break the whole load."
  ([path] (read-edn-safely path {}))
  ([path default-val]
   (let [f (clojure.java.io/file path)]
     (if (.exists f)
       (try
         (clojure.edn/read-string (slurp f))
         (catch Exception e
           (binding [*out* *err*]
             (println "WARN failed to read EDN from" path
                      (.getMessage e)))
           default-val))
       default-val))))

(defn write-edn!
  "Write an EDN map to path, pretty-printed for readability.
   Creates the parent directory if it doesn't exist. Throws on
   I/O failure; callers that want to tolerate failure wrap the
   call in try/catch. This is what sync-back! uses to materialize
   enriched extension entries back to disk."
  [path data]
  (let [f      (clojure.java.io/file path)
        parent (.getParentFile f)]
    (when (and parent (not (.exists parent)))
      (.mkdirs parent))
    (with-open [w (clojure.java.io/writer f)]
      (binding [*out*                  w
                *print-namespace-maps* false]
        (pp/pprint data)))))

(defn- find-edn-files
  "Recursively find every .edn file under dir. Returns a sorted
   sequence by path so load order is deterministic across runs."
  [^java.io.File dir]
  (when (and dir (.exists dir) (.isDirectory dir))
    (->> (file-seq dir)
         (filter (fn [^java.io.File f]
                   (and (.isFile f)
                        (str/ends-with? (.getName f) ".edn"))))
         (sort-by (fn [^java.io.File f] (.getPath f))))))

(defn load-type
  "Load and merge every extension file under
   resources/extensions/<type>/ into a single map. Returns an empty
   map when no extensions are present, so callers can unconditionally
   merge into their base data."
  [type]
  (reduce
   (fn [acc f]
     (if-let [data (read-edn-safely f)]
       (merge acc data)
       acc))
   {}
   (find-edn-files (io/file *extensions-root* type))))

(defn merge-files
  "Collapse the per-file map from any load-*-extensions function
   into a single key→entry map. Later files in the walk order win
   on key collisions, which in practice almost never matters since
   namespacing keeps keys distinct across files."
  [by-file]
  (apply merge (vals by-file)))

(defn load-palette-extensions
  "Read every EDN file under resources/extensions/palettes/ and
   return a map of file-path (as String) to its parsed entries
   (with :source tagged). The per-file shape preserves which file
   each entry came from so sync-back! can write expanded entries
   back to their originating file."
  []
  (let [dir   (clojure.java.io/file *extensions-root* "palettes")
        files (find-edn-files dir)]
    (into {}
          (map (fn [^java.io.File f]
                 [(.getPath f)
                  (tag-source (read-edn-safely f {}) :extension)]))
          files)))

(defn load-association-extensions
  "Load every EDN file under resources/extensions/associations/ and
   merge their entries into a single map keyed by association key.
   Each entry is :source-tagged :extension. Unlike load-palette-extensions,
   this flattens away the per-file structure because associations
   don't need sync-back behavior."
  []
  (let [dir   (clojure.java.io/file *extensions-root* "associations")
        files (find-edn-files dir)]
    (reduce
     (fn [acc ^java.io.File f]
       (merge acc (tag-source (read-edn-safely f {}) :extension)))
     {}
     files)))

(defn load-color-tag-extensions []
  (let [dir   (clojure.java.io/file *extensions-root* "color_tags")
        files (find-edn-files dir)]
    (into {}
          (map (fn [^java.io.File f]
                 [(.getPath f)
                  (tag-source (read-edn-safely f {}) :extension)]))
          files)))

(defn list-loaded
  "Return information about every extension file found, regardless
   of whether it parsed successfully. Useful for confirming what
   actually shipped with a build, or for quick auditing during
   development. Returns a vector of maps with :type, :path
   (relative to the type directory), and :size in bytes."
  []
  (vec
   (for [type ["palettes" "color_tags" "associations"]
         ^java.io.File f (find-edn-files (io/file *extensions-root* type))]
     {:type type
      :path (str/replace (.getPath f)
                         (str (.getPath (io/file *extensions-root* type)) "/")
                         "")
      :size (.length f)})))

(defn sync-back!
  "For each extension file in by-file, pull its keys' fully-enriched
   form from registry-snapshot and write that back to the file.
   The equality check skips files whose enriched form already
   matches what's on disk, so a normal load with no hand-edits is
   silent. Write failures log to stderr and don't abort; the
   in-memory registry is correct regardless of file write success.

   This is what makes the system feel like enrichment and storage
   are one operation. You drop an entry in a file with just :hex,
   the system loads it, enriches it, writes the full entry back.
   Next load sees the full entry already and changes nothing."
  [by-file registry-snapshot]
  (doseq [[path entries] by-file]
    (let [ks       (keys entries)
          expanded (select-keys registry-snapshot ks)
          on-disk  (read-edn-safely path {})]
      (when (not= expanded on-disk)
        (try
          (write-edn! path expanded)
          (catch Exception e
            (binding [*out* *err*]
              (println "WARN sync-back failed for" path
                       (.getMessage e)))))))))