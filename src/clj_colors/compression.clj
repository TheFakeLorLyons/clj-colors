(ns clj-colors.compression
  "Pack and unpack large EDN files for repo sharing.

   Large datasets like color_tags.edn or association corpora are
   slow to commit and slow to clone. This namespace pairs each
   .edn file with an optional .edn.gz companion: the compressed
   form lives in the repo, the uncompressed form lives on disk
   locally and is regenerated on demand.

   Two entry points cover normal use:

     (pack-file! \"resources/color_tags.edn\")
       writes resources/color_tags.edn.gz alongside it.

     (unpack-file! \"resources/color_tags.edn.gz\")
       writes resources/color_tags.edn alongside it.

   For automated bootstrapping at refresh time:

     (ensure-unpacked! \"resources/color_tags.edn\")
       unpacks the .gz if the .edn is missing; no-op otherwise.

   Round-trip safety: every byte of the original .edn is
   preserved. Whitespace, comments, structural ordering are all
   bytestream-preserved by gzip; this namespace does not parse
   EDN at any point.

   File-path conventions:
     <name>.edn     uncompressed source-of-truth on disk
     <name>.edn.gz  gzipped form, committed to the repo

   The .gz extension is conventional; this namespace does not
   accept other extensions. To pack a non-.edn file, rename it
   first or extend gz-path."
  (:require [clojure.java.io :as io])
  (:import [java.io File FileInputStream FileOutputStream]
           [java.util.zip GZIPInputStream GZIPOutputStream]))

(def ^:private buffer-size 65536)

(defn gz-path
  "Given a path to an .edn file, return the conventional .gz
   path. Idempotent: passing a path that already ends in .gz
   returns it unchanged."
  [path]
  (if (.endsWith ^String path ".gz")
    path
    (str path ".gz")))

(defn edn-path
  "Given a path to a .gz file, return the corresponding .edn
   path. Idempotent: passing a path without .gz returns it
   unchanged."
  [path]
  (if (.endsWith ^String path ".gz")
    (subs path 0 (- (count path) 3))
    path))

(defn- file
  ^File [path]
  (io/file path))

(defn exists?
  "True when the file at path exists and is non-empty.
   Empty files are treated as missing to guard against
   interrupted writes."
  [path]
  (let [f (file path)]
    (and (.exists f) (pos? (.length f)))))

(defn pack-file!
  "Compress src-path into dst-path (defaults to src + .gz).
   Returns a map of {:source :destination :original-bytes
   :compressed-bytes :ratio} after success.

   Overwrites dst-path if it already exists. Throws if src-path
   does not exist."
  ([src-path] (pack-file! src-path (gz-path src-path)))
  ([src-path dst-path]
   (when-not (exists? src-path)
     (throw (ex-info "Source file missing or empty"
                     {:source src-path})))
   (with-open [in  (FileInputStream. (file src-path))
               out (GZIPOutputStream.
                    (FileOutputStream. (file dst-path))
                    buffer-size)]
     (io/copy in out :buffer-size buffer-size))
   (let [orig (.length (file src-path))
         comp (.length (file dst-path))]
     {:source src-path
      :destination dst-path
      :original-bytes orig
      :compressed-bytes comp
      :ratio (if (pos? comp) (double (/ orig comp)) 0.0)})))

(defn unpack-file!
  "Decompress src-path into dst-path (defaults to src minus
   .gz). Returns a map of {:source :destination
   :compressed-bytes :decompressed-bytes :ratio} after success.

   Overwrites dst-path if it already exists. Throws if src-path
   does not exist."
  ([src-path] (unpack-file! src-path (edn-path src-path)))
  ([src-path dst-path]
   (when-not (exists? src-path)
     (throw (ex-info "Compressed source missing or empty"
                     {:source src-path})))
   (with-open [in  (GZIPInputStream.
                    (FileInputStream. (file src-path))
                    buffer-size)
               out (FileOutputStream. (file dst-path))]
     (io/copy in out :buffer-size buffer-size))
   (let [comp (.length (file src-path))
         orig (.length (file dst-path))]
     {:source src-path
      :destination dst-path
      :compressed-bytes comp
      :decompressed-bytes orig
      :ratio (if (pos? comp) (double (/ orig comp)) 0.0)})))

(defn ensure-unpacked!
  "If edn-path exists, returns nil (already unpacked).
   If edn-path is missing but its .gz companion exists,
   unpacks the .gz and returns the unpack-file! result map.
   If neither exists, returns nil.

   This is the bootstrap helper for refresh-all-attributes!.
   Call it on every canonical file before loading; if a fresh
   clone has only .gz files committed, the .edn versions will
   materialize on first use."
  [edn-target]
  (cond
    (exists? edn-target)
    nil

    (exists? (gz-path edn-target))
    (unpack-file! (gz-path edn-target) edn-target)

    :else
    nil))

(defn ensure-all-unpacked!
  "Bootstrap helper for a directory of EDN files. Walks the
   given directory, finds every .edn.gz, and ensures its
   .edn companion exists.

   Returns a vector of unpack-file! result maps for every file
   that was actually decompressed. Returns an empty vector if
   nothing needed action."
  [dir-path]
  (let [d (file dir-path)]
    (when (.isDirectory d)
      (->> (file-seq d)
           (filter (fn [^File f]
                     (and (.isFile f)
                          (.endsWith (.getName f) ".edn.gz"))))
           (keep (fn [^File gz]
                   (let [edn-companion (edn-path (.getPath gz))]
                     (ensure-unpacked! edn-companion))))
           vec))))

(defn pack-all!
  "Walk dir-path, find every .edn larger than threshold-bytes
   (default 10 MB), and pack it. Skips files that already have
   a .gz companion newer than the source.

   Returns a vector of pack-file! result maps for every file
   that was actually compressed. Use for a fresh-install pass
   that prepares an entire directory tree for commit."
  ([dir-path] (pack-all! dir-path (* 10 1024 1024)))
  ([dir-path threshold-bytes]
   (let [d (file dir-path)]
     (when (.isDirectory d)
       (->> (file-seq d)
            (filter (fn [^File f]
                      (and (.isFile f)
                           (.endsWith (.getName f) ".edn")
                           (>= (.length f) threshold-bytes))))
            (keep (fn [^File f]
                    (let [src (.getPath f)
                          dst (gz-path src)
                          dst-f (file dst)
                          fresh? (and (.exists dst-f)
                                      (>= (.lastModified dst-f)
                                          (.lastModified f)))]
                      (when-not fresh?
                        (pack-file! src)))))
            vec)))))