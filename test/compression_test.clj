(ns compression-test
  "Round-trip and bootstrap tests for the compression namespace.
   Uses tempfiles exclusively so the suite has no side effects on
   the real resources/ tree."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clj-colors.compression :as compression]))

(defn- temp-dir []
  (let [d (java.io.File/createTempFile "compression-test" "")]
    (.delete d)
    (.mkdirs d)
    (.deleteOnExit d)
    d))

(defn- temp-edn
  "Create a temp file with the given content and return its path.
   The file is registered for deletion at JVM exit."
  [content suffix]
  (let [f (java.io.File/createTempFile "compression-test" suffix)]
    (spit f content)
    (.deleteOnExit f)
    (str f)))

(def sample-edn
  "Representative sample with repeated keys and namespaced
   keywords, matching what color_tags.edn actually contains."
  (str "{\n"
       (apply str
              (for [i (range 100)]
                (format "  :sample/entry-%d {:weight 1.0 :tags #{:foo :bar :baz} :colors {\"#abcdef\" 0.5 \"#012345\" 0.3 \"#fedcba\" 0.2}}\n"
                        i)))
       "}\n"))

(deftest path-conversion-roundtrips
  (testing "gz-path appends .gz to bare .edn"
    (is (= "foo.edn.gz" (compression/gz-path "foo.edn"))))

  (testing "gz-path is idempotent on already-gz paths"
    (is (= "foo.edn.gz" (compression/gz-path "foo.edn.gz"))))

  (testing "edn-path strips .gz suffix"
    (is (= "foo.edn" (compression/edn-path "foo.edn.gz"))))

  (testing "edn-path is idempotent on bare .edn paths"
    (is (= "foo.edn" (compression/edn-path "foo.edn"))))

  (testing "gz then edn-path returns original"
    (is (= "bar.edn"
           (compression/edn-path
            (compression/gz-path "bar.edn"))))))

(deftest pack-unpack-roundtrip
  (testing "small EDN round-trips byte-for-byte"
    (let [src (temp-edn sample-edn ".edn")
          gz  (compression/gz-path src)]
      (compression/pack-file! src)
      (is (compression/exists? gz))

      (let [recovered (temp-edn "" ".edn")]
        (compression/unpack-file! gz recovered)
        (is (= sample-edn (slurp recovered)))))))

(deftest pack-returns-stats
  (testing "pack returns size and ratio info"
    (let [src (temp-edn sample-edn ".edn")
          result (compression/pack-file! src)]
      (is (= src (:source result)))
      (is (= (str src ".gz") (:destination result)))
      (is (pos? (:original-bytes result)))
      (is (pos? (:compressed-bytes result)))
      (is (>= (:ratio result) 1.0)
          "EDN with repeated keys should compress, not expand"))))

(deftest unpack-returns-stats
  (testing "unpack returns size and ratio info"
    (let [src (temp-edn sample-edn ".edn")
          gz  (compression/gz-path src)]
      (compression/pack-file! src)
      (let [recovered (temp-edn "" ".edn")
            result (compression/unpack-file! gz recovered)]
        (is (= gz (:source result)))
        (is (= recovered (:destination result)))
        (is (pos? (:compressed-bytes result)))
        (is (pos? (:decompressed-bytes result)))))))

(deftest pack-missing-source-throws
  (testing "packing a nonexistent file throws ex-info"
    (is (thrown? clojure.lang.ExceptionInfo
                 (compression/pack-file! "/no/such/path.edn")))))

(deftest unpack-missing-source-throws
  (testing "unpacking a nonexistent file throws ex-info"
    (is (thrown? clojure.lang.ExceptionInfo
                 (compression/unpack-file! "/no/such/path.edn.gz")))))

(deftest empty-file-treated-as-missing
  (testing "zero-byte file is treated as missing for pack"
    (let [empty-src (temp-edn "" ".edn")]
      (is (thrown? clojure.lang.ExceptionInfo
                   (compression/pack-file! empty-src))))))

(deftest ensure-unpacked-noop-when-edn-present
  (testing "no work done when uncompressed file already exists"
    (let [src (temp-edn sample-edn ".edn")]
      (is (nil? (compression/ensure-unpacked! src))))))

(deftest ensure-unpacked-bootstraps-from-gz
  (testing "decompresses when only gz exists"
    (let [src (temp-edn sample-edn ".edn")
          gz  (compression/gz-path src)]
      (compression/pack-file! src)
      (.delete (io/file src))
      (is (not (compression/exists? src)))
      (is (compression/exists? gz))

      (let [result (compression/ensure-unpacked! src)]
        (is (map? result))
        (is (compression/exists? src))
        (is (= sample-edn (slurp src)))))))

(deftest ensure-unpacked-noop-when-nothing-exists
  (testing "returns nil when neither edn nor gz exists"
    (let [missing "/tmp/definitely-does-not-exist.edn"]
      (is (nil? (compression/ensure-unpacked! missing))))))

(deftest ensure-all-unpacked-walks-directory
  (testing "directory walk finds and unpacks every gz"
    (let [dir (temp-dir)
          file-a (.getPath (io/file dir "a.edn"))
          file-b (.getPath (io/file dir "b.edn"))]
      (spit file-a sample-edn)
      (spit file-b sample-edn)
      (compression/pack-file! file-a)
      (compression/pack-file! file-b)
      (.delete (io/file file-a))
      (.delete (io/file file-b))

      (let [results (compression/ensure-all-unpacked! (.getPath dir))]
        (is (= 2 (count results)))
        (is (every? map? results))
        (is (compression/exists? file-a))
        (is (compression/exists? file-b))
        (is (= sample-edn (slurp file-a)))
        (is (= sample-edn (slurp file-b)))))))

(deftest pack-all-respects-threshold
  (testing "small files below threshold are not packed"
    (let [dir (temp-dir)
          small (.getPath (io/file dir "small.edn"))
          large (.getPath (io/file dir "large.edn"))]
      (spit small "{:tiny true}")
      (spit large (apply str (repeat 50000 sample-edn)))
      (let [results (compression/pack-all! (.getPath dir) 100000)]
        (is (= 1 (count results)))
        (is (= large (:source (first results))))
        (is (not (compression/exists? (compression/gz-path small))))
        (is (compression/exists? (compression/gz-path large)))))))

(comment
  (run-tests))