(ns clj-colors.logging
  (:require [clojure.java.io :as io]))

(defn write-edn-map-with-progress!
  "Write a Clojure map to path as EDN, printing one [key value] pair
   at a time and logging progress every progress-every entries.

   Bypasses nrepl's *out* middleware for the actual file writes,
   which avoids stack overflow on large maps. Progress goes to the
   REPL via a separately-bound *out*."
  ([path m label] (write-edn-map-with-progress! path m label 1000))
  ([path m label progress-every]
   (let [total    (count m)
         t0       (System/currentTimeMillis)
         counter  (atom 0)
         repl-out *out*]
     (println (format "Writing %d entries to %s..." total path))
     (with-open [w (java.io.BufferedWriter.
                    (java.io.FileWriter. ^String path))]
       (.write w "{")
       (doseq [[k v] m]
         (.write w (pr-str k))
         (.write w " ")
         (.write w (pr-str v))
         (.write w "\n")
         (let [c (swap! counter inc)]
           (when (zero? (mod c progress-every))
             (binding [*out* repl-out]
               (let [elapsed (- (System/currentTimeMillis) t0)
                     rate    (if (pos? elapsed)
                               (/ (double c) (/ elapsed 1000.0))
                               0.0)
                     eta     (if (pos? rate)
                               (/ (- total c) rate)
                               0.0)]
                 (println (format "  %s: %d/%d (%.0f/s, ETA %.0fs)"
                                  label c total rate eta)))))))
       (.write w "}"))
     (println (format "  %s: wrote %d entries in %.1fs"
                      label total
                      (/ (- (System/currentTimeMillis) t0) 1000.0)))
     :written)))