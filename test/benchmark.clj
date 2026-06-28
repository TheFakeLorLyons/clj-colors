(ns benchmark
  "Generic LLM color-proposal benchmarking. Score any LLM-proposed
   color set against a reference spec, regardless of domain. The
   same machinery works for flags, flowers, brand palettes, gemstones,
   or anything else that fits the {hex weight} data shape.

   Reference shape:
     {<spec-key> {:expected-colors {<hex> <weight>}
                  :tolerance <oklab-distance-threshold>
                  :tier <optional-keyword>
                  :notes <optional-string>
                  :source <optional-string>}}

   Proposal shape (same as associations entries):
     {<spec-key> {:colors {<hex> <weight>}
                  :rationale <string>
                  ...}}

   Per-spec scoring with (score-spec reference proposals :spec/key).
   All specs at once with (score-all reference proposals).
   Aggregate breakdown with (summary scores)."
  (:require [clj-colors.color :as color]))

;; ── Distance and similarity primitives ────────────────────────────

(defn oklab-distance
  "OKLAB euclidean distance between two hex strings. Public so
   notebooks and ad-hoc analysis can use it."
  [hex-a hex-b]
  (let [[la aa ba _] (color/rgba->oklab (color/hex->rgba hex-a))
        [lb ab bb _] (color/rgba->oklab (color/hex->rgba hex-b))
        dl (- la lb) da (- aa ab) db (- ba bb)]
    (Math/sqrt (+ (* dl dl) (* da da) (* db db)))))

(defn gaussian-similarity
  "Smooth 0-to-1 similarity between two colors based on OKLAB
   distance and a tolerance treated as Gaussian standard deviation.
   Returns 1.0 when colors are identical, falls off smoothly with
   distance, hits ~0.6 at the tolerance threshold, near zero at
   2x tolerance.

   This is the answer to 'how close is the proposed color to the
   true color' when you want a continuous score instead of a
   binary hit/miss."
  [hex-a hex-b tolerance]
  (let [d (oklab-distance hex-a hex-b)
        sigma tolerance]
    (Math/exp (- (/ (* d d) (* 2.0 sigma sigma))))))

(defn- match-expected
  "For each expected color, find the proposed color closest in OKLAB.
   Returns rich match records with distance, hit/miss, and weight delta."
  [expected-colors proposed-colors tolerance]
  (let [proposed-hexes (vec (keys proposed-colors))]
    (for [[exp-hex exp-w] expected-colors]
      (let [pairs (mapv (fn [act] [act (oklab-distance exp-hex act)])
                        proposed-hexes)
            best (when (seq pairs) (apply min-key second pairs))
            [near-hex near-dist] best
            near-w (when near-hex (get proposed-colors near-hex))]
        {:expected-hex exp-hex
         :expected-weight exp-w
         :nearest-proposed-hex near-hex
         :nearest-proposed-weight near-w
         :distance near-dist
         :hit? (when near-dist (<= near-dist tolerance))
         :weight-delta (when near-w (Math/abs (- exp-w near-w)))
         :similarity (when near-hex
                       (gaussian-similarity exp-hex near-hex tolerance))}))))

(defn- match-proposed
  "For each proposed color, find the expected color closest in OKLAB.
   Used to identify hallucinations (proposed colors that don't track
   anything in the reference)."
  [proposed-colors expected-colors tolerance]
  (let [expected-hexes (vec (keys expected-colors))]
    (for [[prop-hex prop-w] proposed-colors]
      (let [pairs (mapv (fn [exp] [exp (oklab-distance prop-hex exp)])
                        expected-hexes)
            best (when (seq pairs) (apply min-key second pairs))
            [near-hex near-dist] best]
        {:proposed-hex prop-hex
         :proposed-weight prop-w
         :nearest-expected-hex near-hex
         :distance near-dist
         :justified? (when near-dist (<= near-dist tolerance))
         :similarity (when near-hex
                       (gaussian-similarity prop-hex near-hex tolerance))}))))

;; ── Per-spec scoring ─────────────────────────────────────────────

(defn score-spec
  "Score a single spec key. Returns a rich map of statistics
   suitable for aggregation, charting, and detailed per-spec
   analysis."
  [reference proposals spec-key]
  (let [ref-entry (get reference spec-key)
        proposal (get proposals spec-key)]
    (cond
      (nil? ref-entry)
      {:spec-key spec-key :status :no-reference}

      (nil? proposal)
      {:spec-key spec-key :status :no-proposal}

      :else
      (let [expected (:expected-colors ref-entry)
            actual (:colors proposal)
            tolerance (:tolerance ref-entry 0.05)

            expected-matches (vec (match-expected expected actual tolerance))
            proposed-matches (vec (match-proposed actual expected tolerance))

            ;; Color-count metrics — the raw percentages you asked for
            n-expected (count expected)
            n-proposed (count actual)
            n-hits (count (filter :hit? expected-matches))
            n-misses (- n-expected n-hits)
            n-justified (count (filter :justified? proposed-matches))
            n-hallucinated (- n-proposed n-justified)

            ;; Unweighted set metrics
            recall (if (pos? n-expected)
                     (/ n-hits (double n-expected))
                     0.0)
            precision (if (pos? n-proposed)
                        (/ n-justified (double n-proposed))
                        0.0)
            f1 (if (pos? (+ precision recall))
                 (/ (* 2.0 precision recall) (+ precision recall))
                 0.0)

            ;; Weight-aware metrics (visually important colors matter more)
            total-expected-weight (reduce + 0.0 (vals expected))
            total-actual-weight (reduce + 0.0 (vals actual))
            weighted-coverage (if (pos? total-expected-weight)
                                (/ (reduce + 0.0
                                           (map :expected-weight
                                                (filter :hit? expected-matches)))
                                   total-expected-weight)
                                0.0)
            weighted-precision (if (pos? total-actual-weight)
                                 (/ (reduce + 0.0
                                            (map :proposed-weight
                                                 (filter :justified? proposed-matches)))
                                    total-actual-weight)
                                 0.0)
            hallucination-rate (- 1.0 weighted-precision)

            ;; Continuous similarity scores (Gaussian overlap)
            mean-similarity (if (seq expected-matches)
                              (/ (reduce + 0.0
                                         (keep :similarity expected-matches))
                                 (count expected-matches))
                              0.0)

            ;; Distance distribution stats
            hit-distances (mapv :distance (filter :hit? expected-matches))
            miss-distances (mapv :distance (remove :hit? expected-matches))
            mean-hit-distance (if (seq hit-distances)
                                (/ (reduce + 0.0 hit-distances)
                                   (count hit-distances))
                                0.0)
            max-hit-distance (if (seq hit-distances) (apply max hit-distances) 0.0)
            mean-miss-distance (if (seq miss-distances)
                                 (/ (reduce + 0.0 miss-distances)
                                    (count miss-distances))
                                 0.0)

            ;; Weight matching for the colors we got right
            weight-errors (keep :weight-delta (filter :hit? expected-matches))
            mean-weight-error (if (seq weight-errors)
                                (/ (reduce + 0.0 weight-errors)
                                   (count weight-errors))
                                0.0)

            ;; Grading buckets
            coverage-grade (cond
                             (>= weighted-coverage 0.9) :excellent
                             (>= weighted-coverage 0.7) :good
                             (>= weighted-coverage 0.4) :weak
                             :else :poor)
            precision-grade (cond
                              (<= hallucination-rate 0.15) :clean
                              (<= hallucination-rate 0.35) :loose
                              (<= hallucination-rate 0.55) :noisy
                              :else :inventive)
            status (cond
                     (and (>= weighted-coverage 0.85)
                          (<= hallucination-rate 0.2)) :pass
                     (and (>= weighted-coverage 0.7)
                          (<= hallucination-rate 0.3)) :strong
                     (>= weighted-coverage 0.4) :partial
                     :else :fail)]

        {:spec-key spec-key
         :status status
         :tier (:tier ref-entry)
         :notes (:notes ref-entry)
         :source (:source ref-entry)

         ;; Counts
         :n-expected n-expected
         :n-proposed n-proposed
         :n-hits n-hits
         :n-misses n-misses
         :n-justified n-justified
         :n-hallucinated n-hallucinated

         ;; Set metrics (unweighted percentages)
         :recall recall
         :precision precision
         :f1 f1

         ;; Weighted metrics
         :weighted-coverage weighted-coverage
         :weighted-precision weighted-precision
         :hallucination-rate hallucination-rate

         ;; Continuous similarity
         :mean-similarity mean-similarity

         ;; Distance distribution
         :mean-hit-distance mean-hit-distance
         :max-hit-distance max-hit-distance
         :mean-miss-distance mean-miss-distance
         :tolerance tolerance

         ;; Weight accuracy
         :mean-weight-error mean-weight-error

         ;; Grading
         :coverage-grade coverage-grade
         :precision-grade precision-grade

         ;; Raw matches for charts and detailed views
         :expected-matches expected-matches
         :proposed-matches proposed-matches}))))

(defn score-all
  "Score every spec key in the reference against the proposals."
  [reference proposals]
  (mapv #(score-spec reference proposals %) (keys reference)))

;; ── Aggregate summary ────────────────────────────────────────────

(defn summary
  "Aggregate statistics across all scored specs. Returns nil if no
   specs had proposals."
  [scores]
  (let [valid (remove #(contains? #{:no-proposal :no-reference} (:status %))
                      scores)
        n (count valid)]
    (when (pos? n)
      {:n-scored n
       :n-no-proposal (count (filter #(= :no-proposal (:status %)) scores))
       :mean-recall (/ (reduce + 0.0 (map :recall valid)) n)
       :mean-precision (/ (reduce + 0.0 (map :precision valid)) n)
       :mean-f1 (/ (reduce + 0.0 (map :f1 valid)) n)
       :mean-coverage (/ (reduce + 0.0 (map :weighted-coverage valid)) n)
       :mean-hallucination (/ (reduce + 0.0 (map :hallucination-rate valid)) n)
       :mean-similarity (/ (reduce + 0.0 (map :mean-similarity valid)) n)
       :status-counts (frequencies (map :status valid))
       :coverage-grades (frequencies (map :coverage-grade valid))
       :precision-grades (frequencies (map :precision-grade valid))
       :by-tier (when (some :tier valid)
                  (into {}
                        (for [[tier entries] (group-by :tier valid)]
                          [tier {:n (count entries)
                                 :mean-recall (/ (reduce + 0.0 (map :recall entries))
                                                 (count entries))
                                 :mean-precision (/ (reduce + 0.0 (map :precision entries))
                                                    (count entries))
                                 :mean-f1 (/ (reduce + 0.0 (map :f1 entries))
                                             (count entries))
                                 :mean-coverage (/ (reduce + 0.0
                                                           (map :weighted-coverage entries))
                                                   (count entries))
                                 :mean-hallucination
                                 (/ (reduce + 0.0 (map :hallucination-rate entries))
                                    (count entries))
                                 :pass-rate
                                 (/ (count (filter #(= :pass (:status %)) entries))
                                    (double (count entries)))}])))})))

;; ── Helpers for charts and tables ────────────────────────────────

(defn rank-by-metric
  "Sort scored specs by a single metric, descending by default."
  ([scores metric] (rank-by-metric scores metric :desc))
  ([scores metric direction]
   (let [sorter (if (= direction :asc) < >)]
     (->> (remove #(= :no-proposal (:status %)) scores)
          (sort-by metric sorter)))))

(defn worst-failures
  "Top N specs by lowest F1 score, for drill-down analysis."
  ([scores] (worst-failures scores 10))
  ([scores n]
   (->> scores
        (remove #(contains? #{:no-proposal :no-reference} (:status %)))
        (sort-by :f1)
        (take n))))

(defn oklab-projection
  "Project every color in a score (expected + proposed) into OKLAB
   space. Returns a vector of maps with :hex, :L, :a, :b, :weight,
   :role (:expected or :proposed). Use for scatter plots in
   chromatic space."
  [score]
  (concat
   (for [[hex w] (map (juxt :expected-hex :expected-weight)
                      (:expected-matches score))]
     (let [[L a b _] (color/rgba->oklab (color/hex->rgba hex))]
       {:hex hex :L L :a a :b b :weight w :role "expected"}))
   (for [[hex w] (map (juxt :proposed-hex :proposed-weight)
                      (:proposed-matches score))]
     (let [[L a b _] (color/rgba->oklab (color/hex->rgba hex))]
       {:hex hex :L L :a a :b b :weight w :role "proposed"}))))