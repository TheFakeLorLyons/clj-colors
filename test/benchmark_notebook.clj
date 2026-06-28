(ns benchmark-notebook
  "Visualization notebook for LLM color-proposal benchmarks. Generic
   over reference and proposal data, currently configured for flags.

   Render with:
     (require '[scicloj.clay.v2.api :as clay])
     (clay/make! {:source-path \"notebooks/benchmark.clj\"
                  :hide-info-line true})"
  (:require [benchmark :as bench]
            [clj-colors.color :as color]
            [clj-colors.associations :as associations]
            [scicloj.clay.v2.api :as clay]
            [scicloj.kindly.v4.kind :as kind]
            [flags-test :refer [reference]]))

;; ── Setup ─────────────────────────────────────────────────────────

(def scores (bench/score-all reference @associations/data))
(def stats (bench/summary scores))
(def valid-scores (remove #(= :no-proposal (:status %)) scores))

(defn pct [x] (format "%.1f%%" (* 100.0 (double x))))

;; ── Header ────────────────────────────────────────────────────────

(kind/md "# Flag Benchmark — LLM Accuracy Report")

(kind/md
 (str "Scoring " (count valid-scores) " of " (count scores)
      " LLM-authored flag associations against authoritative "
      "Pantone/RGB specifications from encycolorpedia."))

;; ── Summary card ──────────────────────────────────────────────────

(kind/hiccup
 [:div {:style (str "display:grid;grid-template-columns:repeat(4,1fr);"
                    "gap:14px;padding:16px;background:#1d1d23;"
                    "border-radius:8px;font-family:monospace;color:#ddd;")}
  [:div
   [:div {:style "opacity:0.6;font-size:11px;text-transform:uppercase"} "Mean recall"]
   [:div {:style "font-size:24px;color:#7ec88f"} (pct (:mean-recall stats))]
   [:div {:style "font-size:11px;opacity:0.7"}
    "True colors hit (unweighted)"]]
  [:div
   [:div {:style "opacity:0.6;font-size:11px;text-transform:uppercase"} "Mean precision"]
   [:div {:style "font-size:24px;color:#7eb09a"} (pct (:mean-precision stats))]
   [:div {:style "font-size:11px;opacity:0.7"}
    "Proposed colors valid"]]
  [:div
   [:div {:style "opacity:0.6;font-size:11px;text-transform:uppercase"} "Mean F1"]
   [:div {:style "font-size:24px;color:#a07ee0"} (pct (:mean-f1 stats))]
   [:div {:style "font-size:11px;opacity:0.7"}
    "Harmonic mean"]]
  [:div
   [:div {:style "opacity:0.6;font-size:11px;text-transform:uppercase"} "Weighted coverage"]
   [:div {:style "font-size:24px;color:#4eb8b8"} (pct (:mean-coverage stats))]
   [:div {:style "font-size:11px;opacity:0.7"}
    "Visually important colors"]]])

;; ── Status distribution ───────────────────────────────────────────

(kind/md "## Pass / fail distribution")

(kind/vega-lite
 {:data {:values (mapv (fn [[status n]]
                         {:status (name status) :count n})
                       (:status-counts stats))}
  :mark "bar"
  :encoding {:x {:field "status" :type "nominal"
                 :sort ["pass" "strong" "partial" "fail"]}
             :y {:field "count" :type "quantitative" :title "Number of flags"}
             :color {:field "status" :type "nominal"
                     :scale {:domain ["pass" "strong" "partial" "fail"]
                             :range ["#7ec88f" "#7eb09a" "#cdb86a" "#c87e7e"]}}}
  :width 500 :height 200})

;; ── Per-flag accuracy bar chart ──────────────────────────────────

(kind/md "## Per-flag accuracy")

(kind/md
 (str "Recall, precision, and F1 for every flag, sorted by F1. "
      "Recall = fraction of true colors the LLM found. "
      "Precision = fraction of proposed colors that landed on a true color. "
      "F1 = harmonic mean of the two."))

(kind/vega-lite
 {:data {:values (mapcat (fn [s]
                           [{:spec (name (:spec-key s)) :metric "recall"
                             :value (* 100.0 (:recall s)) :f1 (:f1 s)}
                            {:spec (name (:spec-key s)) :metric "precision"
                             :value (* 100.0 (:precision s)) :f1 (:f1 s)}
                            {:spec (name (:spec-key s)) :metric "f1"
                             :value (* 100.0 (:f1 s)) :f1 (:f1 s)}])
                         valid-scores)}
  :mark "bar"
  :encoding {:y {:field "spec" :type "nominal"
                 :sort {:field "f1" :order "descending"}
                 :title nil}
             :x {:field "value" :type "quantitative"
                 :title "Accuracy (%)" :scale {:domain [0 100]}}
             :color {:field "metric" :type "nominal"
                     :scale {:domain ["recall" "precision" "f1"]
                             :range ["#7ec88f" "#7eb09a" "#a07ee0"]}}
             :row {:field "metric" :type "nominal" :title nil}}
  :resolve {:scale {:y "independent"}}
  :width 500 :height {:step 14}})

;; ── Coverage vs precision scatter ────────────────────────────────

(kind/md "## Coverage vs precision")

(kind/md
 (str "Each point is one flag. The top-right corner is the goal: "
      "high recall (got the right colors) and high precision "
      "(didn't add wrong ones). Anything below the diagonal is "
      "over-proposing; anything above is under-proposing."))

(kind/vega-lite
 {:data {:values (mapv (fn [s]
                         {:spec (name (:spec-key s))
                          :recall (:recall s)
                          :precision (:precision s)
                          :tier (str (:tier s))
                          :f1 (:f1 s)
                          :n-expected (:n-expected s)
                          :n-proposed (:n-proposed s)})
                       valid-scores)}
  :layer
  [{:mark {:type "rule" :strokeDash [4 4] :opacity 0.3}
    :data {:values [{:x 0 :y 0} {:x 1 :y 1}]}
    :encoding {:x {:field "x" :type "quantitative"}
               :y {:field "y" :type "quantitative"}}}
   {:mark {:type "point" :filled true :size 120}
    :encoding {:x {:field "recall" :type "quantitative"
                   :scale {:domain [0 1]}
                   :title "Recall (fraction of true colors hit)"}
               :y {:field "precision" :type "quantitative"
                   :scale {:domain [0 1]}
                   :title "Precision (fraction of proposed colors valid)"}
               :color {:field "tier" :type "nominal"
                       :scale {:range ["#a07ee0" "#7eb09a" "#cdb86a"]}}
               :tooltip [{:field "spec"} {:field "recall" :format ".2f"}
                         {:field "precision" :format ".2f"}
                         {:field "f1" :format ".2f"}
                         {:field "n-expected"} {:field "n-proposed"}]}}]
  :width 500 :height 400})

;; ── OKLAB chromatic projection (per flag) ────────────────────────

(kind/md "## Per-flag chromatic projection")

(kind/md
 (str "Each flag's expected and proposed colors plotted in OKLAB "
      "chromaticity. The a-axis runs green to red; the b-axis runs "
      "blue to yellow. Circles are reference colors; triangles are "
      "what the LLM proposed. Distance between matched pairs shows "
      "perceptual error."))

(defn chromatic-chart [score]
  (let [data (bench/oklab-projection score)
        spec-name (name (:spec-key score))]
    (kind/vega-lite
     {:title {:text spec-name :anchor "start" :fontSize 14
              :subtitle (format "F1: %.1f%% · recall: %.1f%% · precision: %.1f%%"
                                (* 100.0 (:f1 score))
                                (* 100.0 (:recall score))
                                (* 100.0 (:precision score)))}
      :data {:values data}
      :mark {:type "point" :filled true :opacity 0.8}
      :encoding {:x {:field "a" :type "quantitative"
                     :scale {:domain [-0.3 0.3]}
                     :title "a (green ← → red)"}
                 :y {:field "b" :type "quantitative"
                     :scale {:domain [-0.3 0.3]}
                     :title "b (blue ← → yellow)"}
                 :color {:field "hex" :type "nominal" :scale nil :legend nil}
                 :shape {:field "role" :type "nominal"
                         :scale {:range ["circle" "triangle"]}}
                 :size {:field "weight" :type "quantitative"
                        :scale {:range [50 800]} :legend nil}
                 :tooltip [{:field "hex"} {:field "role"}
                           {:field "weight" :format ".3f"}]}
      :width 280 :height 280})))

;; Show chromatic charts for the most interesting cases
(kind/md "### Best performers")
(into [kind/fragment]
      (map chromatic-chart
           (take 3 (bench/rank-by-metric valid-scores :f1))))

(kind/md "### Worst performers")
(into [kind/fragment]
      (map chromatic-chart
           (take 3 (bench/rank-by-metric valid-scores :f1 :asc))))

;; ── Per-flag swatch comparison ───────────────────────────────────

(kind/md "## Side-by-side swatches")

(defn swatch-comparison [score]
  (let [spec-name (name (:spec-key score))
        expected (->> (:expected-matches score)
                      (sort-by :expected-weight >)
                      (take 12))
        proposed (->> (:proposed-matches score)
                      (sort-by :proposed-weight >)
                      (take 12))
        bar-w 32 bar-h 36 gap 2]
    (kind/hiccup
     [:div {:style "margin:16px 0;padding:12px;background:#1d1d23;border-radius:6px;color:#ddd;font-family:monospace"}
      [:div {:style "display:flex;justify-content:space-between;margin-bottom:8px;font-size:12px"}
       [:strong spec-name]
       [:span {:style "opacity:0.7"}
        (format "F1 %.1f%% · recall %.1f%% · precision %.1f%%"
                (* 100.0 (:f1 score)) (* 100.0 (:recall score))
                (* 100.0 (:precision score)))]]
      [:div {:style "display:grid;grid-template-columns:80px 1fr;gap:8px;align-items:center"}
       [:div {:style "font-size:10px;opacity:0.6;text-transform:uppercase"} "expected"]
       [:div {:style "display:flex;gap:2px;flex-wrap:wrap"}
        (for [m expected]
          [:div {:title (str (:expected-hex m) " · weight "
                             (format "%.2f" (:expected-weight m))
                             (if (:hit? m)
                               (format " · hit (Δ %.3f)" (:distance m))
                               (format " · miss (Δ %.3f)" (:distance m))))
                 :style (str "width:" bar-w "px;height:" bar-h "px;"
                             "background:" (:expected-hex m) ";"
                             "border:2px solid "
                             (if (:hit? m) "#7ec88f" "#c87e7e")
                             ";border-radius:3px")}])]
       [:div {:style "font-size:10px;opacity:0.6;text-transform:uppercase"} "proposed"]
       [:div {:style "display:flex;gap:2px;flex-wrap:wrap"}
        (for [m proposed]
          [:div {:title (str (:proposed-hex m) " · weight "
                             (format "%.2f" (:proposed-weight m))
                             (if (:justified? m)
                               (format " · justified (Δ %.3f)" (:distance m))
                               (format " · hallucinated (Δ %.3f)" (:distance m))))
                 :style (str "width:" bar-w "px;height:" bar-h "px;"
                             "background:" (:proposed-hex m) ";"
                             "border:2px solid "
                             (if (:justified? m) "#7ec88f" "#cdb86a")
                             ";border-radius:3px")}])]]])))

(kind/md
 (str "Green border = hit (within tolerance). Red border = miss "
      "(expected color the LLM did not find). Yellow border = "
      "hallucinated (proposed color with no matching reference). "
      "Hover any swatch for hex, weight, and distance."))

(kind/md "### Best three")
(into [kind/fragment]
      (map swatch-comparison
           (take 3 (bench/rank-by-metric valid-scores :f1))))

(kind/md "### Worst three")
(into [kind/fragment]
      (map swatch-comparison
           (take 3 (bench/rank-by-metric valid-scores :f1 :asc))))

;; ── Tier breakdown ───────────────────────────────────────────────

(when (:by-tier stats)
  (kind/md "## Tier breakdown")
  (kind/md
   (str "Tier 1 covers simple two- or three-color flags; Tier 2 "
        "distinctive multi-color designs; Tier 3 culturally specific "
        "or visually unusual. Pass rate per tier:"))
  (kind/vega-lite
   {:data {:values (mapv (fn [[tier s]]
                           {:tier (name tier)
                            :pass-rate (* 100.0 (:pass-rate s))
                            :mean-f1 (* 100.0 (:mean-f1 s))
                            :n (:n s)})
                         (:by-tier stats))}
    :layer
    [{:mark "bar"
      :encoding {:x {:field "tier" :type "nominal"}
                 :y {:field "pass-rate" :type "quantitative"
                     :title "Pass rate (%)" :scale {:domain [0 100]}}
                 :color {:value "#7eb09a"}}}
     {:mark {:type "text" :dy -8 :fontSize 12}
      :encoding {:x {:field "tier" :type "nominal"}
                 :y {:field "pass-rate" :type "quantitative"}
                 :text {:field "pass-rate" :format ".0f"}}}]
    :width 400 :height 250}))

;; ── Distance distribution ────────────────────────────────────────

(kind/md "## How close were the matches?")

(kind/md
 (str "For each pair where the LLM hit a true color, how close did "
      "it land in OKLAB space? Tolerance is roughly 0.05-0.07; the "
      "left edge of this distribution is where you want to be."))

(kind/vega-lite
 {:data {:values (mapv (fn [s] {:spec (name (:spec-key s))
                                :mean-distance (:mean-hit-distance s)})
                       (filter #(pos? (:n-hits %)) valid-scores))}
  :mark "bar"
  :encoding {:x {:field "mean-distance" :type "quantitative"
                 :bin {:maxbins 20}
                 :title "Mean OKLAB distance for matched pairs"}
             :y {:aggregate "count" :title "Number of flags"}}
  :width 500 :height 250})

;; ── Worst failures table ─────────────────────────────────────────

(kind/md "## Worst failures, full detail")

(kind/table
 {:column-names ["Flag" "F1" "Recall" "Precision" "Hallucination" "n-expected" "n-proposed"]
  :row-vectors
  (for [s (bench/worst-failures scores 10)]
    [(name (:spec-key s))
     (format "%.2f" (:f1 s))
     (format "%.2f" (:recall s))
     (format "%.2f" (:precision s))
     (format "%.2f" (:hallucination-rate s))
     (:n-expected s)
     (:n-proposed s)])})

(comment
(clay/make!
 {:source-path "test/benchmark.clj"
  :format [:html]
  :hide-info-line true
  :hide-ui-header true
  :show false
  :run-quarto false}))