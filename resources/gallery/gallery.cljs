(ns gallery
  "Swatch gallery UI, interpreted in the browser by Scittle with the
   reagent plugin. Served by clj-colors.gallery; passes EDN to its API.

   Theming: when the studio is open (and the toggle is on), the page's own
   chrome is colored from the palette being edited, live, by the same
   trick m-trees uses for landscapes: sort the colors by luminance and
   hand out roles by position. Background gets the darkest color, text the
   lightest (with a contrast guard), panels and accents in between."
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.edn :as edn]
            [clojure.string :as str]))

;; State -----------------------------------------------------------------------

(defonce palettes (r/atom []))
(defonce filters  (r/atom {:search "" :category "all" :family "all" :tag "all"
                           :brightness 0 :sort-key :name :desc? false}))
(defonce studio   (r/atom nil))
(defonce theming? (r/atom true))
(defonce status   (r/atom nil))

;; Server I/O ---------------------------------------------------------------------

(defn fetch-palettes! []
  (-> (js/fetch "/api/palettes")
      (.then (fn [resp] (.text resp)))
      (.then (fn [text] (reset! palettes (edn/read-string text))))))

(defn post-edn!
  "POST EDN, read EDN back. Failures of any kind reach the callback as an
   {:error ...} map so the status line always tells the truth."
  [url data callback]
  (-> (js/fetch url (clj->js {:method  "POST"
                              :headers {"Content-Type" "application/edn"}
                              :body    (pr-str data)}))
      (.then (fn [resp] (.text resp)))
      (.then (fn [text]
               (callback (try (edn/read-string text)
                              (catch :default _ {:error text})))))
      (.catch (fn [err] (callback {:error (str err)})))))

;; Color math ----------------------------------------------------------------------

(defn hex->rgb [hex]
  (let [h (str/replace hex #"^#" "")
        h (if (= 3 (count h)) (apply str (mapcat (fn [c] [c c]) h)) h)]
    [(js/parseInt (subs h 0 2) 16)
     (js/parseInt (subs h 2 4) 16)
     (js/parseInt (subs h 4 6) 16)]))

(defn rgb->hex [r g b]
  (let [pair (fn [n]
               (-> (.toString (js/Math.round (max 0 (min 255 n))) 16)
                   (.padStart 2 "0")
                   (.toUpperCase)))]
    (str "#" (pair r) (pair g) (pair b))))

(defn- rgba->parts [hex]
  (let [h (str/replace hex #"^#" "")
        h (if (= 3 (count h)) (apply str (mapcat (fn [c] [c c]) h)) h)]
    [(js/parseInt (subs h 0 2) 16)
     (js/parseInt (subs h 2 4) 16)
     (js/parseInt (subs h 4 6) 16)]))

(defn- valid-hex? [s]
  (re-matches #"#[0-9A-Fa-f]{6}" s))

(defn- srgb->linear [channel]
  (let [c (/ channel 255.0)]
    (if (<= c 0.03928)
      (/ c 12.92)
      (js/Math.pow (/ (+ c 0.055) 1.055) 2.4))))

(defn luminance [hex]
  (let [[r g b] (hex->rgb hex)]
    (+ (* 0.2126 (srgb->linear r))
       (* 0.7152 (srgb->linear g))
       (* 0.0722 (srgb->linear b)))))

;; Gradient model --------------------------------------------------------------------
;; A stop is {:hex \"#RRGGBB\" :weight w :alpha a}. Weights give each color
;; a proportional band of the gradient; its stop sits at the band's center.

(defn positions [stops]
  (let [total (reduce + (map :weight stops))]
    (loop [[s & more] stops cum 0 acc []]
      (if-not s
        acc
        (recur more
               (+ cum (:weight s))
               (conj acc (/ (+ cum (/ (:weight s) 2)) total)))))))

(defn gradient-css [stops direction]
  (let [stops (if (= 1 (count stops)) [(first stops) (first stops)] stops)
        pos   (positions stops)]
    (str "linear-gradient(" direction ", "
         (str/join ", "
                   (map (fn [s p]
                          (let [[r g b] (hex->rgb (:hex s))]
                            (str "rgba(" r "," g "," b ","
                                 (.toFixed (:alpha s) 3) ") "
                                 (.toFixed (* 100 p) 2) "%")))
                        stops pos))
         ")")))

(defn sample-at [stops t]
  (let [pos  (positions stops)
        last-i (dec (count stops))]
    (cond
      (<= t (nth pos 0))
      {:hex (:hex (first stops)) :alpha (:alpha (first stops)) :weight 1}

      (>= t (nth pos last-i))
      {:hex (:hex (nth stops last-i)) :alpha (:alpha (nth stops last-i)) :weight 1}

      :else
      (loop [i 0]
        (if (and (>= t (nth pos i)) (<= t (nth pos (inc i))))
          (let [f (/ (- t (nth pos i)) (- (nth pos (inc i)) (nth pos i)))
                [r1 g1 b1] (hex->rgb (:hex (nth stops i)))
                [r2 g2 b2] (hex->rgb (:hex (nth stops (inc i))))
                a1 (:alpha (nth stops i))
                a2 (:alpha (nth stops (inc i)))]
            {:hex    (rgb->hex (+ r1 (* f (- r2 r1)))
                               (+ g1 (* f (- g2 g1)))
                               (+ b1 (* f (- b2 b1))))
             :alpha  (+ a1 (* f (- a2 a1)))
             :weight 1})
          (recur (inc i)))))))

(def curves
  {"linear"      identity
   "ease-in"     (fn [t] (* t t))
   "ease-out"    (fn [t] (- 1 (* (- 1 t) (- 1 t))))
   "ease-in-out" (fn [t] (if (< t 0.5)
                           (* 2 t t)
                           (- 1 (/ (js/Math.pow (- (* 2 t) 2) 2) 2))))
   "exponential" (fn [t] (if (<= t 0) 0 (js/Math.pow 2 (* 10 (- t 1)))))
   "logarithmic" (fn [t] (min 1 (max 0 (/ (js/Math.log (+ 1 (* 9 t)))
                                          (js/Math.log 10)))))
   "sqrt"        (fn [t] (js/Math.sqrt t))})

;; Studio operations ------------------------------------------------------------------

(defn open-studio! [p]
  (let [stops (mapv (fn [h] {:hex h :weight 1 :alpha 1}) (:hex p))]
    (reset! studio {:key        (:key p)
                    :name-input (str "studio/" (name (:key p)))
                    :save-path  "palettes.edn"
                    :bake-n     5
                    :fade       {:dir "out" :curve "linear" :min 0}
                    :orig       stops
                    :stops      stops})))


(defn blank-stop [h]
  {:hex (str/upper-case (subs h 0 7)) :weight 1 :alpha 1
   :oklab nil :oklch nil})

(defn refresh-stop!
  "Ask the server for hex's oklab/oklch and cache them on stop i."
  [i hex]
  (post-edn! "/api/convert" {:hex hex}
             (fn [{:keys [oklab oklch error]}]
               (when-not error
                 (swap! studio update-in [:stops i] assoc
                        :oklab oklab :oklch oklch)))))

(defn refresh-all! []
  (doseq [[i s] (map-indexed vector (:stops @studio))]
    (refresh-stop! i (:hex s))))

(defn- commit-hex!
  "Write a #RRGGBB color to stop i and resync its perceptual caches."
  [i hex]
  (let [hex (str/upper-case hex)]
    (swap! studio assoc-in [:stops i :hex] hex)
    (refresh-stop! i hex)))

(defn- set-rgb! [i s idx raw]
  (let [n   (js/parseInt raw 10)
        v   (if (js/isNaN n) 0 (max 0 (min 255 n)))
        rgb (assoc (hex->rgb (:hex s)) idx v)]
    (commit-hex! i (apply rgb->hex rgb))))

(defn- set-oklch! [i s idx raw]
  (let [n (js/parseFloat raw)]
    (when-not (js/isNaN n)
      (let [oklch (assoc (:oklch s) idx n)]
        (swap! studio assoc-in [:stops i :oklch] oklch)
        (post-edn! "/api/convert" {:oklch oklch}
                   (fn [{:keys [hex oklab error]}]
                     (when-not error
                       (swap! studio update-in [:stops i] assoc
                              :hex (str/upper-case hex) :oklab oklab))))))))

(defn- set-oklab! [i s idx raw]
  (let [n (js/parseFloat raw)]
    (when-not (js/isNaN n)
      (let [oklab (assoc (:oklab s) idx n)]
        (swap! studio assoc-in [:stops i :oklab] oklab)
        (post-edn! "/api/convert" {:oklab oklab}
                   (fn [{:keys [hex oklch error]}]
                     (when-not error
                       (swap! studio update-in [:stops i] assoc
                              :hex (str/upper-case hex) :oklch oklch))))))))

(defn open-studio! [p]
  (let [stops (mapv blank-stop (:hex p))]
    (reset! studio {:key        (:key p)
                    :name-input (str "studio/" (name (:key p)))
                    :save-path  "palettes.edn"
                    :bake-n     5
                    :fade       {:dir "out" :curve "linear" :min 0}
                    :orig       stops
                    :stops      stops})
    (refresh-all!)))

(defn apply-fade! []
  (swap! studio
         (fn [{:keys [stops fade] :as st}]
           (let [f     (get curves (:curve fade))
                 d     (max 1 (dec (count stops)))
                 min-a (:min fade)]
             (assoc st :stops
                    (vec (map-indexed
                          (fn [i s]
                            (let [t  (/ i d)
                                  tt (if (= "in" (:dir fade)) t (- 1 t))]
                              (assoc s :alpha
                                     (min 1 (max 0 (+ min-a (* (- 1 min-a) (f tt))))))))
                          stops)))))))

(defn bake! []
  (swap! studio
         (fn [{:keys [stops bake-n] :as st}]
           (let [n (max 2 (min 12 bake-n))]
             (assoc st :stops
                    (mapv (fn [i] (sample-at stops (/ i (dec n))))
                          (range n)))))))

(defn reset-stops! []
  (swap! studio (fn [st] (assoc st :stops (:orig st)))))

(defn stop->hex [s]
  (let [base (str/upper-case (subs (:hex s) 0 7))]
    (if (>= (:alpha s) 0.999)
      base
      (str base (-> (.toString (js/Math.round (* 255 (:alpha s))) 16)
                    (.padStart 2 "0")
                    (.toUpperCase))))))

(defn save-palette!
  "One action: register the design in the running registry AND persist it
   to the registry file, which the server updates surgically (only this
   entry is added or replaced; the file's index, comments, and formatting
   are untouched)."
  []
  (let [{:keys [stops name-input save-path]} @studio
        nm (if (str/includes? name-input "/")
             name-input
             (str "studio/" name-input))]
    (post-edn! "/api/register"
               {:key    (keyword nm)
                :colors (mapv stop->hex stops)
                :tags   ["studio"]
                :path   save-path}
               (fn [resp]
                 (if (:error resp)
                   (reset! status (str "error: " (:error resp)))
                   (do (reset! status (str "saved " (:registered resp)
                                           " to registry and " (:saved resp)))
                       (fetch-palettes!)))))))

(defn sync-registry! []
  (post-edn! "/api/save"
             {:path (:save-path @studio)}
             (fn [resp]
               (reset! status
                       (if (:error resp)
                         (str "error: " (:error resp))
                         (str "registry synced to " (:saved resp)))))))

(defn delete-palette!
  "Remove the palette open in the studio from the running registry and,
   surgically, from the registry file. Confirmed first; this is the one
   destructive action in the gallery."
  []
  (let [{:keys [key save-path]} @studio]
    (when (js/confirm (str "Delete " key " from the registry and from "
                           save-path "?"))
      (post-edn! "/api/unregister"
                 {:key key :path save-path}
                 (fn [resp]
                   (if (:error resp)
                     (reset! status (str "error: " (:error resp)))
                     (do (reset! status
                                 (str "deleted " (:unregistered resp)
                                      (if (:removed resp)
                                        (str " and removed from " (:removed resp))
                                        " (registry only; not in file)")))
                         (reset! studio nil)
                         (fetch-palettes!))))))))

;; Theming ----------------------------------------------------------------------------
;; The same luminance-sort role mapping m-trees uses for landscapes,
;; pointed at the page's own chrome.

(defn theme-from [hexes]
  (let [sorted (vec (sort-by luminance hexes))
        n      (count sorted)
        at     (fn [t] (nth sorted (js/Math.round (* t (dec n)))))
        bg     (at 0)
        text0  (at 1)
        text   (if (< (- (luminance text0) (luminance bg)) 0.35)
                 (if (< (luminance bg) 0.4) "#ECECEC" "#141414")
                 text0)]
    {"--sg-bg"     bg
     "--sg-panel"  (at 0.22)
     "--sg-border" (at 0.45)
     "--sg-accent" (at 0.75)
     "--sg-text"   text}))

(defn theme-vars []
  (if (and @theming? @studio)
    (theme-from (map :hex (:stops @studio)))
    {}))

;; Components ---------------------------------------------------------------------------

(defn- target-value [e] (.. e -target -value))

(defn- options-of [k]
  (->> @palettes (keep k) (map name) distinct sort))

(defn- all-tags []
  (->> @palettes (mapcat :tags) distinct sort))

(defn filtered []
  (let [{:keys [search category family tag brightness sort-key desc?]} @filters
        q  (str/lower-case search)
        xs (filter (fn [p]
                     (and (or (= "all" category)
                              (= category (some-> (:category p) name)))
                          (or (= "all" family)
                              (= family (some-> (:family p) name)))
                          (or (= "all" tag)
                              (some (fn [t] (= t tag)) (:tags p)))
                          (>= (or (:brightness p) 0) brightness)
                          (or (str/blank? q)
                              (str/includes? (str/lower-case (str (:key p))) q)
                              (some (fn [t] (str/includes? t q)) (:tags p)))))
                   @palettes)
        xs (if (= :name sort-key)
             (sort-by (fn [p] (str (:key p))) xs)
             (sort-by (fn [p] (or (get p sort-key) 0)) xs))]
    (vec (if desc? (reverse xs) xs))))

(defn controls []
  (let [f @filters]
    [:div.sg-controls
     [:label "search "
      [:input {:type "text" :size 12 :value (:search f)
               :on-change (fn [e] (swap! filters assoc :search (target-value e)))}]]
     [:label "category "
      [:select {:value (:category f)
                :on-change (fn [e] (swap! filters assoc :category (target-value e)))}
       [:option {:value "all"} "all"]
       (for [c (options-of :category)] ^{:key c} [:option {:value c} c])]]
     [:label "family "
      [:select {:value (:family f)
                :on-change (fn [e] (swap! filters assoc :family (target-value e)))}
       [:option {:value "all"} "all"]
       (for [c (options-of :family)] ^{:key c} [:option {:value c} c])]]
     [:label "tag "
      [:select {:value (:tag f)
                :on-change (fn [e] (swap! filters assoc :tag (target-value e)))}
       [:option {:value "all"} "all"]
       (for [t (all-tags)] ^{:key t} [:option {:value t} t])]]
     [:label "brightness >= " (.toFixed (:brightness f) 2)
      [:input {:type "range" :min 0 :max 1 :step 0.05 :value (:brightness f)
               :on-change (fn [e] (swap! filters assoc :brightness
                                         (js/parseFloat (target-value e))))}]]
     [:label "sort "
      [:select {:value (name (:sort-key f))
                :on-change (fn [e] (swap! filters assoc :sort-key
                                          (keyword (target-value e))))}
       (for [s ["name" "brightness" "temperature" "saturation" "contrast"]]
         ^{:key s} [:option {:value s} s])]]
     [:label
      [:input {:type "checkbox" :checked (:desc? f)
               :on-change (fn [_] (swap! filters update :desc? not))}]
      " desc"]
     [:label
      [:input {:type "checkbox" :checked @theming?
               :on-change (fn [_] (swap! theming? not))}]
      " theme from palette"]
     [:span.sg-count (str (count (filtered)) " palettes")]]))

(defn card [p]
  [:div.sg-card {:on-click (fn [_] (open-studio! p))}
   [:div.sg-bar
    {:style {:background (str "linear-gradient(to right, "
                              (str/join ", " (:hex p)) ")")}}]
   [:div.sg-name (str (:key p))]
   [:div.sg-tagline (str/join ", " (take 4 (:tags p)))]])

(defn grid []
  [:div.sg-grid
   (for [p (filtered)]
     ^{:key (str (:key p))} [card p])])

(defn synced-slider []
  (let [local (r/atom nil)]
    (fn [{:keys [min max step value on-change class]}]
      (let [loaded? (some? value)
            display (if (some? @local)
                      @local
                      (when loaded? (str value)))]
        [:span.sg-synced

         [:input {:type      "range"
                  :class     class
                  :min       min
                  :max       max
                  :step      step
                  :value     (if loaded? value min)
                  :disabled  (not loaded?)
                  :on-change (fn [e]
                               (let [v (js/parseFloat (target-value e))]
                                 (reset! local (str v))
                                 (on-change v)))}]

         (if loaded?
           [:input {:type        "number"
                    :class       "sg-slider-num"
                    :min         min
                    :max         max
                    :step        step
                    :value       display

                    :on-change
                    (fn [e]
                      (reset! local (target-value e)))

                    :on-blur
                    (fn [e]
                      (let [v (js/parseFloat (target-value e))]
                        (when-not (js/isNaN v)
                          (let [clamped (max min (min max v))]
                            (reset! local nil)
                            (on-change clamped)))))

                    :on-key-down
                    (fn [e]
                      (when (= "Enter" (.-key e))
                        (let [v (js/parseFloat (target-value e))]
                          (when-not (js/isNaN v)
                            (let [clamped (max min (min max v))]
                              (reset! local nil)
                              (on-change clamped))))))}]

           [:span.sg-slider-placeholder "—"])]))))

(defn fmt-sig-dig [x]
  (let [n (js/Number x)]
    (if (< (js/Math.abs n) 1)
      (.toFixed n 3)   ; small values get more precision
      (.toFixed n 2))))

(defn stop-row [i s]
  (let [hex        (subs (:hex s) 0 7)
        [r g b]    (hex->rgb hex)
        [oL oa ob] (:oklab s)
        [cL cC cH] (:oklch s)]
    ; debug
    ; (js/console.log "oklch" (clj->js (:oklch s)) "cL" cL "some?" (some? cL))
    [:div.sg-stop
     [:div.sg-row
      [:input {:type "color" :value hex
               :on-change (fn [e] (commit-hex! i (target-value e)))}]
      [:input.sg-hex {:type "text" :value hex
                      :on-change (fn [e]
                                   (let [raw (target-value e)
                                         val (if (str/starts-with? raw "#") raw (str "#" raw))]
                                     (when (valid-hex? val)
                                       (commit-hex! i val))))}]
      [:label "r" [:input.sg-num {:type "number" :min 0 :max 255 :value r
                                  :on-change (fn [e] (set-rgb! i s 0 (target-value e)))}]]
      [:label "g" [:input.sg-num {:type "number" :min 0 :max 255 :value g
                                  :on-change (fn [e] (set-rgb! i s 1 (target-value e)))}]]
      [:label "b" [:input.sg-num {:type "number" :min 0 :max 255 :value b
                                  :on-change (fn [e] (set-rgb! i s 2 (target-value e)))}]]
      [:label "weight"
       [synced-slider {:min 0.1 :max 4 :step 0.05 :value (:weight s)
                       :on-change (fn [v] (swap! studio assoc-in [:stops i :weight] v))}]]
      [:label "alpha"
       [synced-slider {:min 0 :max 1 :step 0.01 :value (:alpha s)
                       :on-change (fn [v] (swap! studio assoc-in [:stops i :alpha] v))}]]
      ;; oklch
      [:span.sg-oklch
       [:span.sg-tag "oklch"]
       [:label "L" [synced-slider {:min 0 :max 1 :step 0.005 :value (fmt-sig-dig cL)
                                   :on-change (fn [v] (set-oklch! i s 0 v))}]]
       [:label "C" [synced-slider {:min 0 :max 0.37 :step 0.002 :value (fmt-sig-dig cC)
                                   :on-change (fn [v] (set-oklch! i s 1 v))}]]
       [:label "H" [synced-slider {:min 0 :max 360 :step 1 :value (fmt-sig-dig cH)
                                   :on-change (fn [v] (set-oklch! i s 2 v))}]]]
      ;; oklab
      [:span.sg-oklab
       [:span.sg-tag "oklab"]
       [:label "L" [synced-slider {:min 0 :max 1 :step 0.005 :value (fmt-sig-dig oL)
                                   :on-change (fn [v] (set-oklab! i s 0 v))}]]
       [:label "a" [synced-slider {:min -0.4 :max 0.4 :step 0.005 :value (fmt-sig-dig oa)
                                   :on-change (fn [v] (set-oklab! i s 1 v))}]]
       [:label "b" [synced-slider {:min -0.4 :max 0.4 :step 0.005 :value (fmt-sig-dig ob)
                                   :on-change (fn [v] (set-oklab! i s 2 v))}]]]]]))

(defn studio-panel []
  (let [{:keys [key name-input save-path bake-n fade stops]} @studio]
    [:div.sg-studio
     [:div.sg-head
      [:h2 (str key)]
      [:button {:on-click reset-stops!} "reset to default"]
      [:button.sg-danger {:on-click delete-palette!} "delete palette"]
      [:button.sg-grow {:on-click (fn [_] (reset! studio nil))} "close"]]
     [:div.sg-previews
      [:div.sg-checker
       [:div.sg-preview-h {:style {:background (gradient-css stops "to right")}}]]
      [:div.sg-checker
       [:div.sg-preview-v {:style {:background (gradient-css stops "to bottom")}}]]]
     [:div.sg-rows
      (for [[i s] (map-indexed vector stops)]
        ^{:key i} [stop-row i s])]
     [:div.sg-tools
      [:label "fade "
       [:select {:value (:dir fade)
                 :on-change (fn [e] (swap! studio assoc-in [:fade :dir]
                                           (target-value e)))}
        [:option {:value "out"} "out"]
        [:option {:value "in"} "in"]]]
      [:label "curve "
       [:select {:value (:curve fade)
                 :on-change (fn [e] (swap! studio assoc-in [:fade :curve]
                                           (target-value e)))}
        (for [c (sort (keys curves))]
          ^{:key c} [:option {:value c} c])]]
      [:label.sg-min-alpha "min-alpha"
       [:input {:type "range" :min 0 :max 1 :step 0.05 :value (:min fade)
                :on-change (fn [e] (swap! studio assoc-in [:fade :min]
                                          (js/parseFloat (target-value e))))}]]
      [:button {:on-click apply-fade!} "apply fade"]
      [:label "stops "
       [:input {:type "number" :min 2 :max 12 :value bake-n
                :on-change (fn [e] (swap! studio assoc :bake-n
                                          (js/parseInt (target-value e))))}]]
      [:button {:on-click bake!} "bake"]]
     [:div.sg-tools
      [:label "save as: "
       [:input {:type "text" :size 22 :value name-input
                :on-change (fn [e] (swap! studio assoc :name-input
                                          (target-value e)))}]]
      [:label "file "
       [:input {:type "text" :size 18 :value save-path
                :on-change (fn [e] (swap! studio assoc :save-path
                                          (target-value e)))}]]
      [:button {:on-click save-palette!} "save palette"]
      [:button {:on-click sync-registry!} "sync whole registry"]]]))

(defn app []
  [:div#sg {:style (theme-vars)}
   [:h1 "Swatch palette gallery"]
   [:div#sg-status (or @status "")]
   (when @studio [studio-panel])
   [controls]
   [grid]])

;; Mount -----------------------------------------------------------------------------

(rdom/render [app] (js/document.getElementById "app"))
(fetch-palettes!)