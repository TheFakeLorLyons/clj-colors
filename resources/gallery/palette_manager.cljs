(ns palette-manager
  "Palette gallery screen: grid + studio. Registers itself with the
   main gallery namespace as the :palettes screen on load."
  (:require [reagent.core :as r]
            [clojure.string :as str]))

;; --- State ---

(defonce tag-idf  (r/atom {}))
(defonce palettes (r/atom []))
(defonce filters
  (r/atom {:search "" :search-scope #{:name :tags}
           :category "all" :family "all" :attr-tag "all"
           :association "all" :brightness 0
           :sort-key :name :desc? false}))

(def blank-new-stops
  [{:hex "#808080" :weight 1 :alpha 1 :oklab nil :oklch nil}
   {:hex "#C0C0C0" :weight 1 :alpha 1 :oklab nil :oklch nil}
   {:hex "#404040" :weight 1 :alpha 1 :oklab nil :oklch nil}])

;; --- Server I/O ---

(defn compute-tag-idf! []
  (let [ps @palettes
        n  (count ps)
        df (reduce (fn [acc p]
                     (reduce (fn [a tag] (update a tag (fnil inc 0)))
                             acc (keys (:tags (:attributes p)))))
                   {} ps)]
    (reset! tag-idf (into {} (for [[tag c] df] [tag (js/Math.log (/ n c))])))))

(defn fetch-palettes! []
  (gallery/get-edn! "/api/palettes"
                    (fn [data] (reset! palettes data) (compute-tag-idf!))))

(defn preview-attrs! []
  (let [stops (:stops @gallery/studio)]
    (when (seq stops)
      (gallery/post-edn! "/api/preview"
                         {:colors  (mapv (fn [s] (subs (:hex s) 0 7)) stops)
                          :weights (mapv :weight stops)}
                         (fn [resp]
                           (when-not (:error resp)
                             (swap! gallery/studio assoc :attrs resp)))))))

;; --- Stop operations ---

(defn blank-stop [h]
  {:hex (str/upper-case (subs h 0 7)) :weight 1 :alpha 1 :oklab nil :oklch nil})

(defn refresh-stop! [i hex]
  (gallery/post-edn! "/api/convert" {:hex hex}
                     (fn [{:keys [oklab oklch error]}]
                       (when-not error
                         (swap! gallery/studio update-in [:stops i] assoc
                                :oklab oklab :oklch oklch)))))

(defn refresh-all-stops! []
  (doseq [[i s] (map-indexed vector (:stops @gallery/studio))]
    (refresh-stop! i (:hex s))))

(defn add-stop! []
  (let [last-hex (or (some-> @gallery/studio :stops last :hex) "#808080")
        new      {:hex last-hex :weight 1 :alpha 1 :oklab nil :oklch nil}]
    (swap! gallery/studio update :stops conj new)
    (refresh-stop! (dec (count (:stops @gallery/studio))) last-hex)))

(defn remove-stop! [i]
  (swap! gallery/studio update :stops
         (fn [stops]
           (if (<= (count stops) 1) stops
               (vec (concat (subvec stops 0 i) (subvec stops (inc i))))))))

(defn commit-hex! [i hex]
  (let [hex (str/upper-case hex)]
    (swap! gallery/studio assoc-in [:stops i :hex] hex)
    (refresh-stop! i hex)))

(defn set-rgb! [i s idx raw]
  (let [n   (js/parseInt raw 10)
        v   (if (js/isNaN n) 0 (max 0 (min 255 n)))
        rgb (assoc (gallery/hex->rgb (:hex s)) idx v)]
    (commit-hex! i (apply gallery/rgb->hex rgb))))

(defn set-oklch! [i s idx raw]
  (let [n (js/parseFloat raw)]
    (when-not (js/isNaN n)
      (let [oklch (assoc (:oklch s) idx n)]
        (swap! gallery/studio assoc-in [:stops i :oklch] oklch)
        (gallery/post-edn! "/api/convert" {:oklch oklch}
                           (fn [{:keys [hex oklab error]}]
                             (when-not error
                               (swap! gallery/studio update-in [:stops i] assoc
                                      :hex (str/upper-case hex) :oklab oklab))))))))

(defn set-oklab! [i s idx raw]
  (let [n (js/parseFloat raw)]
    (when-not (js/isNaN n)
      (let [oklab (assoc (:oklab s) idx n)]
        (swap! gallery/studio assoc-in [:stops i :oklab] oklab)
        (gallery/post-edn! "/api/convert" {:oklab oklab}
                           (fn [{:keys [hex oklch error]}]
                             (when-not error
                               (swap! gallery/studio update-in [:stops i] assoc
                                      :hex (str/upper-case hex) :oklch oklch))))))))

;; --- Studio lifecycle ---

(defn open-studio! [p]
  (let [stops (mapv blank-stop (:hex p))]
    (reset! gallery/studio
            {:type :palette :key (:key p)
             :name-input (str "studio/" (name (:key p)))
             :save-path "palettes.edn" :bake-n 5
             :fade {:dir "out" :curve "linear" :min 0}
             :orig stops :stops stops 
             :attrs (:attributes p)})
    (refresh-all-stops!)))

(defn new-studio! []
  (reset! gallery/studio
          {:type :palette :key nil
           :name-input "studio/new-palette"
           :save-path "palettes.edn" :bake-n 5
           :fade {:dir "out" :curve "linear" :min 0}
           :orig blank-new-stops :stops blank-new-stops :attrs nil})
  (refresh-all-stops!))

(defn stop->hex [s]
  (let [base (str/upper-case (subs (:hex s) 0 7))]
    (if (>= (:alpha s) 0.999)
      base
      (str base (-> (.toString (js/Math.round (* 255 (:alpha s))) 16)
                    (.padStart 2 "0") (.toUpperCase))))))

(defn save-palette! []
  (let [{:keys [stops name-input save-path]} @gallery/studio
        nm (if (str/includes? name-input "/") name-input (str "studio/" name-input))]
    (gallery/post-edn! "/api/register"
                       {:key (keyword nm) :colors (mapv stop->hex stops) :path save-path}
                       (fn [resp]
                         (if (:error resp)
                           (reset! gallery/status (str "error: " (:error resp)))
                           (do (reset! gallery/status
                                       (str "saved " (:registered resp)))
                               (fetch-palettes!)))))))

(defn sync-registry! []
  (gallery/post-edn! "/api/save" {:path (:save-path @gallery/studio)}
                     (fn [resp]
                       (reset! gallery/status
                               (if (:error resp)
                                 (str "error: " (:error resp))
                                 (str "synced to " (:saved resp)))))))

(defn delete-palette! []
  (let [{:keys [key save-path]} @gallery/studio]
    (when (js/confirm (str "Delete " key "?"))
      (gallery/post-edn! "/api/unregister" {:key key :path save-path}
                         (fn [resp]
                           (if (:error resp)
                             (reset! gallery/status (str "error: " (:error resp)))
                             (do (reset! gallery/status (str "deleted " (:unregistered resp)))
                                 (reset! gallery/studio nil)
                                 (fetch-palettes!))))))))

(defn reset-stops! []
  (swap! gallery/studio (fn [st] (assoc st :stops (:orig st)))))

;; --- Filter logic ---

(defn options-of [k]
  (->> @palettes (keep k) (map name) distinct sort))

(defn filtered []
  (let [{:keys [search search-scope category family attr-tag association
                brightness sort-key desc?]} @filters
        q  (str/lower-case search)
        search? (fn [p]
                  (cond
                    (str/blank? q) true
                    (empty? search-scope) false
                    :else
                    (or (and (contains? search-scope :name)
                             (str/includes? (str/lower-case (str (:key p))) q))
                        (and (contains? search-scope :tags)
                             (or (some (fn [t]
                                         (str/includes? (str/lower-case (name t)) q))
                                       (keys (get-in p [:attributes :tags])))
                                 (some (fn [t]
                                         (str/includes? (str/lower-case (str t)) q))
                                       (:tags p)))))))
        xs (filter (fn [p]
                     (and (or (= "all" category)
                              (= category (some-> (:category p) name)))
                          (or (= "all" family)
                              (= family (some-> (:family p) name)))
                          (or (= "all" attr-tag)
                              (get-in p [:attributes :tags (keyword attr-tag)]))
                          (or (= "all" association)
                              (get-in p [:attributes :associations (keyword association)]))
                          (>= (or (:brightness p) 0) brightness)
                          (search? p)))
                   @palettes)
        xs (if (= :name sort-key)
             (sort-by (fn [p] (str (:key p))) xs)
             (sort-by (fn [p] (or (get p sort-key) 0)) xs))]
    (vec (if desc? (reverse xs) xs))))

;; --- Components ---

(defn controls []
  (let [f @filters]
    [:div.sg-controls
     [:button.sg-new {:on-click (fn [_] (new-studio!))} "+ new palette"]
     [:div.sg-search-group
      [:label.sg-search-label "search "
       [:input {:type "text" :size 12 :value (:search f)
                :on-change (fn [e] (swap! filters assoc :search (gallery/target-value e)))}]]
      [:span.sg-search-scope
       [:label.sg-search-scope-item
        [:input {:type "checkbox"
                 :checked (contains? (:search-scope f) :name)
                 :on-change (fn [_]
                              (swap! filters update :search-scope
                                     (fn [s] (if (contains? s :name)
                                               (disj s :name)
                                               (conj s :name)))))}]
        " name"]
       [:label.sg-search-scope-item
        [:input {:type "checkbox"
                 :checked (contains? (:search-scope f) :tags)
                 :on-change (fn [_]
                              (swap! filters update :search-scope
                                     (fn [s] (if (contains? s :tags)
                                               (disj s :tags)
                                               (conj s :tags)))))}]
        " tags"]]]
     [:label "category "
      [:select {:value (:category f)
                :on-change (fn [e] (swap! filters assoc :category (gallery/target-value e)))}
       [:option {:value "all"} "all"]
       (for [c (options-of :category)] ^{:key c} [:option {:value c} c])]]
     [:label "family "
      [:select {:value (:family f)
                :on-change (fn [e] (swap! filters assoc :family (gallery/target-value e)))}
       [:option {:value "all"} "all"]
       (for [c (options-of :family)] ^{:key c} [:option {:value c} c])]]
     [:label "attr tag "
      [:select {:value (:attr-tag f)
                :on-change (fn [e] (swap! filters assoc :attr-tag (gallery/target-value e)))}
       [:option {:value "all"} "all"]
       (for [t (sort (distinct (mapcat (comp keys :tags :attributes) @palettes)))]
         ^{:key t} [:option {:value (name t)} (name t)])]]
     [:label "sort "
      [:select {:value (name (:sort-key f))
                :on-change (fn [e] (swap! filters assoc :sort-key
                                          (keyword (gallery/target-value e))))}
       (for [s ["name" "brightness" "temperature" "saturation" "contrast"]]
         ^{:key s} [:option {:value s} s])]]
     [:label
      [:input {:type "checkbox" :checked (:desc? f)
               :on-change (fn [_] (swap! filters update :desc? not))}]
      " desc"]
     [:span.sg-count (str (count (filtered)) " palettes")]]))

(defn card [p]
  [:div.sg-card {:on-click (fn [_] (open-studio! p))}
   [:div.sg-bar
    {:style {:background (str "linear-gradient(to right, "
                              (str/join ", " (:hex p)) ")")}}]
   [:div.sg-name (str (:key p))]
   [:div.sg-tagline
    (str/join ", "
              (->> (:tags (:attributes p))
                   (map (fn [[tag score]]
                          [tag (* score (or (get @tag-idf tag) 0))]))
                   (sort-by (comp - second))
                   (take 4)
                   (map (fn [[t _]] (name t)))))]])

(defn grid []
  [:div.sg-grid
   (for [p (filtered)]
     ^{:key (str (:key p))} [card p])])

(defn stop-row [i s]
  (let [hex        (subs (:hex s) 0 7)
        [r g b]    (gallery/hex->rgb hex)
        [oL oa ob] (:oklab s)
        [cL cC cH] (:oklch s)]
    [:div.sg-stop
     [:div.sg-row
      [:button.sg-stop-remove {:on-click (fn [_] (remove-stop! i))} "x"]
      [:input {:type "color" :value hex
               :on-change (fn [e] (commit-hex! i (gallery/target-value e)))}]
      [:input.sg-hex {:type "text" :value hex
                      :on-change (fn [e]
                                   (let [raw (gallery/target-value e)
                                         val (if (str/starts-with? raw "#") raw (str "#" raw))]
                                     (when (gallery/valid-hex? val) (commit-hex! i val))))}]
      [:label "r" [:input.sg-num {:type "number" :min 0 :max 255 :value r
                                  :on-change (fn [e] (set-rgb! i s 0 (gallery/target-value e)))}]]
      [:label "g" [:input.sg-num {:type "number" :min 0 :max 255 :value g
                                  :on-change (fn [e] (set-rgb! i s 1 (gallery/target-value e)))}]]
      [:label "b" [:input.sg-num {:type "number" :min 0 :max 255 :value b
                                  :on-change (fn [e] (set-rgb! i s 2 (gallery/target-value e)))}]]
      [:label "weight"
       [gallery/synced-slider {:min 0.1 :max 4 :step 0.05 :value (:weight s)
                               :on-change (fn [v] (swap! gallery/studio assoc-in [:stops i :weight] v))}]]
      [:label "alpha"
       [gallery/synced-slider {:min 0 :max 1 :step 0.01 :value (:alpha s)
                               :on-change (fn [v] (swap! gallery/studio assoc-in [:stops i :alpha] v))}]]
      [:span.sg-oklch
       [:span.sg-tag "oklch"]
       [:label "L" [gallery/synced-slider {:min 0 :max 1 :step 0.005 :value (gallery/fmt-sig-dig cL)
                                           :on-change (fn [v] (set-oklch! i s 0 v))}]]
       [:label "C" [gallery/synced-slider {:min 0 :max 0.37 :step 0.002 :value (gallery/fmt-sig-dig cC)
                                           :on-change (fn [v] (set-oklch! i s 1 v))}]]
       [:label "H" [gallery/synced-slider {:min 0 :max 360 :step 1 :value (gallery/fmt-sig-dig cH)
                                           :on-change (fn [v] (set-oklch! i s 2 v))}]]]
      [:span.sg-oklab
       [:span.sg-tag "oklab"]
       [:label "L" [gallery/synced-slider {:min 0 :max 1 :step 0.005 :value (gallery/fmt-sig-dig oL)
                                           :on-change (fn [v] (set-oklab! i s 0 v))}]]
       [:label "a" [gallery/synced-slider {:min -0.4 :max 0.4 :step 0.005 :value (gallery/fmt-sig-dig oa)
                                           :on-change (fn [v] (set-oklab! i s 1 v))}]]
       [:label "b" [gallery/synced-slider {:min -0.4 :max 0.4 :step 0.005 :value (gallery/fmt-sig-dig ob)
                                           :on-change (fn [v] (set-oklab! i s 2 v))}]]]]]))

(defn attrs-panel [attrs]
  (let [tags   (:tags attrs)
        assocs (:associations attrs)]
    [:div.sg-attrs
     [:div.sg-attrs-col
      [:h4 (str "Tags" (when (seq tags) (str " (" (count tags) ")")))]
      (if (seq tags)
        [:ul.sg-attrs-list
         (for [[t score] (->> tags (sort-by (comp - second)) (take 30))]
           ^{:key t}
           [:li
            [:span.sg-attrs-name (name t)]
            [:span.sg-attrs-score (.toFixed score 3)]])]
        [:p.sg-attrs-empty "no tags yet — click refresh"])]
     [:div.sg-attrs-col
      [:h4 (str "Associations" (when (seq assocs) (str " (" (count assocs) ")")))]
      (if (seq assocs)
        [:ul.sg-attrs-list
         (for [[a score] (->> assocs (sort-by (comp - second)) (take 30))]
           ^{:key a}
           [:li
            [:span.sg-attrs-name (str a)]
            [:span.sg-attrs-score (.toFixed score 3)]])]
        [:p.sg-attrs-empty "no associations"])]]))

(defn studio-panel []
  (let [{:keys [key name-input save-path stops attrs]} @gallery/studio]
    [:div.sg-studio
     [:div.sg-head
      [:h2 (or (some-> key str) "new palette")]
      [:button {:on-click reset-stops!} "reset to default"]
      (when key [:button.sg-danger {:on-click delete-palette!} "delete palette"])
      [:button {:on-click (fn [_] (preview-attrs!))} "refresh tags"]
      [:button.sg-grow {:on-click (fn [_] (reset! gallery/studio nil))} "close"]]
     [:div.sg-previews
      [:div.sg-checker [:div.sg-preview-h {:style {:background (gallery/gradient-css stops "to right")}}]]
      [:div.sg-checker [:div.sg-preview-v {:style {:background (gallery/gradient-css stops "to bottom")}}]]]
     [attrs-panel (or attrs {})]
     [:div.sg-rows
      (for [[i s] (map-indexed vector stops)]
        ^{:key i} [stop-row i s])]
     [:div.sg-tools
      [:button {:on-click add-stop!} "+ add color"]]
     [:div.sg-tools
      [:label "save as: "
       [:input {:type "text" :size 22 :value name-input
                :on-change (fn [e] (swap! gallery/studio assoc :name-input (gallery/target-value e)))}]]
      [:label "file "
       [:input {:type "text" :size 18 :value save-path
                :on-change (fn [e] (swap! gallery/studio assoc :save-path (gallery/target-value e)))}]]
      [:button {:on-click save-palette!} "save palette"]
      [:button {:on-click sync-registry!} "sync whole registry"]]]))

;; --- Screen ---

(defn screen []
  [:div
   (when (and @gallery/studio (= :palette (:type @gallery/studio)))
     [studio-panel])
   [controls]
   [grid]])

;; --- Register ---

(gallery/register-screen! :palettes "Palette Gallery" screen)
(fetch-palettes!)