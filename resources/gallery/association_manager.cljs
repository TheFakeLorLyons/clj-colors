(ns association-manager
  "Association gallery screen: grid + studio for browsing and editing
   authored associations."
  (:require [reagent.core :as r]
            [clojure.string :as str]))

;; --- State ---

(defonce associations (r/atom []))
(defonce filters
  (r/atom {:search "" :search-scope #{:name :tags}
           :category "all" :tag "all"
           :sort-key :name :desc? false}))

;; --- Server I/O ---

(defn fetch! []
  (gallery/get-edn! "/api/associations"
                    (fn [data] (reset! associations data))))

;; --- Operations ---

(defn open-studio! [a]
  (let [colors-map (:colors a)
        stops (->> colors-map
                   (sort-by (comp - second))
                   (mapv (fn [[hex weight]]
                           {:hex (str/upper-case
                                  (cond-> hex (not (str/starts-with? hex "#"))
                                          (->> (str "#"))))
                            :weight weight :alpha 1 :oklab nil :oklch nil})))]
    (reset! gallery/studio
            {:type :association :key (:key a)
             :name-input (str (:key a))
             :authored-file "" :orig a :stops stops
             :tags (or (:tags a) {})
             :sigma (:sigma a) :category (:category a)
             :rationale (:rationale a)
             :references (:references a)
             :new-tag-name ""})
    (palette-manager/refresh-all-stops!)))

(defn new-studio! []
  (reset! gallery/studio
          {:type :association :key nil
           :name-input "scene/new-thing"
           :authored-file "" :orig nil
           :stops palette-manager/blank-new-stops
           :tags {} :sigma 0.08 :category :scene
           :rationale "" :references [] :new-tag-name ""})
  (palette-manager/refresh-all-stops!))

(defn add-tag! [tag-name]
  (when-not (str/blank? tag-name)
    (let [k (keyword (str/replace tag-name #"^:" ""))]
      (swap! gallery/studio assoc-in [:tags k] {:weight 0.7 :specificity 1.0})
      (swap! gallery/studio assoc :new-tag-name ""))))

(defn remove-tag! [tag-k]
  (swap! gallery/studio update :tags dissoc tag-k))

(defn save! []
  (let [{:keys [name-input stops tags sigma category rationale references
                authored-file]} @gallery/studio
        colors-map (into {} (map (fn [s]
                                   [(str/lower-case (subs (:hex s) 0 7))
                                    (:weight s)])
                                 stops))
        entry {:colors colors-map :tags tags :sigma sigma
               :category category :rationale rationale
               :references (or references [])}
        key (keyword (str/replace name-input #"^:" ""))]
    (gallery/post-edn! "/api/association/register"
                       (cond-> {:key key :entry entry}
                         (not (str/blank? authored-file))
                         (assoc :authored-file authored-file))
                       (fn [resp]
                         (if (:error resp)
                           (reset! gallery/status (str "error: " (:error resp)))
                           (do (reset! gallery/status (str "saved " (:registered resp)))
                               (fetch!)))))))

(defn delete! []
  (let [{:keys [key authored-file]} @gallery/studio]
    (when (js/confirm (str "Delete " key "?"))
      (gallery/post-edn! "/api/association/unregister"
                         (cond-> {:key key}
                           (not (str/blank? authored-file))
                           (assoc :authored-file authored-file))
                         (fn [resp]
                           (if (:error resp)
                             (reset! gallery/status (str "error: " (:error resp)))
                             (do (reset! gallery/status (str "deleted " (:unregistered resp)))
                                 (reset! gallery/studio nil)
                                 (swap! filters assoc :search "")
                                 (fetch!))))))))

;; --- Filter ---

(defn options-of [k]
  (->> @associations (keep k) (map name) distinct sort))

(defn all-tags []
  (->> @associations (mapcat (comp keys :tags)) distinct sort))

(defn filtered []
  (let [{:keys [search search-scope category tag sort-key desc?]} @filters
        q  (str/lower-case search)
        search? (fn [a]
                  (cond
                    (str/blank? q) true
                    (empty? search-scope) false
                    :else
                    (and (contains? search-scope :name)
                         (and (contains? search-scope :tags)
                              (some (fn [t]
                                      (str/includes? (str/lower-case (name t)) q))
                                    (keys (:tags a)))))))
        xs (filter (fn [a]
                     (and (or (= "all" category)
                              (= category (some-> (:category a) name)))
                          (or (= "all" tag) (contains? (:tags a) (keyword tag)))
                          (search? a)))
                   @associations)
        xs (if (= :name sort-key)
             (sort-by (fn [a] (str (:key a))) xs)
             (sort-by (fn [a] (or (get a sort-key) 0)) xs))]
    (vec (if desc? (reverse xs) xs))))

;; --- Components ---

(defn controls []
  (let [f @filters]
    [:div.sg-controls
     [:button.sg-new {:on-click (fn [_] (new-studio!))} "+ new association"]
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
     [:label "tag "
      [:select {:value (:tag f)
                :on-change (fn [e] (swap! filters assoc :tag (gallery/target-value e)))}
       [:option {:value "all"} "all"]
       (for [t (all-tags)] ^{:key t} [:option {:value (name t)} (name t)])]]
     [:label "sort "
      [:select {:value (name (:sort-key f))
                :on-change (fn [e] (swap! filters assoc :sort-key
                                          (keyword (gallery/target-value e))))}
       (for [s ["name" "sigma"]]
         ^{:key s} [:option {:value s} s])]]
     [:span.sg-count (str (count (filtered)) " associations")]]))

(defn card [a]
  (let [colors-vec (->> (:colors a)
                        (sort-by (comp - second))
                        (mapv (fn [[hex _]]
                                (cond-> hex (not (str/starts-with? hex "#"))
                                        (->> (str "#"))))))
        top-tags (->> (:tags a)
                      (sort-by (fn [[_ {:keys [weight]}]] (- (or weight 0))))
                      (take 4) (map first))]
    [:div.sg-card.sg-assoc-card
     {:on-click (fn [_] (open-studio! a))}
     [:div.sg-bar {:style {:background (str "linear-gradient(to right, "
                                            (str/join ", " colors-vec) ")")}}]
     [:div.sg-name (str (:key a))]
     [:div.sg-tagline (str/join ", " (map name top-tags))]
     [:div.sg-meta
      (when (:category a) [:span.sg-category (name (:category a))])
      (when (:sigma a) [:span.sg-sigma (str "σ=" (.toFixed (:sigma a) 2))])]]))

(defn grid []
  [:div.sg-grid
   (for [a (filtered)]
     ^{:key (str (:key a))} [card a])])

(defn tag-row [tag-k {:keys [weight specificity]}]
  [:div.sg-tag-row
   [:button.sg-tag-remove {:on-click (fn [_] (remove-tag! tag-k))} "x"]
   [:span.sg-tag-name (str tag-k)]
   [:label "weight"
    [gallery/synced-slider {:min 0 :max 1 :step 0.05 :value (or weight 0)
                            :on-change (fn [v]
                                         (swap! gallery/studio assoc-in [:tags tag-k :weight] v))}]]
   [:label "specificity"
    [gallery/synced-slider {:min 0.2 :max 3.0 :step 0.05 :value (or specificity 1.0)
                            :on-change (fn [v]
                                         (swap! gallery/studio assoc-in [:tags tag-k :specificity] v))}]]])

(defn studio-panel []
  (let [{:keys [key name-input stops tags sigma category rationale
                authored-file new-tag-name]} @gallery/studio]
    [:div.sg-studio.sg-assoc-studio
     [:div.sg-head
      [:h2 (or (some-> key str) "new association")]
      (when key [:button.sg-danger {:on-click delete!} "delete association"])
      [:button.sg-grow {:on-click (fn [_] (reset! gallery/studio nil))} "close"]]
     [:div.sg-previews
      [:div.sg-checker [:div.sg-preview-h {:style {:background (gallery/gradient-css stops "to right")}}]]]
     [:div.sg-tools
      [:label "name (ns/key) "
       [:input {:type "text" :size 30 :value name-input
                :on-change (fn [e] (swap! gallery/studio assoc :name-input (gallery/target-value e)))}]]
      [:label "category "
       [:input {:type "text" :size 14 :value (when category (name category))
                :on-change (fn [e]
                             (let [v (gallery/target-value e)]
                               (swap! gallery/studio assoc :category
                                      (when-not (str/blank? v) (keyword v)))))}]]
      [:label "sigma"
       [gallery/synced-slider {:min 0.02 :max 0.2 :step 0.005 :value sigma
                               :on-change (fn [v] (swap! gallery/studio assoc :sigma v))}]]
      [:label "file (optional) "
       [:input {:type "text" :size 18 :value authored-file
                :on-change (fn [e] (swap! gallery/studio assoc :authored-file (gallery/target-value e)))}]]]
     [:h3 "Colors"]
     [:div.sg-rows
      (for [[i s] (map-indexed vector stops)]
        ^{:key i} [palette-manager/stop-row i s])]
     [:div.sg-tools
      [:button {:on-click palette-manager/add-stop!} "+ add color"]]
     [:h3 "Tags"]
     (for [[tag-k tag-data] (sort-by (fn [[_ d]] (- (or (:weight d) 0))) tags)]
       ^{:key tag-k} [tag-row tag-k tag-data])
     [:div.sg-tag-add
      [:input {:type "text" :placeholder "new tag name (e.g. :woodland)"
               :value (or new-tag-name "")
               :on-change (fn [e] (swap! gallery/studio assoc :new-tag-name (gallery/target-value e)))
               :on-key-down (fn [e]
                              (when (= "Enter" (.-key e)) (add-tag! new-tag-name)))}]
      [:button {:on-click (fn [_] (add-tag! new-tag-name))} "+ add tag"]]
     [:h3 "Rationale"]
     [:textarea.sg-rationale
      {:value (or rationale "")
       :on-change (fn [e] (swap! gallery/studio assoc :rationale (gallery/target-value e)))}]
     [:div.sg-tools
      [:button {:on-click save!} "save association"]]]))

(defn screen []
  [:div
   (when (and @gallery/studio (= :association (:type @gallery/studio)))
     [studio-panel])
   [controls]
   [grid]])

(gallery/register-screen! :associations "Association Gallery" screen)
(fetch!)