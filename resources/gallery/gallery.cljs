(ns gallery
  "Top-level gallery UI. Owns the shared state atoms, tab routing,
   theming, and the React mount point. Manager CLJS files (palette,
   association, proposal) attach their components to this namespace
   via gallery/register-screen! and refer to shared atoms here."
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.edn :as edn]
            [clojure.string :as str]))

;; --- Shared state ---

(defonce active-tab (r/atom :palettes))
(defonce studio     (r/atom nil))
(defonce theming?   (r/atom true))
(defonce status     (r/atom nil))

;; Each manager file populates its own screen-fn into this map at load
;; time via register-screen!. The app component looks up :active-tab here.
(defonce screens (r/atom {}))

(defn register-screen!
  "Attach a screen component to a tab id. The component is a thunk that
   returns a hiccup form. Called by each manager file at load time."
  [tab-id label component]
  (swap! screens assoc tab-id {:label label :component component}))

;; --- Server I/O (shared) ---

(defn post-edn!
  "POST EDN, read EDN back. Failures become {:error ...} maps."
  [url data callback]
  (-> (js/fetch url (clj->js {:method  "POST"
                              :headers {"Content-Type" "application/edn"}
                              :body    (pr-str data)}))
      (.then (fn [resp] (.text resp)))
      (.then (fn [text]
               (callback (try (edn/read-string text)
                              (catch :default _ {:error text})))))
      (.catch (fn [err] (callback {:error (str err)})))))

(defn get-edn!
  "GET an EDN endpoint."
  [url callback]
  (-> (js/fetch url)
      (.then (fn [resp] (.text resp)))
      (.then (fn [text] (callback (edn/read-string text))))
      (.catch (fn [err] (callback {:error (str err)})))))

;; --- Color math ---

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

(defn valid-hex? [s]
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

;; --- Gradient model ---

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

;; --- Theming ---

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

;; --- Shared UI helpers ---

(defn target-value [e] (.. e -target -value))

(defn fmt-sig-dig [x]
  (let [n (js/Number x)]
    (if (< (js/Math.abs n) 1)
      (.toFixed n 3)
      (.toFixed n 2))))

(defn synced-slider []
  (let [local (r/atom nil)]
    (fn [{:keys [min max step value on-change class]}]
      (let [loaded? (some? value)
            display (if (some? @local)
                      @local
                      (when loaded? (str value)))]
        [:span.sg-synced
         [:input {:type "range" :class class :min min :max max :step step
                  :value (if loaded? value min) :disabled (not loaded?)
                  :on-change (fn [e]
                               (let [v (js/parseFloat (target-value e))]
                                 (reset! local (str v))
                                 (on-change v)))}]
         (if loaded?
           [:input {:type "number" :class "sg-slider-num"
                    :min min :max max :step step :value display
                    :on-change (fn [e] (reset! local (target-value e)))
                    :on-blur (fn [e]
                               (let [v (js/parseFloat (target-value e))]
                                 (when-not (js/isNaN v)
                                   (let [clamped (max min (min max v))]
                                     (reset! local nil)
                                     (on-change clamped)))))
                    :on-key-down (fn [e]
                                   (when (= "Enter" (.-key e))
                                     (let [v (js/parseFloat (target-value e))]
                                       (when-not (js/isNaN v)
                                         (let [clamped (max min (min max v))]
                                           (reset! local nil)
                                           (on-change clamped))))))}]
           [:span.sg-slider-placeholder "—"])]))))

;; --- Tabs ---

(defn tabs []
  (let [tab-ids [:palettes :associations :proposals]
        labels  {:palettes "Palette Gallery"
                 :associations "Association Gallery"
                 :proposals "Proposal Builder"}]
    [:div.sg-tabs
     (for [id tab-ids]
       ^{:key id}
       [:button.sg-tab
        {:class (when (= id @active-tab) "active")
         :on-click (fn [_]
                     (reset! studio nil)
                     (reset! active-tab id))}
        (get labels id)])]))

;; --- App ---

(defn app []
  [:div#sg {:style (theme-vars)}
   [:h1 "Swatch palette gallery"]
   [:div#sg-status (or @status "")]
   [tabs]
   (if-let [screen (get @screens @active-tab)]
     [(:component screen)]
     [:div "loading..."])])

(rdom/render [app] (js/document.getElementById "app"))