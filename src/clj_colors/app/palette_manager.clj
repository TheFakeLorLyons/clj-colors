(ns clj-colors.app.palette-manager
  "HTTP handlers and catalog data for the palette gallery. Wraps the
   main palette registry, exposing browse, register, unregister, save,
   and attribute-preview operations."
  (:require [clojure.string :as str]
            [clj-colors.color :as color]
            [clj-colors.app.server :as server]
            [clj-colors.associations :as associations]
            [clj-colors.main :as main]))

(defn catalog
  "Browser-facing palette list. Strips internal fields that the UI
   doesn't need so payloads stay small."
  []
  (->> (main/all-palettes)
       (sort-by key)
       (mapv (fn [[k p]]
               {:key         k
                :category    (:category p)
                :family      (:family p)
                :attributes  (:attributes p)
                :brightness  (:brightness p)
                :temperature (:temperature p)
                :saturation  (:saturation p)
                :contrast    (:contrast p)
                :hex         (:hex p)}))))

(defn- handle-list [_req]
  (server/edn-response (catalog)))

(defn- handle-register [req]
  (let [{:keys [key colors weights path]} (server/read-body req)
        palette (main/register-palette! key colors
                                        (cond-> {}
                                          (seq weights) (assoc :weights weights)))]
    (server/edn-response
     (cond-> {:registered key :palette palette}
       path (assoc :saved (main/save-registry! path))))))

(defn- handle-unregister [req]
  (let [{:keys [key path]} (server/read-body req)]
    (main/unregister-palette! key)
    (server/edn-response
     (cond-> {:unregistered key}
       path (assoc :saved (main/save-registry! path))))))

(defn- handle-save [req]
  (let [{:keys [path]} (server/read-body req)
        path (or path "palettes.edn")]
    (server/edn-response {:saved (main/save-registry! path)})))

(defn- handle-preview
  "Compute :attributes for an unregistered palette spec. Lets the
   studio show predicted tags without persisting."
  [req]
  (let [{:keys [colors weights]} (server/read-body req)
        hexes  (mapv str/lower-case colors)
        n      (count hexes)
        ws     (or (when (seq weights) (vec weights))
                   (vec (repeat n (/ 1.0 n))))
        total  (reduce + 0.0 ws)
        ws     (mapv #(/ % total) ws)
        dist   (zipmap hexes ws)
        oklabs (mapv (fn [h]
                       (-> h color/hex->rgba color/rgba->oklab (subvec 0 3)))
                     hexes)]
    (server/edn-response
     (associations/palette-attributes hexes ws dist oklabs))))

(defn register-routes! []
  (server/register-route! :get  "/api/palettes"     handle-list)
  (server/register-route! :post "/api/register"     #(server/safely handle-register %))
  (server/register-route! :post "/api/unregister"   #(server/safely handle-unregister %))
  (server/register-route! :post "/api/save"         #(server/safely handle-save %))
  (server/register-route! :post "/api/preview"      #(server/safely handle-preview %)))