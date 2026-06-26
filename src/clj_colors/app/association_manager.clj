(ns clj-colors.app.association-manager
  "HTTP handlers and catalog data for the association gallery. Wraps
   authoring's CRUD so the browser can browse, register, and unregister
   authored associations."
  (:require [clj-colors.app.server :as server]
            [clj-colors.associations :as associations]
            [clj-colors.authoring :as authoring]))

(defn catalog
  "Browser-facing association list. Only :authored entries; the
   ingested color-tag corpus and emotional dump aren't meant to be
   browsed one at a time."
  []
  (->> @associations/data
       (filter  (fn [[_ v]] (#{:authored :llm-generated} (:source v))))
       (sort-by key)
       (mapv (fn [[k v]] (assoc v :key k)))))

(defn- handle-list [_req]
  (server/edn-response (catalog)))

(defn- handle-register
  "Add or update an authored association.
   Body: {:key :ns/name :entry {...} :authored-file F?}"
  [req]
  (let [{:keys [key entry authored-file]} (server/read-body req)]
    (binding [authoring/*authored-path*
              (if authored-file
                (authoring/resolve-path authored-file)
                authoring/*authored-path*)]
      (let [persisted (authoring/add-association! key entry)]
        (server/edn-response {:registered key :entry persisted})))))

(defn- handle-unregister
  "Remove an authored association entry.
   Body: {:key :ns/name :authored-file F?}"
  [req]
  (let [{:keys [key authored-file]} (server/read-body req)]
    (binding [authoring/*authored-path*
              (if authored-file
                (authoring/resolve-path authored-file)
                authoring/*authored-path*)]
      (authoring/remove-association! key)
      (server/edn-response {:unregistered key}))))

(defn register-routes! []
  (server/register-route! :get  "/api/associations"            handle-list)
  (server/register-route! :post "/api/association/register"    #(server/safely handle-register %))
  (server/register-route! :post "/api/association/unregister"  #(server/safely handle-unregister %)))