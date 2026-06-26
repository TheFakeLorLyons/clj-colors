(ns clj-colors.app.server
  "HTTP server infrastructure for the gallery app. Owns route dispatch,
   request/response helpers, and the run/stop lifecycle. Knows nothing
   about palettes, associations, or LLM proposals — those concerns live
   in the manager namespaces and register their handlers with this one."
  (:require [clojure.edn :as edn]
            [clojure.java.browse :as browse]
            [org.httpkit.server :as http]))

;; --- Response helpers ---

(defn edn-response
  "Build an EDN response. Default 200 status; pass :status to override."
  ([data] (edn-response 200 data))
  ([status data]
   {:status  status
    :headers {"Content-Type" "application/edn; charset=utf-8"}
    :body    (pr-str data)}))

(defn html-response [body]
  {:status  200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body    body})

(defn text-response [content-type body]
  {:status  200
   :headers {"Content-Type" (str content-type "; charset=utf-8")}
   :body    body})

(defn not-found []
  {:status  404
   :headers {"Content-Type" "text/plain"}
   :body    "not found"})

(defn read-body
  "Read and parse an EDN request body. Returns nil if no body."
  [req]
  (some-> (:body req) slurp edn/read-string))

(defn safely
  "Wrap a handler so any exception becomes an EDN {:error ...} response.
   Lets the page render an error message instead of facing a 500."
  [handle req]
  (try
    (handle req)
    (catch Exception e
      (edn-response
       {:error (str (.getSimpleName (class e)) ": "
                    (ex-message e))}))))

;; --- Route registry ---
;;
;; Manager namespaces add their routes here at load time via
;; register-route!. The dispatcher walks the registry on each request,
;; finds the first match, and calls its handler. Order matters only when
;; routes overlap, which they shouldn't.

(defonce ^:private routes (atom []))

(defn register-route!
  "Add a route. method is one of :get :post :put :delete; uri is a
   literal path (no patterns yet); handler is (fn [req] -> response)."
  [method uri handler]
  (swap! routes conj [method uri handler]))

(defn reset-routes! []
  (reset! routes []))

(defn- match-route
  [request-method uri]
  (some (fn [[m u h]]
          (when (and (= m request-method) (= u uri))
            h))
        @routes))

(defn handler
  "Top-level Ring handler. Dispatches via the route registry; returns
   404 if no route matches."
  [{:keys [request-method uri] :as req}]
  (if-let [h (match-route request-method uri)]
    (h req)
    (not-found)))

;; --- Lifecycle ---

(defonce ^:private server (atom nil))

(defn stop!
  "Stop the server if running."
  []
  (when-let [stop-fn @server]
    (stop-fn)
    (reset! server nil)
    :stopped))

(defn serve!
  "Start (or restart) the server. opts: :port (default 8350),
   :open? (default true, opens a browser at the root URL)."
  ([] (serve! {}))
  ([{:keys [port open?] :or {port 8350 open? true}}]
   (stop!)
   (reset! server (http/run-server handler {:port port}))
   (let [url (str "http://localhost:" port "/")]
     (when open? (browse/browse-url url))
     url)))