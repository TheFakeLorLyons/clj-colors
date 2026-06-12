(ns clj-colors.gallery
  "A live palette gallery and studio, served by http-kit and written in
   ClojureScript interpreted by Scittle in the browser. No build step, no
   node, no shadow-cljs, no ring: http-kit's handler is a plain function
   over plain request maps, and the UI ships as ClojureScript source in
   this library's resources, so any project depending on clj-colors can
   launch the gallery from a REPL:

     (require '[clj-colors.gallery :as gallery])
     (gallery/serve!)            ; starts on 8350 and opens a browser
     (gallery/serve! {:port 9000 :open? false})
     (gallery/stop!)

   Because there is a live server, the studio writes back to the running
   registry for real: registering a design calls register-palette! and the
   new palette appears in the grid immediately, and the registry can be
   written to disk from the page. The studio's color picks, weights,
   alphas, fades, and bakes all happen client-side; only register and save
   round-trip.

   Endpoints:
     GET  /              the page shell (loads Scittle + reagent from CDN)
     GET  /gallery.cljs  the UI source, from resources/gallery/gallery.cljs
     GET  /api/palettes  the full registry as EDN
     POST /api/register  {:key k :colors [hex...] :tags [...] :path p?}
                         -> register-palette!, then save-registry! to :path
                            when one is given, so a studio save is one
                            atomic action against both registry and file
     POST /api/unregister {:key k :path p?}
                         -> unregister-palette!, then surgical removal of
                            the entry from the file at :path when given
     POST /api/save      {:path p} -> save-registry! (surgical for
                            existing files; see clj-colors.main)

   API errors come back as EDN {:error ...} maps so the page can show
   them instead of failing silently.

   The page needs network access at load time for the CDN scripts; vendor
   them into resources and serve them locally if offline use matters."
  (:require [clojure.edn :as edn]
            [clojure.java.browse :as browse]
            [clojure.java.io :as io]
            [org.httpkit.server :as http]
            [clj-colors.main :as main]))

(def ^:private scittle-version "0.8.31")

;; Data ------------------------------------------------------------------------

(defn- catalog
  "The registry as EDN-friendly data, sorted by key. Keywords travel as
   keywords; the client reads this with cljs.reader."
  []
  (->> (main/all-palettes)
       (sort-by key)
       (mapv (fn [[k p]]
               {:key         k
                :category    (:category p)
                :family      (:family p)
                :tags        (vec (sort (:tags p)))
                :brightness  (:brightness p)
                :temperature (:temperature p)
                :saturation  (:saturation p)
                :contrast    (:contrast p)
                :hex         (:hex p)}))))

;; Page shell --------------------------------------------------------------------

(defn- css-source []
  (if-let [r (io/resource "gallery/gallery_styles.css")]
    (slurp r)
    (throw (ex-info "resources/gallery/gallery_styles.css not found on classpath" {}))))

(defn- page-html []
  (str "<!DOCTYPE html>"
       "<html><head><meta charset='utf-8'>"
       "<title>Swatch</title>"
       "<style>body{margin:0;background:#0e0e11}</style>"
       "<link rel='stylesheet' href='/gallery_styles.css'>"
       "</head><body>"
       "<div id='app' style='color:#888;font-family:monospace;padding:20px'>"
       "loading...</div>"
       "<script crossorigin src='https://cdn.jsdelivr.net/npm/react@18.2.0/umd/react.production.min.js'></script>"
       "<script crossorigin src='https://cdn.jsdelivr.net/npm/react-dom@18.2.0/umd/react-dom.production.min.js'></script>"
       "<script src='https://cdn.jsdelivr.net/npm/scittle@" scittle-version "/dist/scittle.js'></script>"
       "<script src='https://cdn.jsdelivr.net/npm/scittle@" scittle-version "/dist/scittle.reagent.js'></script>"
       "<script type='application/x-scittle' src='/gallery.cljs'></script>"
       "</body></html>"))

(defn- cljs-source []
  (if-let [r (io/resource "gallery/gallery.cljs")]
    (slurp r)
    (throw (ex-info "resources/gallery/gallery.cljs not found on classpath" {}))))

;; Request handling ----------------------------------------------------------------

(defn- edn-response [data]
  {:status  200
   :headers {"Content-Type" "application/edn; charset=utf-8"}
   :body    (pr-str data)})

(defn- read-body [req]
  (some-> (:body req) slurp edn/read-string))

(defn- safely
  "Run an API handler; turn any exception into an EDN {:error ...} response
   the page can display, instead of a silent 500."
  [handle req]
  (try
    (handle req)
    (catch Exception e
      (edn-response {:error (str (.getSimpleName (class e)) ": "
                                 (ex-message e))}))))

(defn- handle-register [req]
  (let [{:keys [key colors tags path]} (read-body req)
        palette (main/register-palette! key colors {:tags tags})]
    (edn-response (cond-> {:registered key :palette palette}
                    path (assoc :saved (main/save-registry! path))))))

(defn- handle-unregister [req]
  (let [{:keys [key path]} (read-body req)]
    (main/unregister-palette! key)
    (edn-response (cond-> {:unregistered key}
                    path (assoc :removed (main/remove-palette-from-file! path key))))))

(defn- handle-save [req]
  (let [{:keys [path]} (read-body req)
        path (or path "palettes.edn")]
    (edn-response {:saved (main/save-registry! path)})))

(defn- handler [{:keys [request-method uri] :as req}]
  (cond
    (and (= :get request-method) (= "/" uri))
    {:status 200 :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (page-html)}

    (and (= :get request-method) (= "/gallery.cljs" uri))
    {:status 200 :headers {"Content-Type" "text/plain; charset=utf-8"}
     :body (cljs-source)}

    (and (= :get request-method) (= "/gallery_styles.css" uri))
    {:status 200 :headers {"Content-Type" "text/css; charset=utf-8"}
     :body (css-source)}

    (and (= :get request-method) (= "/api/palettes" uri))
    (edn-response (catalog))

    (and (= :post request-method) (= "/api/register" uri))
    (safely handle-register req)

    (and (= :post request-method) (= "/api/unregister" uri))
    (safely handle-unregister req)

    (and (= :post request-method) (= "/api/save" uri))
    (safely handle-save req)

    :else
    {:status 404 :headers {"Content-Type" "text/plain"} :body "not found"}))

;; Lifecycle ------------------------------------------------------------------------

(defonce ^:private server (atom nil))

(defn stop!
  "Stop the gallery server if it is running."
  []
  (when-let [stop-fn @server]
    (stop-fn)
    (reset! server nil)
    :stopped))

(defn serve!
  "Start (or restart) the gallery server. Returns the URL.
   opts: :port (default 8350), :open? (default true, opens a browser)."
  ([] (serve! {}))
  ([{:keys [port open?] :or {port 8350 open? true}}]
   (stop!)
   (reset! server (http/run-server handler {:port port}))
   (let [url (str "http://localhost:" port "/")]
     (when open? (browse/browse-url url))
     url)))

(comment
  (serve!)
  (stop!)
)