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
                         -> unregister-palette!, then save-registry! to
                            the file at :path when one is given
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
            [clj-colors.color :as color]
            [clj-colors.main :as main]
            [clj-colors.meta :as meta]))

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

(defn- css-source []
  (if-let [r (io/resource "gallery/gallery_styles.css")]
    (slurp r)
    (throw (ex-info "resources/gallery/gallery_styles.css not found on classpath" {}))))

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
  (let [{:keys [key colors tags weights path]} (read-body req)
        palette (main/register-palette! key colors
                                        (cond-> {}
                                          (seq tags)    (assoc :tags tags)
                                          (seq weights) (assoc :weights weights)))]
    (edn-response (cond-> {:registered key :palette palette}
                    path (assoc :saved (main/save-registry! path))))))

(defn- handle-unregister [req]
  (let [{:keys [key path]} (read-body req)]
    (main/unregister-palette! key)
    (edn-response (cond-> {:unregistered key}
                    path (assoc :saved (main/save-registry! path))))))

(defn- handle-save [req]
  (let [{:keys [path]} (read-body req)
        path (or path "palettes.edn")]
    (edn-response {:saved (main/save-registry! path)})))

(defn- handle-ramp
  "Perceptual resample via color/ramp. Body:
   {:colors [hex...] :n N :space :oklab|:oklch :weights [..]?}
   -> {:ramp [hex...]}. Hex in, hex out, so it drops straight into stops."
  [req]
  (let [{:keys [colors n space weights]} (read-body req)]
    (edn-response
     {:ramp (color/ramp colors (or n 5)
                        (cond-> {}
                          space         (assoc :space space)
                          (seq weights)  (assoc :weights weights)))})))

(defn- handle-blend
  "Two-color blend via color/blend. Body:
   {:from c1 :to c2 :t 0.5 :space :oklab|:oklch}
   -> {:hex h :rgba [r g b a]}."
  [req]
  (let [{:keys [from to t space]} (read-body req)
        rgba (color/blend from to (double (or t 0.5))
                          (cond-> {} space (assoc :space space)))]
    (edn-response {:hex (color/rgba->hex rgba) :rgba rgba})))

(defn- handle-convert
  "Bidirectional conversion. Accepts one of :hex, :rgba, :oklab [L a b], or
   :oklch [L C H] and returns them all. The studio works in 3-element
   oklab/oklch; color.clj carries alpha as a 4th element, so we append 1.0
   going in and take the first 3 coming out."
  [req]
  (let [{:keys [hex rgba oklab oklch]} (read-body req)
        rgba (cond
               hex   (color/hex->rgba hex)
               rgba  rgba
               oklch (color/oklab->rgba (color/oklch->oklab (conj (vec oklch) 1.0)))
               oklab (color/oklab->rgba (conj (vec oklab) 1.0))
               :else (throw (ex-info "convert needs :hex, :rgba, :oklab, or :oklch" {})))
        lab  (color/rgba->oklab rgba)]
    (edn-response
     {:hex   (color/rgba->hex rgba)
      :rgba  rgba
      :hsluv (color/rgba->hsluv rgba)
      :oklab (vec (take 3 lab))
      :oklch (vec (take 3 (color/oklab->oklch lab)))})))

(def routes
  [[:get  "/" page-html]
   [:get  "/gallery.cljs" cljs-source]
   [:get  "/gallery_styles.css" css-source]

   [:get  "/api/palettes" #(edn-response (catalog))]

   [:post "/api/register"   #(safely handle-register %)]
   [:post "/api/unregister" #(safely handle-unregister %)]
   [:post "/api/save"       #(safely handle-save %)]
   [:post "/api/ramp"       #(safely handle-ramp %)]
   [:post "/api/blend"      #(safely handle-blend %)]
   [:post "/api/convert"    #(safely handle-convert %)]])

(defn- handler [{:keys [request-method uri] :as req}]
  (if-let [[_ _ f] (some (fn [[m u f]]
                           (when (and (= m request-method)
                                      (= u uri))
                             [m u f]))
                         routes)]
    (f req)
    {:status 404
     :headers {"Content-Type" "text/plain"}
     :body "not found"}))

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

    (and (= :post request-method) (= "/api/ramp" uri))
    (safely handle-ramp req)

    (and (= :post request-method) (= "/api/blend" uri))
    (safely handle-blend req)

    (and (= :post request-method) (= "/api/convert" uri))
    (safely handle-convert req)

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