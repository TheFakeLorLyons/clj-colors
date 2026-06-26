(ns clj-colors.app.gallery
  "Top-level gallery namespace. Composes the manager namespaces' route
   registrations, serves the static assets (HTML shell, CLJS sources,
   CSS), and provides the user-facing serve!/stop! lifecycle."
  (:require [clojure.java.io :as io]
            [clj-colors.app.server :as server]
            [clj-colors.app.color-ops :as color-ops]
            [clj-colors.app.palette-manager :as palette-mgr]
            [clj-colors.app.association-manager :as assoc-mgr]
            [clj-colors.app.proposal :as proposal]
            [clj-colors.main :as main]))

(def ^:private scittle-version "0.8.31")

;; --- CLJS file ordering ---
;;
;; gallery.cljs declares the shared 'gallery' namespace that the manager
;; CLJS files refer to. So gallery.cljs must load FIRST. The managers
;; load next; their order between each other doesn't matter. proposal_gui
;; loads last because it references both managers' helpers.

(def ^:private cljs-files
  ["gallery.cljs"
   "palette_manager.cljs"
   "association_manager.cljs"
   "proposal_gui.cljs"])

;; --- Static assets ---

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
       ;; Each CLJS file becomes its own Scittle tag, loaded in order.
       (apply str
              (for [f cljs-files]
                (str "<script type='application/x-scittle' src='/" f "'></script>")))
       "</body></html>"))

(defn- read-resource
  "Load a resource file from the gallery resources directory."
  [filename]
  (if-let [r (io/resource (str "gallery/" filename))]
    (slurp r)
    (throw (ex-info (str "resources/gallery/" filename " not found on classpath")
                    {:filename filename}))))

(defn- handle-page [_req]
  (server/html-response (page-html)))

(defn- handle-css [_req]
  (server/text-response "text/css" (read-resource "gallery_styles.css")))

(defn- make-cljs-handler [filename]
  (fn [_req]
    (server/text-response "text/plain" (read-resource filename))))

;; --- Route registration ---

(defn- register-gallery-routes! []
  (server/register-route! :get "/"                    handle-page)
  (server/register-route! :get "/gallery_styles.css"  handle-css)
  ;; One route per CLJS file, served from the gallery resources dir.
  (doseq [f cljs-files]
    (server/register-route! :get (str "/" f) (make-cljs-handler f))))

(defn- register-all-routes! []
  (server/reset-routes!)
  (register-gallery-routes!)
  (color-ops/register-routes!)
  (palette-mgr/register-routes!)
  (assoc-mgr/register-routes!)
  (proposal/register-routes!))

;; --- Lifecycle ---

(defn stop! []
  (server/stop!))

(defn serve!
  "Start (or restart) the gallery server. Returns the URL.
   opts: :port (default 8350), :open? (default true)."
  ([] (serve! {}))
  ([opts]
   (main/refresh-all-attributes!)
   (register-all-routes!)
   (server/serve! opts)))

(comment
  (serve!)
  (stop!))