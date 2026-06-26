(ns clj-colors.app.color-ops
  "HTTP handlers for color math operations: ramp, blend, convert.
   These are general-purpose color utilities, not palette-specific."
  (:require [clj-colors.app.server :as server]
            [clj-colors.color :as color]))

(defn- handle-ramp
  "Perceptual resample via color/ramp.
   Body: {:colors [hex...] :n N :space :oklab|:oklch :weights [..]?}
   Returns: {:ramp [hex...]}."
  [req]
  (let [{:keys [colors n space weights]} (server/read-body req)]
    (server/edn-response
     {:ramp (color/ramp colors (or n 5)
                        (cond-> {}
                          space         (assoc :space space)
                          (seq weights) (assoc :weights weights)))})))

(defn- handle-blend
  "Two-color blend via color/blend.
   Body: {:from c1 :to c2 :t 0.5 :space :oklab|:oklch}
   Returns: {:hex h :rgba [r g b a]}."
  [req]
  (let [{:keys [from to t space]} (server/read-body req)
        rgba (color/blend from to (double (or t 0.5))
                          (cond-> {} space (assoc :space space)))]
    (server/edn-response {:hex (color/rgba->hex rgba) :rgba rgba})))

(defn- handle-convert
  "Bidirectional color space conversion. Accepts one of :hex, :rgba,
   :oklab, or :oklch and returns them all."
  [req]
  (let [{:keys [hex rgba oklab oklch]} (server/read-body req)
        rgba (cond
               hex   (color/hex->rgba hex)
               rgba  rgba
               oklch (color/oklab->rgba (color/oklch->oklab (conj (vec oklch) 1.0)))
               oklab (color/oklab->rgba (conj (vec oklab) 1.0))
               :else (throw (ex-info "convert needs :hex, :rgba, :oklab, or :oklch" {})))
        lab  (color/rgba->oklab rgba)]
    (server/edn-response
     {:hex   (color/rgba->hex rgba)
      :rgba  rgba
      :hsluv (color/rgba->hsluv rgba)
      :oklab (vec (take 3 lab))
      :oklch (vec (take 3 (color/oklab->oklch lab)))})))

(defn register-routes! []
  (server/register-route! :post "/api/ramp"    #(server/safely handle-ramp %))
  (server/register-route! :post "/api/blend"   #(server/safely handle-blend %))
  (server/register-route! :post "/api/convert" #(server/safely handle-convert %)))