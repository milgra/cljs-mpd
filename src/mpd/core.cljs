(ns ^:figwheel-hooks mpd.core
  (:require [goog.dom :as gdom]
            [goog.events :as events]
            [tubax.core :refer [xml->clj]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! chan put! take! poll!]]
            [cljs.core.async :refer-macros [go]]
            [mpd.surface :as surface]
            [mpd.svg :as svg]
            [mpd.mass :as mass]
            [mpd.math4 :as math4]
            [mpd.webgl :as webgl])
  (:import [goog.events EventType]))
  

(defn load-level! [channel name]
  (go
    (let [response (<! (http/get name
                                 {:with-credentials? false}))
          xmlstr (xml->clj (:body response) {:strict false})
          shapes (svg/psvg xmlstr "")]
      (put! channel shapes))))


(defn animate [state draw-fn]
  (letfn [(loop [oldstate frame]
            (fn [time]
              (let [newstate (draw-fn oldstate frame time)]
              (.requestAnimationFrame js/window (loop newstate (inc frame))))
              ))]
    ((loop state 0) 0 )))


(defn resize-context! [ ]
  (let [canvas (. js/document getElementById "main")]
        (set! (. canvas -width) (. js/window -innerWidth))
        (set! (. canvas -height) (. js/window -innerHeight))))


(defn main []

  (let
      [initstate {:glstate (webgl/init)
                  :level_file "level0.svg"
                  :level_state "none"
                  :keypresses {}
                  :masses [(mass/mass2 100.0 300.0 1.0 1.0 1.0)]}
       filechannel (chan)
       keychannel (chan)]

    ;; key listeners

    (events/listen
     js/document
     EventType.KEYDOWN
     (fn [event] (put! keychannel {:code (.-keyCode event) :value true})))

    (events/listen
     js/document
     EventType.KEYUP
     (fn [event] (put! keychannel {:code (.-keyCode event) :value false})))

    (events/listen
     js/window
     EventType.RESIZE
     (fn [event]
       (resize-context!)))

    (resize-context!)
    
    ;; runloop
    
    (animate
     initstate
     (fn [state frame time]
       (cond 
         
         (= (:level_state state) "none")
         (do
           (load-level! filechannel (:level_file state))
           (assoc state :level_state "loading"))
         
         (= (:level_state state) "loading")
         (let [shapes (poll! filechannel)]
           (if shapes
             (let [surfacepts (filter #(and (= (% :id) "Surfaces") (not (contains? % :color))) shapes )
                   lines (partition 2 (flatten (map (fn [shape]
                                (partition 2 (flatten (partition 2 1 (:path shape)))))
                              surfacepts)))]
 
               (-> state
                   (assoc :surfaces (surface/generate-from-pointlist surfacepts))
                   (assoc :lines lines )
                   (assoc :level_state "loaded")))
               state))


         (= (:level_state state) "loaded")
         (let [r (/ (.-innerWidth js/window) (.-innerHeight js/window) )
               h 300.0
               w (* h r)
               projection (math4/proj_ortho
                           (- (* w 2))
                           (+ (* w 2))
                           (+ (* h 2))
                           (- (* h 2))
                           -1.0
                           1.0)
               
               keyevent (poll! keychannel)
               
               surfaces (:surfaces state)
               masses (:masses state)

               newmasses (mass/update-masses masses surfaces 1.0)

               newstate (-> state
                            (assoc :masses newmasses))]
           
           (webgl/drawlines! (:glstate state) projection (:lines state))
           (webgl/drawpoints! (:glstate state) projection (map :trans newmasses))

           newstate
           )
         )
       )
     )
    )
  )

;; template functions

;;(println "AA This text is printed from src/brawl/core.cljs. Go ahead and edit it and see reloading in action.")

(def app-state (atom {}))

(defn multiply [a b] (* a b))

(defn get-app-element []
  (gdom/getElement "app"))

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  (swap! app-state update-in [:__figwheel_counter] inc)
  (println "app-state" app-state)
)

;; start entry point, can we do this from project.clj?
(main)
