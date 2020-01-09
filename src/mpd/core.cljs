(ns ^:figwheel-hooks mpd.core
  (:require [goog.dom :as gdom]
            [goog.events :as events]
            [tubax.core :refer [xml->clj]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! chan put! take! poll!]]
            [cljs.core.async :refer-macros [go]]
            [mpd.svg :as svg]
            [mpd.mass :as mass]
            [mpd.math4 :as math4]
            [mpd.webgl :as webgl])
  (:import [goog.events EventType]))
  

(defn animate [state draw-fn]
  "main runloop, syncs animation to display refresh rate"
  (letfn [(loop [oldstate frame]
            (fn [time]
              (let [newstate (draw-fn oldstate frame time)]
                (.requestAnimationFrame
                 js/window
                 (loop newstate (inc frame))))))]
    ((loop state 0) 0 )))


(defn resize-context! [ ]
  "resizes canvas on window size change"
  (let [canvas (. js/document getElementById "main")]
        (set! (. canvas -width) (. js/window -innerWidth))
        (set! (. canvas -height) (. js/window -innerHeight))))


(defn init-events! [keych tchch]

    (events/listen
     js/document
     EventType.KEYDOWN
     (fn [event] (put! keych {:code (.-keyCode event) :value true})))

    (events/listen
     js/document
     EventType.KEYUP
     (fn [event] (put! keych {:code (.-keyCode event) :value false})))

    (events/listen                          
     js/document
     EventType.MOUSEDOWN
     (fn [event] (put! tchch {:code "mouse" :x (.-clientX event) :y (.-clientY event) :type "down"})))
    
    (events/listen
     js/document         
     EventType.MOUSEUP           
     (fn [event] (put! tchch {:code "mouse" :x (.-clientX event) :y (.-clientY event) :type "up"})))

    (events/listen
     js/window
     EventType.RESIZE
     (fn [event] (resize-context!))))


(defn main []
  "entering point"
  (let [keych (chan)

        tchch (chan)

        drawer (webgl/init)

        points '([(10 200) (10 400) (500 430) (990 400) (990 200)])

        masses [(mass/mass2 100.0 0.0 1.0 1.0 1.0)]
               
        surfaces (mass/surfaces-from-pointlist points)

        lines (apply concat (partition 2 1 (apply concat points)))

        state {:drawer drawer
               :masses masses}]

    (init-events! keych tchch)
    
    (resize-context!)
    
    (animate
     state
     (fn [oldstate frame time]
       ;;(if (= 0 (mod time 5))
       (let [projection (math4/proj_ortho 0.0 1000.0 500.0 0.0 -1.0 1.0)

             keyevent (poll! keych)

             tchevent (poll! tchch)

             ;; move and collide masses to get new position
             newmasses (mass/update-masses (:masses oldstate) surfaces 1.0)

             ;; draw mass points and surfaces
             newdrawer (-> (:drawer oldstate)
                            (webgl/drawlines! projection lines)
                            (webgl/drawpoints! projection (map :trans newmasses)))]

         ;; return with new state
         (-> oldstate
             (assoc :masses newmasses)
             (assoc :drawer newdrawer)))
       ;;oldstate)
       ))))

(main)
