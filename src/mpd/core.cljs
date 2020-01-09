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


(defn main []
  "entering point"
  (let [keych (chan)

        tchch (chan)

        points '([(38.613 241.018) (53.321 396.925)
                  (1103.035 464.38) (1204.737 465.087)
                  (2197.789 408.692) (2218.38 258.667)])

        linepts (apply concat (partition 2 1 (apply concat points)))
               
        surfaces (surface/generate-from-pointlist points)

        state {:glstate (webgl/init)
               :masses [(mass/mass2 100.0 300.0 1.0 1.0 1.0)]}]

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
     (fn [event] (resize-context!)))

    (resize-context!)
    
    (animate
     state
     (fn [oldstate frame time]
       (let [r (/ (.-innerWidth js/window) (.-innerHeight js/window) )
             h 300.0
             w (* h r)
             projection (math4/proj_ortho
                         (+ (- (* w 2)) 900.0)
                         (+ (+ (* w 2)) 900.0)
                         (+ (* h 2))
                         (- (* h 2))
                         -1.0
                         1.0)

             keyevent (poll! keych)

             tchevent (poll! tchch)

             ;; move and collide masses to get new position
             newmasses (mass/update-masses (:masses oldstate) surfaces 1.0)

             ;; draw mass points and surfaces
             newglstate (-> (:glstate oldstate)
                            (webgl/drawlines! projection linepts)
                            (webgl/drawpoints! projection (map :trans newmasses)))]

         ;; return with new state
         (-> oldstate
             (assoc :masses newmasses)
             (assoc :glstate newglstate)))))))

(main)
