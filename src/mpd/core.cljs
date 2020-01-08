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
                (.requestAnimationFrame
                 js/window
                 (loop newstate (inc frame))))))]
    ((loop state 0) 0 )))


(defn resize-context! [ ]
  (let [canvas (. js/document getElementById "main")]
        (set! (. canvas -width) (. js/window -innerWidth))
        (set! (. canvas -height) (. js/window -innerHeight))))


(defn main []
  "entering point"
  (let [keych (chan)

        tchch (chan)

        filech (chan)
        
        state {:glstate (webgl/init)
               :level_file "level0.svg"
               :level_state "none"
               :keypresses {}
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
     (fn [event]
       (resize-context!)))

    (resize-context!)
    
    (animate
     state
     (fn [oldstate frame time]
       (cond 
         
         (= (:level_state oldstate) "none")
         (do
           (load-level! filech (:level_file oldstate))
           (assoc oldstate :level_state "loading"))
         
         (= (:level_state oldstate) "loading")
         (let [shapes (poll! filech)]
           (if shapes
             (let [surfacepts (filter #(and (= (% :id) "Surfaces") (not (contains? % :color))) shapes )
                   lines (partition 2 (flatten (map (fn [shape]
                                (partition 2 (flatten (partition 2 1 (:path shape)))))
                              surfacepts)))]
 
               (-> oldstate
                   (assoc :surfaces (surface/generate-from-pointlist surfacepts))
                   (assoc :lines lines )
                   (assoc :level_state "loaded")))
               oldstate))

         (= (:level_state oldstate) "loaded")
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
               
               surfaces (:surfaces oldstate)
               
               masses (:masses oldstate)

               newmasses (mass/update-masses masses surfaces 1.0)

               newstate (-> oldstate
                            (assoc :masses newmasses))]

           (webgl/drawlines! (:glstate oldstate) projection (:lines oldstate))
           (webgl/drawpoints! (:glstate oldstate) projection (map :trans newmasses))

           newstate))))))

(main)
