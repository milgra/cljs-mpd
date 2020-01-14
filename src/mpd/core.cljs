(ns ^:figwheel-hooks mpd.core
  (:require [goog.dom :as gdom]
            [goog.events :as events]
            [tubax.core :refer [xml->clj]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! chan put! take! poll!]]
            [cljs.core.async :refer-macros [go]]
            [mpd.phys2 :as phys2]
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

        points '([(10 200) (300 430) (500 430) (700 0) (990 200)])

        massa (phys2/mass2 210.0 200.0 1.0 1.0 0.9)
        massb (phys2/mass2 310.0 200.0 1.0 1.0 0.9)
        massc (phys2/mass2 410.0 200.0 1.0 1.0 0.9)

        ;;masses {:a massa :b massb :c massc}

        masses (reduce
                (fn [result number]
                  (let [id (keyword (str number))
                        mass (phys2/mass2 (rand 900) (rand 400) 1.0 1.0 0.9)]
                   (assoc result id mass)))
                {}
                (range 0 100))
        
        dguards [(phys2/dguard2 :a :b 100.0 0.8)
                 (phys2/dguard2 :b :c 100.0 0.8)]
                 ;;(phys2/dguard2 :c :a 100.0 0.8)]

        aguards [(phys2/aguard2 :a :b :c (/ Math/PI 2) Math/PI)]

        surfaces (phys2/surfaces-from-pointlist points)

        lines (apply concat (partition 2 1 (apply concat points)))

        state {:drawer drawer
               :dguards dguards
               :masses masses}]

    (init-events! keych tchch)
    
    (resize-context!)
    
    (animate
     state
     (fn [oldstate frame time]
       (if (= 0 (mod frame 1))
       (let [projection (math4/proj_ortho 0.0 1000.0 500.0 0.0 -1.0 1.0)

             keyevent (poll! keych)

             tchevent (poll! tchch)

             newmasses (-> (:masses oldstate)
                           (phys2/add-gravity 1.0)
                           ;;(phys2/keep-angles aguards)
                           ;;(phys2/keep-distances dguards 1.0)
                           (phys2/move-masses surfaces 1.0))

             ;; draw mass points and surfaces
             newdrawer (-> (:drawer oldstate)
                            (webgl/drawlines! projection lines)
                            (webgl/drawpoints! projection (map :trans (vals newmasses))))]

         ;; return with new state
         (-> oldstate
             (assoc :masses newmasses)
             (assoc :drawer newdrawer)))
       oldstate)
       ))))

(main)
