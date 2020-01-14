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
  (letfn [(loop [prestate frame]
            (fn [time]
              (let [newstate (if (> time 0)
                               (draw-fn prestate frame time)
                               prestate)]
                (.requestAnimationFrame
                 js/window
                 (loop newstate (inc frame))))))]
    ((loop state 0) 0 )))


(defn resize-context! [ ]
  "resizes canvas on window size change"
  (let [canvas (. js/document getElementById "main")]
        (set! (. canvas -width) (. js/window -innerWidth))
        (set! (. canvas -height) (. js/window -innerHeight))))


(defn init-events! [tchch]

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
  (let [tchch (chan)

        drawer (webgl/init)

        points '([(10 200) (300 430) (500 430) (700 0) (990 200)])

        massa (phys2/mass2 210.0 100.0 1.0 1.0 0.7)
        massb (phys2/mass2 210.0 200.0 1.0 1.0 0.7)
        massc (phys2/mass2 310.0 200.0 1.0 1.0 0.7)

        masses {:a massa :b massb :c massc}

        massesb (reduce
                (fn [result number]
                  (let [id (keyword (str number))
                        mass (phys2/mass2 (rand 900) (rand 400) 1.0 1.0 0.8)]
                    (assoc result id mass)))
                {}
                (range 0 100))
        
        dguards [(phys2/dguard2 :a :b 100.0 0.8)
                 (phys2/dguard2 :b :c 100.0 0.8)
                 (phys2/dguard2 :c :a 100.0 0.8)
                 ]

        aguards [(phys2/aguard2 :a :b :c (/ Math/PI 2) Math/PI)]

        surfaces (phys2/surfaces-from-pointlist points)

        lines (apply concat (partition 2 1 (apply concat points)))

        projection (math4/proj_ortho 0.0 1000.0 500.0 0.0 -1.0 1.0)

        state {:drawer drawer
               :dguards dguards
               :masses masses
               :faszom "faszom"
               :time 0}]

    (init-events! tchch)
    
    (resize-context!)
    
    (animate
     state
     (fn [{ pretime :time :as prestate} frame time]
       (if (> pretime 0)
         (if (= 0 (mod frame 1))
           (let [delta (/ (- time pretime) 16.8)]
             (loop [currdelta delta
                    currstate prestate]
               (if (< currdelta 0.01)
                 (do
                   (webgl/drawlines! (:drawer currstate) projection lines)
                   (webgl/drawpoints! (:drawer currstate) projection (map :trans (vals (:masses currstate))))
                   currstate)
                 (let [actualdelta (if (> currdelta 0.99)
                                     0.99
                                     currdelta)
 
                       tchevent (poll! tchch)
                       
                       newmasses (-> (:masses currstate)
                                     (phys2/add-gravity [0.0 0.5])
                                     (phys2/timescale actualdelta)
                                     ;;(phys2/keep-angles aguards)
                                     (phys2/keep-distances dguards)
                                     (phys2/move-masses surfaces)
                                     (phys2/timescale (/ 1.0 actualdelta)))
                       
                       newstate (-> currstate
                                    (assoc :time time)
                                    (assoc :masses newmasses))]

                   ;;(println "currdelta actualdelta" currdelta actualdelta)
                   (recur (- currdelta actualdelta) newstate)))))
          prestate)
         (assoc prestate :time time)))
     )))

(main)
         
