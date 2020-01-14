(ns ^:figwheel-hooks mpd.core
  (:require [goog.dom :as gdom]
            [goog.events :as events]
            [tubax.core :refer [xml->clj]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! chan put! take! poll!]]
            [cljs.core.async :refer-macros [go]]
            [mpd.phys2 :as phys2]
            [mpd.math4 :as math4]
            [mpd.webgl :as webgl]
            [mpd.scenes :as scenes])
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


(defn draw-world [state]
  (let [projection (math4/proj_ortho 0.0 1000.0 500.0 0.0 -1.0 1.0)
        scene (:scene state)]
    (webgl/drawlines! (:drawer state) projection (:lines scene))
    (webgl/drawpoints! (:drawer state) projection (map :p (vals (:masses scene))))))


(defn update-world [state delta]
  (let [newmasses (-> (:masses state)
                      (phys2/add-gravity [0.0 0.5])
                      (phys2/timescale delta)
                      (phys2/keep-angles (:aguards state))
                      (phys2/keep-distances (:dguards state))
                      (phys2/move-masses (:surfaces state))
                      (phys2/timescale (/ 1.0 delta)))]
    (assoc state :masses newmasses)))


(defn main []
  "entering point"
  (let [tchch (chan)
        drawer (webgl/init)
        state {:drawer drawer
               :scene (scenes/scene1)
               :time 0}]

    (init-events! tchch)
    (resize-context!)
    
    (animate
     state
     (fn [{ pretime :time :as prestate} frame time]
       (if (> pretime 0) ;; avoid invalid time delta
         (if (= 0 (mod frame 1)) ;; frame skipping if needed
           (let [delta (/ (- time pretime) 16.8)] ;; natural stepping is 60 fps
             (loop [currdelta delta
                    currstate prestate]
               (if (< currdelta 0.01)
                 (do
                   (draw-world currstate)
                   (assoc currstate :time time))
                 (let [actualdelta (if (> currdelta 0.99) 0.99 currdelta)
                       tchevent (poll! tchch)
                       newscene (update-world (:scene currstate) actualdelta)
                       newstate (assoc currstate :scene newscene)]
                   (recur (- currdelta actualdelta) newstate)))))
          prestate)
         (assoc prestate :time time)))
     )))

(main)
         
