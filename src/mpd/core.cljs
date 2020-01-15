(ns ^:figwheel-hooks mpd.core
  (:require [goog.dom :as gdom]
            [goog.events :as events]
            [tubax.core :refer [xml->clj]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! chan put! take! poll!]]
            [cljs.core.async :refer-macros [go]]
            [cljs.js :as cljs]
            [mpd.phys2 :as phys2]
            [mpd.math4 :as math4]
            [mpd.webgl :as webgl]
            [mpd.scenes :as scenes])
  (:import [goog.events EventType]))
  

(defn resize-context! [ ]
  "resizes canvas on window size change"
  (let [canvas (. js/document getElementById "main")]
        (set! (. canvas -width) (. js/window -innerWidth))
        (set! (. canvas -height) (. js/window -innerHeight))))


(defn init-events! [tchch]
  "inits mouse and window events"
    
    (events/listen
     js/document         
     EventType.MOUSEUP           
     (fn [event] (put! tchch {:code "mouse" :x (.-clientX event) :y (.-clientY event) :type "up"})))

    (events/listen
     js/window
     EventType.RESIZE
     (fn [event] (resize-context!))))


(defn draw-world [state]
  "draws surfaces and masspoints"
  (let [projection (math4/proj_ortho
                    0
                    (.-innerWidth js/window)
                    (.-innerHeight js/window)
                    0
                    -10.0
                    10.0)
        
        scene (:scene state)

        masses (:masses scene)
        dguards (:dguards scene)

        nlines (reduce
                (fn [result {a :a b :b :as dguard}]
                  (let [{pa :p :as massa} (masses a)
                        {pb :p :as massb} (masses b)]
                  (conj result pa pb)))
                []
                dguards)]

    (when (> (count nlines) 0) (webgl/drawlines! (:drawer state) projection nlines))
    (webgl/drawlines! (:drawer state) projection (:lines scene))
    (webgl/drawpoints! (:drawer state) projection (map :p (vals (:masses scene))))))


(defn update-world [state delta]
  "updates current masses"
  (let [newmasses (-> (:masses state)
                      (phys2/add-gravity [0.0 0.2])
                      (phys2/timescale delta)
                      (phys2/keep-angles (:aguards state))
                      (phys2/keep-distances (:dguards state))
                      (phys2/move-masses (:surfaces state))
                      (phys2/timescale (/ 1.0 delta)))]
    (assoc state :masses newmasses)))


(defn load-next-scene [{index :index :as scene}]
  "loads next scene from scene list"
  (let [scenes [scenes/scene0
                scenes/scene1
                scenes/scene2
                scenes/scene3]
        nextindex (if (= (inc index) 4) 0 (inc index))]
    (println "load scene" nextindex)
    ((get scenes nextindex)
     (.-innerWidth js/window)
     (.-innerHeight js/window))))


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


(defn main []
  "entering point"
  (let [tchch (chan)
        drawer (webgl/init)
        state {:drawer drawer
               :scene (scenes/scene0 (.-innerWidth js/window)
                                     (.-innerHeight js/window))
               :time 0}]

    (init-events! tchch)
    (resize-context!)
    
    (animate
     state
     (fn [{ pretime :time :as prestate} frame time]
       (if (> pretime 0) ;; avoid invalid time delta
         (if (= 0 (mod frame 1)) ;; skip frames if needed
           (let [delta (/ (- time pretime) 16.8)] ;; natural stepping is 60 fps
             (loop [currdelta delta
                    currstate prestate]
               (if (< currdelta 0.01)
                 (do
                   (draw-world currstate)
                   (assoc currstate :time time))
                 (let [actualdelta (if (> currdelta 0.99) 0.99 currdelta)
                       tchevent (poll! tchch)
                       newscene (if tchevent
                                  (load-next-scene (:scene currstate))
                                  (update-world (:scene currstate) actualdelta))
                       newstate (assoc currstate :scene newscene)]
                   (recur (- currdelta actualdelta) newstate)))))
          prestate)
         (assoc prestate :time time))))))

(main)
         
