(ns mpd.scenes
  (:require [mpd.phys2 :as phys2]))

(defn scene0 [w h]
  "simple masspoints on scattered surface"
  (let [points (let [stepping (/ w 20.0)]
                 [(for [x (range 0 21)]
                    (cond
                      (= x 0) [stepping 0]
                      (= x 20) [(- w stepping) 0]
                      :default [( * x stepping ) (- h (rand 200))]))])

        masses (reduce
                (fn [result number]
                  (let [id (keyword (str number))
                        mass (phys2/mass2 ( + 50.0 (rand (- w 100.0 ))) (rand 300) 1.0 1.0 0.8)]
                    (assoc result id mass)))
                {}
                (range 0 15))
        
        dguards []
        aguards []

        surfaces (phys2/surfaces-from-pointlist points)

        lines (apply concat (partition 2 1 (apply concat points)))]
    
    {:surfaces surfaces
     :dguards dguards
     :aguards aguards
     :masses masses
     :lines lines
     :index 0}))


(defn scene1 [w h]
  "triangles on scattered surface"
  (let [points (let [stepping (/ w 20.0)]
                 [(for [x (range 0 21)]
                    (cond
                      (= x 0) [stepping 0]
                      (= x 20) [(- w stepping) 0]
                      :default [( * x stepping ) (- h (rand 200))]))])

        masses (reduce
                (fn [result number]
                  (let [id (keyword (str number))
                        mass (phys2/mass2 (+ 50.0 (* number 40.0)) (+ 100 (rand 10)) 1.0 1.0 0.5)]
                    (assoc result id mass)))
                {}
                (range 0 15))
        
        dguards (reduce
                 (fn [result [a b c]]
                   (conj result
                         (phys2/dguard2 (keyword (str a)) (keyword (str b)) 100.0 0.9)
                         (phys2/dguard2 (keyword (str b)) (keyword (str c)) 100.0 0.9)
                         (phys2/dguard2 (keyword (str c)) (keyword (str a)) 100.0 0.9)))
                 []
                 (partition 3 3 (range 0 16)))

        aguards []

        surfaces (phys2/surfaces-from-pointlist points)

        lines (apply concat (partition 2 1 (apply concat points)))]

    {:surfaces surfaces
     :dguards dguards
     :aguards aguards
     :masses masses
     :lines lines
     :index 1}))


(defn scene2 [w h]
  "squares on scattered surface"
  (let [points (let [stepping (/ w 20.0)]
                 [(for [x (range 0 21)]
                    (cond
                      (= x 0) [stepping 0]
                      (= x 20) [(- w stepping) 0]
                      :default [( * x stepping ) (- h (rand 200))]))])

        masses (reduce
                (fn [result number]
                  (let [id (keyword (str number))
                        mass (phys2/mass2 (+ 50.0 (* number 40.0)) (+ 100 (rand 10)) 1.0 1.0 0.5)]
                    (assoc result id mass)))
                {}
                (range 0 15))
        
        dguards (reduce
                 (fn [result [a b c d]]
                   (conj result
                         (phys2/dguard2 (keyword (str a)) (keyword (str b)) 100.0 0.9)
                         (phys2/dguard2 (keyword (str b)) (keyword (str c)) 100.0 0.9)
                         (phys2/dguard2 (keyword (str c)) (keyword (str d)) 100.0 0.9)
                         (phys2/dguard2 (keyword (str d)) (keyword (str a)) 100.0 0.9)
                         (phys2/dguard2 (keyword (str a)) (keyword (str c)) 141.42 0.9)
                         (phys2/dguard2 (keyword (str b)) (keyword (str d)) 141.42 0.9)))
                 []
                 (partition 4 4 (range 0 16)))

        aguards []

        surfaces (phys2/surfaces-from-pointlist points)

        lines (apply concat (partition 2 1 (apply concat points)))]

    {:surfaces surfaces
     :dguards dguards
     :aguards aguards
     :masses masses
     :lines lines
     :index 2}))


(defn scene3 [w h]
  "ragdolls on scattered surface"
  (let [
        points (let [stepping (/ w 20.0)]
                 [(for [x (range 0 21)]
                    (cond
                      (= x 0) [stepping 0]
                      (= x 20) [(- w stepping) 0]
                      :default [( * x stepping ) (- h (rand 200))]))])

        massa (phys2/mass2 300.0 100.0 1.0 1.0 0.7)
        massb (phys2/mass2 300.0 200.0 1.0 1.0 0.7)
        massc (phys2/mass2 300.0 300.0 1.0 1.0 0.7)
        massd (phys2/mass2 200.0 200.0 1.0 1.0 0.7)
        masse (phys2/mass2 400.0 200.0 1.0 1.0 0.7)

        masses {:a massa :b massb :c massc :d massd :e masse}
        
        dguards [(phys2/dguard2 :a :b 100.0 0.8)
                 (phys2/dguard2 :b :c 100.0 0.8)
                 (phys2/dguard2 :c :d 100.0 0.8)
                 (phys2/dguard2 :d :e 100.0 0.8)]

        aguards [];;(phys2/aguard2 :a :b :c (/ Math/PI 2) Math/PI)]

        surfaces (phys2/surfaces-from-pointlist points)

        lines (apply concat (partition 2 1 (apply concat points)))]

    {:surfaces surfaces
     :dguards dguards
     :aguards aguards
     :masses masses
     :lines lines
     :index 3}))


;; curtain
;; tuske
