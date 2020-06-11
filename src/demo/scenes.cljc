(ns demo.scenes
  (:require [mpd.phys2 :as phys2]
            [mpd.math2 :as math2]))

(defn scene0 [w h]
  "simple masspoints on scattered surface"
  (let [bottom (let [stepping (/ w 20.0)]
                 [(for [x (range 0 21)]
                    (cond
                      (= x 0) [0 0]
                      (= x 1) [3 (- h (rand 100))]
                      (= x 19) [(- w 3) (- h (rand 100))]
                      (= x 20) [w 0]
                      :default [( * x stepping ) (- h (rand 100))]))])

        islanda (let [stepping (/ w 20.0)]
                 [(for [x (range 0 5)] [(+ 100 (* x stepping)) (- 400 (rand 100))])])

        islandb (let [stepping (/ w 20.0)]
                 [(for [x (range 0 5)] [(+ 500 (* x stepping)) (- 200 (rand 100))])])

        points (concat bottom islanda islandb)

        masses (reduce
                (fn [result number]
                  (let [id (keyword (str number))
                        mass (phys2/mass2 ( + 50.0 (rand (- w 100.0 ))) (rand 300) 1.0 1.0 0.1 0.8)]
                    (assoc result id mass)))
                {}
                (range 0 15))
        
        dguards []
        aguards []

        surfaces (phys2/surfaces-from-pointlist points)

        lines (reduce (fn [result {t :t b :b}] (conj result t (math2/add-v2 t b))) [] surfaces)]
    
    {:surfaces surfaces
     :dguards dguards
     :aguards aguards
     :masses masses
     :lines lines
     :index 0}))


(defn scene1 [w h]
  "triangles on scattered surface"
  (let [bottom (let [stepping (/ w 20.0)]
                 [(for [x (range 0 21)]
                    (cond
                      (= x 0) [0 0]
                      (= x 1) [3 (- h (rand 100))]
                      (= x 19) [(- w 3) (- h (rand 100))]
                      (= x 20) [w 0]
                      :default [( * x stepping ) (- h (rand 100))]))])

        islanda (let [stepping (/ w 20.0)]
                 [(for [x (range 0 5)] [(+ 100 (* x stepping)) (- 400 (rand 100))])])

        islandb (let [stepping (/ w 20.0)]
                 [(for [x (range 0 5)] [(+ 500 (* x stepping)) (- 200 (rand 100))])])

        points (concat bottom islanda islandb)

        masses (reduce
                (fn [result number]
                  (let [id (keyword (str number))
                        mass (phys2/mass2 (+ 150.0 (* number 30.0)) (rand 10) 1.0 1.0 0.1 0.5)]
                    (assoc result id mass)))
                {}
                (range 0 21))
        
        dguards (reduce
                 (fn [result [a b c]]
                   (conj result
                         (phys2/dguard2 masses (keyword (str a)) (keyword (str b)) 80.0 0.9)
                         (phys2/dguard2 masses (keyword (str b)) (keyword (str c)) 80.0 0.9)
                         (phys2/dguard2 masses (keyword (str c)) (keyword (str a)) 80.0 0.9)))
                 []
                 (partition 3 3 (range 0 22)))

        aguards []

        surfaces (phys2/surfaces-from-pointlist points)

        lines (reduce (fn [result {t :t b :b}] (conj result t (math2/add-v2 t b))) [] surfaces)]

    {:surfaces surfaces
     :dguards dguards
     :aguards aguards
     :masses masses
     :lines lines
     :index 1}))


(defn scene2 [w h]
  "squares on scattered surface"
  (let [bottom (let [stepping (/ w 20.0)]
                 [(for [x (range 0 21)]
                    (cond
                      (= x 0) [0 0]
                      (= x 1) [3 (- h (rand 100))]
                      (= x 19) [(- w 3) (- h (rand 100))]
                      (= x 20) [w 0]
                      :default [( * x stepping ) (- h (rand 100))]))])

        islanda (let [stepping (/ w 20.0)]
                 [(for [x (range 0 5)] [(+ 100 (* x stepping)) (- 400 (rand 100))])])

        islandb (let [stepping (/ w 20.0)]
                 [(for [x (range 0 5)] [(+ 500 (* x stepping)) (- 200 (rand 100))])])

        points (concat bottom islanda islandb)

        masses (reduce
                (fn [result number]
                  (let [id (keyword (str number))
                        mass (phys2/mass2 (+ 100.0 (* number 30.0)) (rand 10) 1.0 1.0 0.1 0.5)]
                    (assoc result id mass)))
                {}
                (range 0 20))
        
        dguards (reduce
                 (fn [result [a b c d]]
                   (conj result
                         (phys2/dguard2 masses (keyword (str a)) (keyword (str b)) 50.0 0.9)
                         (phys2/dguard2 masses (keyword (str b)) (keyword (str c)) 50.0 0.9)
                         (phys2/dguard2 masses (keyword (str c)) (keyword (str d)) 50.0 0.9)
                         (phys2/dguard2 masses (keyword (str d)) (keyword (str a)) 50.0 0.9)
                         (phys2/dguard2 masses (keyword (str a)) (keyword (str c)) 70.71 0.9)
                         (phys2/dguard2 masses (keyword (str b)) (keyword (str d)) 70.71 0.9)))
                 []
                 (partition 4 4 (range 0 21)))

        aguards []

        surfaces (phys2/surfaces-from-pointlist points)

        lines (reduce (fn [result {t :t b :b}] (conj result t (math2/add-v2 t b))) [] surfaces)]

    {:surfaces surfaces
     :dguards dguards
     :aguards aguards
     :masses masses
     :lines lines
     :index 2}))


(defn scene3 [w h]
  "ragdolls on scattered surface"
  (let [bottom (let [stepping (/ w 20.0)]
                 [(for [x (range 0 21)]
                    (cond
                      (= x 0) [0 0]
                      (= x 1) [3 (- h (rand 100))]
                      (= x 19) [(- w 3) (- h (rand 100))]
                      (= x 20) [w 0]
                      :default [( * x stepping ) (- h (rand 100))]))])

        islanda (let [stepping (/ w 20.0)]
                 [(for [x (range 0 5)] [(+ 100 (* x stepping)) (- 400 (rand 100))])])

        islandb (let [stepping (/ w 20.0)]
                 [(for [x (range 0 5)] [(+ 500 (* x stepping)) (- 200 (rand 100))])])

        points (concat bottom islanda islandb)

        head (phys2/mass2 300.0 60.0 1.0 1.0 0.1 0.7)
        neck (phys2/mass2 300.0 100.0 1.0 1.0 0.1 0.7)
        hip (phys2/mass2 300.0 200.0 1.0 1.0 0.1 0.7)
        lknee (phys2/mass2 250.0 250.0 1.0 1.0 0.1 0.7)
        rknee (phys2/mass2 350.0 250.0 1.0 1.0 0.1 0.7)
        lfoot (phys2/mass2 200.0 300.0 1.0 1.0 0.1 0.7)
        rfoot (phys2/mass2 400.0 300.0 1.0 1.0 0.1 0.7)
        lelbow (phys2/mass2 250.0 100.0 1.0 1.0 0.1 0.7)
        relbow (phys2/mass2 350.0 100.0 1.0 1.0 0.1 0.7)
        lhand (phys2/mass2 200.0 100.0 1.0 1.0 0.1 0.7)
        rhand (phys2/mass2 400.0 100.0 1.0 1.0 0.1 0.7)

        masses {:h head
                :n neck
                :p hip
                :lk lknee
                :rk rknee
                :lf lfoot
                :rf rfoot
                :le lelbow
                :re relbow
                :lh lhand
                :rh rhand}
        
        dguards [(phys2/dguard2 masses :h :n 40.0 0.8)
                 (phys2/dguard2 masses :n :p 100.0 0.8)
                 (phys2/dguard2 masses :p :lk 50.0 0.8)
                 (phys2/dguard2 masses :p :rk 50.0 0.8)
                 (phys2/dguard2 masses :rk :rf 50.0 0.8)
                 (phys2/dguard2 masses :lk :lf 50.0 0.8)
                 (phys2/dguard2 masses :n :le 50.0 0.8)
                 (phys2/dguard2 masses :n :re 50.0 0.8)
                 (phys2/dguard2 masses :re :rh 50.0 0.8)
                 (phys2/dguard2 masses :le :lh 50.0 0.8)]

        aguards [(phys2/aguard2 masses :h :n :p 0 Math/PI 0.5)
                 (phys2/aguard2 masses :p :lk :lf (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
                 (phys2/aguard2 masses :p :rk :rf (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
                 (phys2/aguard2 masses :n :re :rh (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)
                 (phys2/aguard2 masses :n :le :lh (/ Math/PI 2) (/ (* 3  Math/PI) 2) 0.1)]
        
        surfaces (phys2/surfaces-from-pointlist points)

        lines (reduce (fn [result {t :t b :b}] (conj result t (math2/add-v2 t b))) [] surfaces)]

    {:surfaces surfaces
     :dguards dguards
     :aguards aguards
     :masses masses
     :lines lines
     :index 3}))
