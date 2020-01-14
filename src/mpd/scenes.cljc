(ns mpd.scenes
  (:require [mpd.phys2 :as phys2]))

(defn scene1 []
  (let [points '([(10 200) (300 430) (500 430) (700 0) (990 200)])

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
                 ;;(phys2/dguard2 :c :a 100.0 0.8)
                 ]

        aguards [(phys2/aguard2 :a :b :c (/ Math/PI 2) Math/PI)]

        surfaces (phys2/surfaces-from-pointlist points)

        lines (apply concat (partition 2 1 (apply concat points)))]

    {:surfaces surfaces
     :dguards dguards
     :aguards aguards
     :masses masses
     :lines lines}))
