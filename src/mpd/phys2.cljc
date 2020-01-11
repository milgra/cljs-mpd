(ns mpd.phys2
  (:require [mpd.math2 :as math2]))


(defn surfaces-from-pointlist
  "Generates physics/segment2-s from surface point list"
  [surfacepoints]
  (loop [src surfacepoints
         res []]
    (if (not-empty src)
      (concat res
       (reduce
        (fn builder [res [x y]] (conj res (math2/segment2 x y)))
        []
        (partition 2 1 (first src))))
       (recur (rest src) res))))


(defn get-colliding-surfaces [mass surfaces]
  "collect surfaces crossed by masspoint basis or nearby endpoint"
  (let [trans (:trans mass)
        basis (:basis mass)
        radius (:radius mass)]
    (reduce
     (fn [result surface]
       (let [isp (math2/isp-v2-v2
                  trans
                  basis
                  (surface :trans)
                  (surface :basis)
                  radius)
             end (math2/add-v2 trans basis)
             near (math2/p2-near-v2? end (:trans surface) (:basis surface) radius)]
         (if (not= cross nil)
           (conj result [(math2/dist-p2-p2-cubic trans isp) surface])
           (if near
             (conj result [radius surface])
             result))))
     []
     surfaces)))


(defn move-mass-back [[tx ty][bx by][mx my][mbx mby] radius]
  (let [[cx cy] (math2/isp-l2-l2 [tx ty][bx by][mx my][(- by) bx])
        [dx dy] [(- mx cx)(- my cy)]
        [nx ny] (math2/resize-v2 [dx dy] radius)
        [fx fy] (math2/add-v2 [cx cy] [nx ny])]
    (math2/isp-l2-l2 [fx fy] [bx by] [mx my] [mbx mby])))


(defn mass2
  "create basic structure"
  [x y r w e]
  {:trans [x y]
   :basis [0 0]
   :weight w
   :radius r
   :elasticity e
   :segmentgroups []})


(defn move-mass [{:keys [trans basis radius elasticity] :as mass}
                 surfaces
                 time]
  "check collision of mass basis with all surfaces, moves mass to next iteration point based on time"
  ;;(loop [prevtrans trans
  ;;       prevbasis basis]
         
  (let [segments (map second (sort-by first < (get-colliding-surfaces mass surfaces)))

        ;; in case of intersection with surface, move mass back to safe distance (radius from surface)
        ;; mirror basis on surface and reduce it with passed distance
        ;; in case of multiple surfaces revert basis and move mass back to safe distance from all surfaces
        ;; repeat while basis exists
        ;; if basis is smaller than radius stop movement

        segment (first segments)
        
        newtrans (if (not-empty segments)
                   (move-mass-back (:trans segment) (:basis segment) trans basis (* 1.1 radius))
                   (math2/add-v2 trans basis))

        newbasis (if (not-empty segments)
                   (math2/scale-v2 (math2/mirror-v2-bases (segment :basis) basis) elasticity)
                   basis)]

    (if segment
      (println "newtrans" newtrans "newbasis" newbasis "segments" segments ))
    
    (-> mass
        (assoc :trans newtrans)
        (assoc :basis newbasis))))


(defn moves-masses
  "check collisions and move masses to new positions considering collisions"
  [masses surfaces time]
  (map (fn [element]
         (move-mass element surfaces time))
       masses))

(defn add-gravity
  "adds gravity vector to masspoints basises"
  [masses gravity time]
  (map (fn [{[bx by] :basis :as element}]
         (assoc element :basis [bx (+ by 0.5)]))
       masses))


(defn update-masses
  "Moves masses to new positions considering collisions"
  [masses surfaces time]
  (let [newmasses
        (-> masses
            (add-gravity 0.5 time)
            (moves-masses surfaces time))]
    newmasses))
