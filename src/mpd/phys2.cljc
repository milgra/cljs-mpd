(ns mpd.phys2
  (:require [mpd.math2 :as math2]))


(defn timescale [masses delta]
  "check collisions and move masses to new positions considering collisions"
  (reduce
   (fn [ result [mid { dir :d :as mass } ] ]
        (assoc result mid (assoc mass :d (math2/scale-v2 dir delta))))
   masses
   masses))


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


(defn get-colliding-surfaces [pos dir radius surfaces]
  "collect surfaces crossed by masspoint dir or nearby endpoint"
  (reduce
   (fn [result surface]
     (let [isp (math2/isp-v2-v2
                pos
                dir
                (surface :trans)
                (surface :basis)
                radius)
           end (math2/add-v2 pos dir)
           dst (math2/dist-p2-v2 end (:trans surface) (:basis surface))]
       (if (not= isp nil)
         (conj result [(math2/dist-p2-p2-cubic pos isp) surface])
         (if (< dst radius)
           (conj result [dst surface])
           result))))
     []
     surfaces))


(defn move-mass-back [[tx ty][bx by][mx my][mbx mby] radius]
  "moves line closer to mass with radius, gets isp"
  (let [[cx cy] (math2/isp-l2-l2 [tx ty][bx by][mx my][by (- bx)])
        [dx dy] [(- mx cx)(- my cy)]
        [nx ny] (math2/resize-v2 [dx dy] radius)
        [fx fy] (math2/add-v2 [cx cy] [nx ny])]
    ;;(println "mx" mx "my" my "bx" bx "by" by "cx" cx "cy" cy "dx" dx "dy" dy "nx" nx "ny" ny "fx" fx "fy" fy)
    (math2/isp-l2-l2 [fx fy] [bx by] [mx my] [mbx mby])))


(defn mass2 [x y radius weight elasticity]
  "create basic structure"
  {:p [x y] ;; position
   :d [0 0] ;; direction
   :w weight
   :r radius
   :e elasticity})


(defn dguard2 [massa massb distance elasticity]
  "create distance guard"
  {:a massa
   :b massb
   :d distance
   :e elasticity})


(defn keep-distances [masses dguards]
  (reduce
   (fn [result dguard]
     (let [{:keys [a b d e]} dguard
           {ta :p ba :d :as massa} (get result a)
           {tb :p bb :d :as massb} (get result b)
           fa (math2/add-v2 ta ba)
           fb (math2/add-v2 tb bb)
           conn (math2/sub-v2 fa fb)
           dist (- (math2/length-v2 conn) d)]
       (if (> (Math/abs dist) 0.01)
         (let [newdist (if (> e 0.0)
                          (/ dist e)
                          dist)
               conna (math2/resize-v2 conn (- (* newdist 0.5)))
               connb (math2/resize-v2 conn (* newdist 0.5 ))
               newmassa (assoc massa :d (math2/add-v2 ba conna))
               newmassb (assoc massb :d (math2/add-v2 bb connb))]
           (-> result
               (assoc a newmassa)
               (assoc b newmassb)))
         result)))
       masses
       dguards))


(defn aguard2 [massa massb massc minangle maxangle]
  "create angle guard"
  {:a massa
   :b massb
   :c massc
   :min minangle
   :max maxangle})


(defn keep-angles [masses aguards]
  (reduce
   (fn [result aguard]
     (let [{:keys [a b c min max]} aguard
           {pa :p da :d :as massa} (get result a)
           {pb :p db :d :as massb} (get result b)
           {pc :p dc :d :as massc} (get result c)
           fa (math2/add-v2 pa da) ;; final a
           fb (math2/add-v2 pb db) ;; final b
           fc (math2/add-v2 pc dc) ;; final c
           fda (math2/sub-v2 fa fb)
           fdc (math2/sub-v2 fc fb)
           fdalength (math2/length-v2 fda)
           fdclength (math2/length-v2 fdc)
           angleda (math2/angle-x-v2 fda)
           angledc (math2/angle-x-v2 fdc)
           anglere (math2/normalize-angle (- angledc angleda))] ;; ccw angle difference 
       (if (or (< anglere min) (> anglere max))
         (let [diffmin (math2/normalize-angle (- min anglere)) ;; ccw delta
               diffmax (math2/normalize-angle (- anglere max)) ;; ccw delta
               ;; using smaller angle difference
               newangleda (if (< diffmin diffmax)
                            (- angleda ( * diffmin 0.5 ) )
                            (+ angleda ( * diffmax 0.5 ) ))
               newangledc (if (< diffmin diffmax)
                            (+ angledc ( * diffmin 0.5 ) )
                            (- angledc ( * diffmax 0.5 ) ))
               ;; calculate rotated da and dc
               ndax ( + (pb 0) (* (Math/cos newangleda) fdalength ))
               nday ( + (pb 1) (* (Math/sin newangleda) fdalength ))
               ndcx ( + (pb 0) (* (Math/cos newangledc) fdclength ))
               ndcy ( + (pb 1) (* (Math/sin newangledc) fdclength ))
               ;; calculate forces. b will move backwards because we rotate da and dc around their centers
               force_a (math2/scale-v2 (math2/sub-v2 [ndax nday] fa) 0.4)
               force_c (math2/scale-v2 (math2/sub-v2 [ndcx ndcy] fc) 0.4)
               force_b (math2/scale-v2 (math2/add-v2 force_a force_c) -0.1)
               ;; update dires
               newmassa (assoc massa :d ( math2/add-v2 da force_a))
               newmassb (assoc massb :d ( math2/add-v2 db force_b))
               newmassc (assoc massc :d ( math2/add-v2 dc force_c))]
           (-> result
               (assoc a newmassa)
               (assoc b newmassb)
               (assoc c newmassc)))
         result)))
   masses
   aguards))

;; in case of intersection with surface, move mass back to safe distance (radius from surface)
;; mirror dir on surface and reduce it with passed distance
;; if dir is smaller than radius stop movement

(defn move-mass [{:keys [p d r e] :as mass}
                 surfaces]
  "check collision of mass dir with all surfaces, moves mass to next iteration point based on time"
  (loop [ppos p ;; previous position
         pdir d ;; previous direction
         fdir d ;; final direction
         done false ;; movement done
         iter 0 ] ;; iterations
    (if done
      (-> mass
          (assoc :p ppos)
          (assoc :d fdir))          
      (let [segments (map second (sort-by first < (get-colliding-surfaces ppos pdir r surfaces)))
            {strans :trans sbasis :basis :as segment} (first segments)]
        (if segment
          (let [newpos (move-mass-back strans sbasis ppos pdir (* 2.0 r))
                fullsize (math2/length-v2 pdir)
                currsize (math2/length-v2 (math2/sub-v2 newpos ppos))
                usedsize (if (< currsize r)
                           r
                           (- fullsize currsize))
                newfdir (math2/scale-v2 (math2/mirror-v2-bases sbasis fdir) e)
                newdir (math2/resize-v2 newfdir usedsize)
                ready (or (> iter 4) (not segment))]
            (recur newpos newdir newfdir ready (inc iter)))
          (recur (math2/add-v2 ppos pdir) pdir fdir true (inc iter)))))))


(defn move-masses [masses surfaces]
  "check collisions and move masses to new positions considering collisions"
  (reduce
   (fn [result [mid mass]]
     (let [newmass (move-mass mass surfaces)]
       (assoc result mid newmass)))
   masses
   masses))


(defn add-gravity [masses gravity]
  "adds gravity vector to masspoints dires"
  (reduce
   (fn [result [mid mass]]
     (let [dir (:d mass)
           newmass (assoc mass :d (math2/add-v2 dir gravity))]
       (assoc result mid newmass)))
   masses
   masses))
