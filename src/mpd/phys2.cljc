(ns mpd.phys2
  (:require [mpd.math2 :as math2]))


(defn timescale [masses delta]
  "check collisions and move masses to new positions considering collisions"
  (reduce
   (fn [ result [mid { basis :basis :as mass } ] ]
        (assoc result mid (assoc mass :basis (math2/scale-v2 basis delta))))
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


(defn get-colliding-surfaces [trans basis radius surfaces]
  "collect surfaces crossed by masspoint basis or nearby endpoint"
  (reduce
   (fn [result surface]
     (let [isp (math2/isp-v2-v2
                trans
                basis
                (surface :trans)
                (surface :basis)
                radius)
           end (math2/add-v2 trans basis)
           dst (math2/dist-p2-v2 end (:trans surface) (:basis surface))]
       (if (not= isp nil)
         (conj result [(math2/dist-p2-p2-cubic trans isp) surface])
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


(defn mass2 [x y r w e]
  "create basic structure"
  {:trans [x y]
   :basis [0 0]
   :weight w
   :radius r
   :elasticity e})


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
           {ta :trans ba :basis :as massa} (get result a)
           {tb :trans bb :basis :as massb} (get result b)
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
               newmassa (assoc massa :basis (math2/add-v2 ba conna))
               newmassb (assoc massb :basis (math2/add-v2 bb connb))]
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
           {ta :trans ba :basis :as massa} (get result a)
           {tb :trans bb :basis :as massb} (get result b)
           {tc :trans bc :basis :as massc} (get result c)
           fa (math2/add-v2 ta ba)
           fb (math2/add-v2 tb bb)
           fc (math2/add-v2 tc bc)
           fba (math2/sub-v2 fa fb) ;; fb?
           fbc (math2/sub-v2 fc fb) ;; fb?
           fbalength (math2/length-v2 fba)
           fbclength (math2/length-v2 fbc)
           angleba (math2/angle-x-v2 fba)
           anglebc (math2/angle-x-v2 fbc)
           anglere (math2/normalize-angle (- anglebc angleba))] ;; ccw angle difference 
       (if (or (< anglere min) (> anglere max))
         (let [diffmin (math2/normalize-angle (- min anglere)) ;; ccw delta
               diffmax (math2/normalize-angle (- anglere max)) ;; ccw delta
               ;; using smaller angle difference
               newangleba (if (< diffmin diffmax)
                            (- angleba ( * diffmin 0.5 ) )
                            (+ angleba ( * diffmax 0.5 ) ))
               newanglebc (if (< diffmin diffmax)
                            (+ anglebc ( * diffmin 0.5 ) )
                            (- anglebc ( * diffmax 0.5 ) ))
               ;; calculate rotated ba and bc
               nbax ( + (tb 0) (* (Math/cos newangleba) fbalength ))
               nbay ( + (tb 1) (* (Math/sin newangleba) fbalength ))
               nbcx ( + (tb 0) (* (Math/cos newanglebc) fbclength ))
               nbcy ( + (tb 1) (* (Math/sin newanglebc) fbclength ))
               ;; calculate forces. b will move backwards because we rotate ba and bc around their centers
               force_a (math2/scale-v2 (math2/sub-v2 [nbax nbay] fa) 0.4)
               force_c (math2/scale-v2 (math2/sub-v2 [nbcx nbcy] fc) 0.4)
               force_b (math2/scale-v2 (math2/add-v2 force_a force_c) -0.1)
               ;; update basises
               newmassa (assoc massa :basis ( math2/add-v2 ba force_a))
               newmassb (assoc massb :basis ( math2/add-v2 bb force_b))
               newmassc (assoc massc :basis ( math2/add-v2 bc force_c))]
           (-> result
               (assoc a newmassa)
               (assoc b newmassb)
               (assoc c newmassc)))
         result)))
   masses
   aguards))

;; in case of intersection with surface, move mass back to safe distance (radius from surface)
;; mirror basis on surface and reduce it with passed distance
;; ??? in case of multiple surfaces revert basis and move mass back to safe distance from all surfaces
;; repeat while basis exists
;; if basis is smaller than radius stop movement

(defn move-mass [{:keys [trans basis radius elasticity] :as mass}
                 surfaces]
  "check collision of mass basis with all surfaces, moves mass to next iteration point based on time"
  (loop [ptrans trans
         pbasis basis
         fbasis basis
         finished false
         counter 0]
    (if finished
      (-> mass
          (assoc :trans ptrans)
          (assoc :basis fbasis))          
      (let [segments (map second (sort-by first < (get-colliding-surfaces ptrans pbasis radius surfaces)))
            {strans :trans sbasis :basis :as segment} (first segments)]
        (if segment
          (let [newtrans (move-mass-back strans sbasis ptrans pbasis (* 2.0 radius))
                fullsize (math2/length-v2 pbasis)
                currsize (math2/length-v2 (math2/sub-v2 newtrans ptrans))
                usedsize (if (< currsize radius)
                           radius
                           (- fullsize currsize))
                newfbasis (math2/scale-v2 (math2/mirror-v2-bases sbasis fbasis) elasticity)
                newbasis (math2/resize-v2 newfbasis usedsize)]

            (recur
             newtrans
             newbasis
             newfbasis
             (or (> counter 4) (not segment))
             (inc counter)))
          
          (recur
           (math2/add-v2 ptrans pbasis)
           pbasis
           fbasis
           true
           (inc counter)))))))


(defn move-masses [masses surfaces]
  "check collisions and move masses to new positions considering collisions"
  (reduce
   (fn [result [mid mass]]
     (let [newmass (move-mass mass surfaces)]
       (assoc result mid newmass)))
   masses
   masses))


(defn add-gravity [masses gravity]
  "adds gravity vector to masspoints basises"
  (reduce
   (fn [result [mid mass]]
     (let [basis (:basis mass)
           newmass (assoc mass :basis (math2/add-v2 basis gravity))]
       (assoc result mid newmass)))
   masses
   masses))
