(ns mpd.math2)


(defn segment2
  "creates segment 2d structure"
  [[x y][ z v]]
  {:trans [x y]
   :basis [(- z x) (- v y)]})


(defn isp-l2-l2 [[tax tay][bax bay][tbx tby][bbx bby]]
  "line-line intersection calculation based on determinant calculation : Ax + By = C
   parameters : trans a basis a trans b basis b"
  (if (or
       (and (= bax 0)(= bay 0))
       (and (= bbx 0)(= bby 0)))
    nil ;; invalid line
    (let [A1 bay
          B1 (- bax)
          C1 (+ (* A1 tax) (* B1 tay))
          A2 bby
          B2 (- bbx)
          C2 (+ (* A2 tbx) (* B2 tby))
          DT (- (* A2 B1) (* B2 A1))]
      (if (= DT 0)
       nil ;; parallel lines
       [( / (- (* B1 C2) (* B2 C1)) DT )
        ( / (- (* A2 C1) (* A1 C2)) DT) ]))))


(defn p2-in-v2? [[tx ty] [bx by] [px py] radius]
  "check if p is inside vector defined by trans and basis with given radius from endpoints
   it checks point distance from the halfpoint of the vector"
  (let [lx (/ bx 2)
        ly (/ by 2)
        cx (+ tx lx)
        cy (+ ty ly)
        dx (- px cx)
        dy (- py cy)
        xok (< (Math/abs dx) (+ (Math/abs lx) radius)) 
        yok (< (Math/abs dy) (+ (Math/abs ly) radius))]
    (and xok yok)))


(defn dist-p2-v2 [[px py] [tx ty] [bx by]]
  "calculate distance of point and vector
   https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line #Line defined by two points"
  (let [cx (+ tx bx)
        cy (+ ty by)]
    (/
     (Math/abs (+ (* by px) (* bx py -1) (* cx ty) (* cy tx -1)))
     (Math/sqrt (+ (* bx bx) (* by by))))))


(defn isp-v2-v2 [transa basisa transb basisb radius]
  "vector - vector intersection calculation with given radius from endpoints"
  (let [cp (isp-l2-l2 transa basisa transb basisb)]
    (if (= cp nil)
     nil
     (if (and
          (p2-in-v2? transa basisa cp radius)
          (p2-in-v2? transb basisb cp radius))
       cp
       nil))))


(defn mirror-v2-bases [[ax ay] [vx vy]]
  "mirrors vector on axis, projects v on a, then adds the connecting vector of v and p to p"
  (let [[px py] (isp-l2-l2 [0 0] [ax ay] [vx vy] [ay (- ax)])
        [bx by] [(- cx vx) (- cy vy)]]
    [(+ px bx) (+ py by)]))


(defn length-v2 [ [ax ay] ]
  (Math/sqrt (+ (* ax ax) (* ay ay))))


(defn resize-v2 [[x y] size]
  (let [ratio (/ size (length-v2 [x y]))]
    [(* x ratio) (* y ratio)]))


(defn scale-v2 [ [x y] ratio ]
  [(* x ratio) (* y ratio)])


(defn sub-v2 [ [ax ay] [bx by] ]
  [(- ax bx) (- ay by)])


(defn add-v2 [[ax ay][bx by]]
  [(+ ax bx) (+ ay by)])


(defn rotate-90-cw [ [x y] ]
  [y (- x)])


(defn rotate-90-ccw [ [x y] ]
  [(- y) x])
