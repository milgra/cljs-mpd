(ns mpd.math2)


(defn segment2
  "creates segment 2d structure"
  [[x y][ z v]]
  {:trans [x y]
   :basis [(- z x) (- v y)]})

;;  Ax + By = C
;;  A = y2 - y1
;;  B = x1 - x2
;;  C = A * x1 + B * y1
;;  C1 = A1 * s1x + B1 * s1y"
(defn isp-l2-l2
  "line-line intersection calculation"
  ;; trans a basis a trans b basis b
  [[tax tay][bax bay][tbx tby][bbx bby]]
  ;; if no length there is no direction
  (if (or
       (and (= bax 0)(= bay 0))
       (and (= bbx 0)(= bby 0)))
    nil
    (let [A1 bay
          B1 (- bax)
          C1 (+ (* A1 tax) (* B1 tay))
          A2 bby
          B2 (- bbx)
          C2 (+ (* A2 tbx) (* B2 tby))
          DT (- (* A2 B1) (* B2 A1))]
      (if (= DT 0)
       ;; if determinant is 0 they are parallel
       nil
       [( / (- (* B1 C2) (* B2 C1)) DT )
        ( / (- (* A2 C1) (* A1 C2)) DT) ]))))

;; (isp-l2-l2 [0 0][100 0][50 50][0 -100])

;; checking is based on ratio checking of original and connecting vector
(defn p2-in-v2?
  "check if p is inside vector defined by trans and basis"
  [[tx ty] [bx by] [px py]]
  (let [dx (- px tx)
        dy (- py ty)
        xok (if (= bx 0)
              (< (Math/abs dx) 0.00001)
              (let [rx (/ dx bx)]
                (and (> rx 0.0)(< rx 1.0))))
        yok (if (= by 0)
              (< (Math/abs dy) 0.00001)
              (let [ry (/ dy by)]
                (and (> ry 0.0)(< ry 1.0))))]
    (and xok yok)))

;; https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line #Line defined by two points 
(defn dist-p2-v2
  "calculate distance of point and vector"
  [[px py] [tx ty] [bx by]]
  (let [cx (+ tx bx)
        cy (+ ty by)]
    (/
     (Math/abs (+ (* by px) (* bx py -1) (* cx ty) (* cy tx -1)))
     (Math/sqrt (+ (* bx bx) (* by by))))))

;;(dist-p2-v2 [10.0 10.0] [0.0 0.0] [100.0 0.0])

(defn isp-v2-v2
  "checks vector crossing"
  [transa basisa transb basisb]
  (let [cp (isp-l2-l2 transa basisa transb basisb)]
    (if (= cp nil)
     nil
     (if (and
          (p2-in-v2? transa basisa cp)
          (p2-in-v2? transb basisb cp))
       cp
       nil))))


(defn mirror-v2-bases
  "mirrors vector on axis"
  ;; axis   vector
  [[ax ay] [vx vy]]
  (let [[px py :as projpt] (isp-l2-l2 [0 0] [ax ay] [vx vy] [ay (- ax)])
        [vx vy :as projvec] [(- px vx) (- py vy)]]
    [(+ px vx) (+ py vy)]))


(defn length_vec2 [ [ax ay] ]
  (Math/sqrt (+ (* ax ax) (* ay ay))))


(defn resize_vec2 [[x y] size]
  (let [ratio (/ size (length_vec2 [x y]))]
    [(* x ratio) (* y ratio)]))


(defn scale_vec2 [ [x y] ratio ]
  [(* x ratio) (* y ratio)])


(defn sub_vec2 [ [ax ay] [bx by] ]
  [(- ax bx) (- ay by)])


(defn add_vec2 [ [ax ay] [bx by] ]
  [(+ ax bx) (+ ay by)])


(defn rotate_90_cw [ [x y] ]
  [y (- x)])

(defn rotate_90_ccw [ [x y] ]
  [(- y) x])


;; TODO optimize by removing sqrt
(defn triangle_with_bases [ va vb side dir ]
  (let [dirv (sub_vec2 vb va)
        half (scale_vec2 dirv 0.5)
        size (length_vec2 half) 
        result (if (< size side)
                 (let [needed (Math/sqrt (- (* side side) (* size size)))
                       normal (resize_vec2 [(* dir (- (half 1))) (* dir (half 0))] needed)]
                   (add_vec2 (add_vec2 va half) normal))
                 (add_vec2 va half))]
    result
    ))
