(ns myclojure.mmd03
  (:use [incanter core stats charts io datasets]))


(def w4aq1 (matrix [[1 2 3 4 5]
                [2 3 2 5 3]
                [5 5 5 3 2]]))

(defn average
  [numbers]
    (/ (apply + numbers) (count numbers)))

(defn normalize [A]
  (matrix
    (for [x A]
      (let [avg (average x)]
        (map #(- % avg) x)))))

(-> w4aq1 normalize trans normalize trans)


(def w4aq2 (matrix [[1 0 1 0 1 2]
                    [1 1 0 0 1 6]
                    [0 1 0 1 0 2]]))

(matrix [(first w4aq2) (second w4aq2)])

;; (def a (first w4aq2))
;; (def b (second w4aq2))

(defn mcos [a b]
  (/ (reduce + (map * a b))
     (* (vector-length a) (vector-length b))))

;; (vector-length (first w4aq2))
;; (reduce + (map * a b))
;; (mcos (first w4aq2) (second w4aq2))
;; (for [x w4aq2]
;;   x)

;; (matrix (concat (drop 1 a) [(last a)]))
;; (map (vector-length w4aq2))

(defn mapα [A α]
  (concat (drop 1 A) [(* (last A) α)]))

(for [α [0 0.5 1 2]]
  (for [x (range 3) y (range 3)]
    (when (not (= x y))
      (let [a (mapα (nth w4aq2 x) α)
            b (mapα (nth w4aq2 y) α)]
      [α x y
       (mcos a b)]))))




(def w4bq1 (matrix [(/ 2 7) (/ 3 7) (/ 6 7)]))

(def w4bq1a (matrix [[0.728, 0.485, -0.485]
                     [0.312, 0.156, -0.937]
                     [2.250, -0.500, -0.750]
                     [0.975, 0.700, -0.675]]))

(for [x w4bq1a]
  [(mmult x w4bq1)
   (vector-length x)]
  )


(def w4bq3 (matrix [[1 1] [2 2] [3 4]]))

(def MTM (mmult (trans w4bq3) w4bq3))
MTM
(def w4bq3v (:vectors (decomp-eigenvalue MTM)))

(mmult w4bq3 w4bq3v)
