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

