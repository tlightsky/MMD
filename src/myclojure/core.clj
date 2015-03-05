(ns myclojure.core
  (:use [incanter core stats charts io]))

;(def M (matrix [[1/2 1/2 0] [1/2 0 0] [0 1/2 1]]))
;(def r (matrix [1/3 1/3 1/3]))
(def T (matrix 1/3 3 3))

(defn fir
  ([M r beta]
  (fir M r beta 20))

  ([M r beta times]
  (loop [rr r t times]
    (if (pos? t)
      (do (println (str (- times t) rr))
          (recur (mmult (plus (mult beta M) (mult (- 1 beta) T)) rr) (- t 1)))
      rr))))

(defn -main
  "I don't do a whole lot."
  [& args]
  (fir))

(let [M (matrix [[1/2 1/2 0] [1/2 0 0] [0 1/2 1]])
      r (matrix [1/3 1/3 1/3])
      beta 0.8]
  (fir M r beta))
(let [M (matrix [[0 0 0] [1/2 0 0] [1/2 1 1]])
      r (matrix [1/3 1/3 1/3])
      beta 0.7]
  (fir M r beta))
(let [M (matrix [[0 0 1] [1/2 0 0] [1/2 1 0]])
      r (matrix [1/3 1/3 1/3])
      beta 0.85]
  (fir M r beta))


(let [[a b c] [0.38778971170152604 0.21481062747314855 0.3973996608253248]]
  (+ c (* 0.15 b))
  (* 0.85 a)
  (+ (* 0.575 a) (* 0.15 c))
  (= (+ (* 0.9 b) (* 0.475 a))
  (* 0.95 c))
  )


; Q3
(let [M (matrix [[0 0 1] [1/2 0 0] [1/2 1 0]])
      r (matrix [1 1 1])
      beta 1]
  (fir M r beta 5))



