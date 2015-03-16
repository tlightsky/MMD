(ns myclojure.mmd05
  (:use [incanter core stats charts io]))


(def w5b1 [[28 145] [65 140] [50 130] [25 125] [38 115] [55 118]
            [44 105] [29 97]  [50 90]  [63 88]  [43 83]  [35 63]
            [55 63]  [50 60]  [42 57]  [23 40]  [64 37]  [50 30]
            [33 22]  [55 20]])


(def w5b1c '((25,125), (44,105), (29,97), (35,63),
            (55,63), (42,57), (23,40), (64,37),
            (33,22), (55,20)))

(def clu
  (loop [cs w5b1c
         clu {}]
    (if-let [c (first cs)]
      (recur (rest cs) (assoc clu c #{c}))
      clu)))

(defn L2
  [p0 p1]
  (Math/sqrt
   (+ (Math/pow (- (first p0) (first p1)) 2)
      (Math/pow (- (second p0) (second p1)) 2))))

(defn clust [c p]
  (let [[k v] (first (sort-by (fn [[k v]] (L2 p k)) c))]
    (assoc c k (conj (c k) p))))

(defn average [coll f]
  (float (/ (reduce + (map f coll)) (count coll))))

(defn recentroid [c]
  (into {} (map (fn [[k v]]
                  [[(average v first) (average v second)] v]) c)))

(loop [pts w5b1
       clu clu]
  (if-let [p (first pts)]
    (recur (rest pts) (clust clu p))
    (->> clu recentroid keys (sort-by #(first %)))))

;; (def rg (range 150))
;; (def plot (xy-plot rg rg))
;; (for [pts w5b1]
;;   (add-pointer plot (first pts) (second pts) :text (str pts)))
;; (view plot)



(def w5b2 {:Y [5 10] :B [20 5]})

(def w5b2a {:A {:Y [[6 15] [13 7]] :B [[16 19] [25 12]]}
            :B {:Y [[7 8 ] [12 5]] :B [[15 14] [20 10]]}
            :C {:Y [[6 7 ] [11 4]] :B [[14 10] [23 6 ]]}
            :D {:Y [[6 7 ] [11 4]] :B [[11 5 ] [17 2 ]]}})

(defn len [x ax ix]
  (cond (> x ax) (Math/pow (- x ax) 2)
        (< x ix) (Math/pow (- x ix) 2)
        :else 0))

(defn dist [p r]
  (println (len (first p) (-> r second first) (-> r ffirst)))
  (println (len (second p) (-> r first second) (-> r second second)))
  (+ (len (first p) (-> r second first) (-> r ffirst))
     (len (second p) (-> r first second) (-> r second second))))

(defn is-right? [mp]
  (and (> (dist (:Y w5b2) (:Y mp)) (dist (:B w5b2) (:Y mp)))
       (< (dist (:B ) ()) ())))

(for [[k v] w5b2a]
  [k (is-right? v)])


(defn poly [mp]
  (let [ul (first mp)
        lr (second mp)]
    [ul  [(first lr) (second ul)] lr [(first ul) (second lr)]  ]))
(poly (-> w5b2a :A :Y))
(defn plt [mp]
  (let [rg (range 35)
        plot (xy-plot rg rg)]
    (loop [ks [:Y :B]]
      (when-let [k (first ks)]
        (add-pointer plot (-> w5b2 k first) (-> w5b2 k second))
        (add-polygon plot (poly (k mp)))
        (recur (rest ks))))
    (view plot)
  ))
(plt (:D w5b2a))


(def w5a1 [[1 0 0] [0 2 0] [0 0 0]])
(matrix w5a1)
;; (decomp-svd (matrix w5a1) :type :values)
(let [{:keys [S U V]} (decomp-svd (matrix w5a1))]
  (mmult U (matrix [[2 0 0] [0 1 0] [0 0 0]]) (trans V)))

(def svd [[1 1 1 0 0]
[3 3 3 0 0]
[4 4 4 0 0]
[5 5 5 0 0]
[0 0 0 4 4]
[0 0 0 5 5]
[0 0 0 2 2]])

(decomp-svd (matrix svd))

(def w5a2 {:bids [0.1 0.09 0.08 0.07 0.06]
           :ctrs [[0.015 0.016 0.017 0.018 0.019]
                  [0.01 0.012 0.014 0.015 0.016]
                  [0.005 0.006 0.007 0.008 0.01]]
           :budget [1 2 3 4 5]})
(loop [budget (:budget w5a2)
       click 0]
  (doseq [x (range 3)]
    ))



(def w5a3 '((0,0)
 (10,10)
 (1,6)
 (3,7)
 (4,3)
 (7,7)
 (8,2)
 (9,5)))

(def centroids [(first w5a3) (second w5a3)])

