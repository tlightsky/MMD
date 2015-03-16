(ns myclojure.mmd02
  (:use [incanter core stats charts io]))

; he, she, his, and hers

; 1 3 2
; 4 3
; 3

(defn med
  "Minimum Edit Distance"
  [seq1 seq2]
  (cond
   (empty? seq1) (count seq2)
   (empty? seq2) (count seq1)
   :else
   (min
     (+
      (if (= (first seq1) (first seq2)) 0 2) ;; if dp find equal 0 else 2
      (#'med (rest seq1) (rest seq2)))
     (inc (#'med (rest seq1) seq2))
     (inc (#'med seq1 (rest seq2))))))

(def med (memoize med))

(med "avada kedavra" "abracadabra")
(def all ["he" "she" "his" "hers"])

(for [s1 all]
  (for [s2 all]
    (med s1 s2)))


(def q4 ["ABRACADABRA" "BRICABRAC"])

(defn shingle
  [doc n]
  (loop [sets (hash-set) d doc]
      (if (< (count d) n)
        sets
        (recur (conj sets (subs d 0 n)) (subs d 1)))))

(def q4a (first q4))
(def q4as (shingle q4a 2))
q4as
(count q4as)

(def q4b (second q4))
(def q4bs (shingle q4b 2))
q4bs
(count q4bs)

(clojure.set/intersection  q4as q4bs)




; q2
; C1C2C3C4
; R5R6R4R3
; 5 2 1 4


(def p00 [0 0])
(def p10040 [100 40])

(defn L2
  [p0 p1]
  (Math/sqrt
   (+ (Math/pow (- (first p0) (first p1)) 2)
      (Math/pow (- (second p0) (second p1)) 2))))

(defn L1
  [p0 p1]
  (+ (Math/abs (- (first p0) (first p1)))
      (Math/abs (- (second p0) (second p1)))))

(defn judge [f p]
  (if (< (f p p00) (f p p10040))
    :p00
    :p100))

(judge L1 [58 13])
(judge L2 [58 13])
(judge L1 [50 18])
(judge L2 [50 18])
(judge L1 [53 10])
(judge L2 [53 10])
(judge L1 [53 15])
(judge L2 [53 15])


; q3
(def q3 [
[1	2	1	1	2	5	4]
[2	3	4	2	3	2	2]
[3	1	2	3	1	3	2]
[4	1	3	1	2	4	4]
[5	2	5	1	1	5	1]
[6	1	6	4	1	1	4]
])

(defn h [row col]
  (map #(nth % col) [(nth q3 row) (nth q3 (inc row))]))

(h 4 0)

(defn q3c []
  (for [b (range 0 5 2)]
    (for [c1 (range 0 7)]
      (for [c2 (range (inc c1) 7)]
        (when (= (h b c1) (h b c2))
          [c1 c2])))))
(def not-nil? (complement nil?))
(def mi (partial mapcat identity))
(def q3r (into #{} (filter identity (mi (mi (q3c))))))
(map (fn [[x y]] [(inc x) (inc y)] ) q3r)

(mapcat identity [[[0 1] [1 2]] [[11 12]]])



; 2b q2
(defn w2bq2 []
  (for [j (range 1 101)]
    (for [i (range 1 101)]
      (when (zero? (rem j i)) i))))

(def w2bq2s (w2bq2))
(def w2bq2sf (map #(filter identity %) w2bq2s))
(map (fn [x] (println x)) w2bq2sf)



; 2b q1
(def w2bq1d [{"N" 40000 "M"  60000000  "S"  3200000000 }
    {"N" 10000 "M" 40000000 "S" 200000000 }
    {"N" 30000 "M" 100000000 "S" 500000000 }
    {"N" 50000 "M" 40000000 "S" 800000000 }])

;; (def w2bq1d 1)

(defn triangular [N]
  (* (+ 1000000 (/ (Math/pow N 2) 2) ) 4))

(defn triple [M]
  (* (+ 1000000 (* 3 M)) 4))

(triangular ((second w2bq1d) "N"))
(triple ((second w2bq1d) "M"))

(defn w2bq1s [t]
  (let [S (t "S")
        trian (triangular (t "N"))
        trip (triple (t "M"))
        best (min trian trip)]
    [best
    (and (> best (* S 0.9)) (< best (* S 1.1)))
    ]))

(defn w2bq1 []
  (for [t w2bq1d]
    (str t (w2bq1s t))))
(w2bq1)


(def w2cq1d [{:S 0.2e9 :P 0.4e9}
             {:S 0.5e9 :P 3.2e9}
             {:S 0.2e9 :P 0.85e9}
             {:S 0.5e9 :P 10e9}])

(defn w2cq1s [t]
  (let [S (/ (:S t) 4)
        P (:P t)
        M 1e6
        pt0 (/ 1 32 3 M)
        pt1 (* pt0 31 S S)
        pt2 (- pt1 S)]
    [(and (> pt2 (* P 0.9)) (< pt2 (* P 1.1))) (/ pt2 P) pt2 pt1 pt0 ]))

(for [t w2cq1d]
  (str (:S t) " " (:P t) (w2cq1s t)))

