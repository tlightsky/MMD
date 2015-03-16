(ns myclojure.FSS
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
;;             [digest :refer [md5]]
            )
;;   (:use [incanter core stats charts io])
  )


(def size 9397022)
(defn log [msg id]
  (when (= 0 (rem id 1000))
    (println msg " " id " " (/ id size 1.0))))

(def sentences (atom []))

(def fname "test.txt")
;(def fname "sentences.txt")
;(def fname "s.txt")
(def fname "s1k.txt")
;(def fname "s10k.txt")
;(def fname "s100k.txt")

(with-open [rdr (jio/reader fname)]
  (doseq [line (line-seq rdr)]
    (let [[_ id s] (re-find #"^(\d+) (.+)$" line)]
      (log "reading " (read-string id))
      (swap! sentences conj s)
;;       (swap! sentences assoc id s)
      )))
;; (.parseInt Integer "1")
;; (def s (slurp "test.txt"))

(def buckets (atom {}))

(defn store-bucket [h i]
;;   (log "storing" i (nth @sentences i))
  (swap! buckets
         (fn [bkt]
           (if-let [bucket (bkt h)]
            (assoc bkt h (conj bucket i))
            (assoc bkt h #{i})))))

(defn word-dst-1 [s]
  (let [words (str/split s #" ")]
    (for [i (-> words count range)]
      (str/join " "
        (concat (subvec words 0 i)
              (subvec words (inc i) (count words)))))))
;; (word-dst-1 (first @sentences))

(defn med
  "Minimum Edit Distance"
  [seq1 seq2]
  (cond
   (empty? seq1) (count seq2)
   (empty? seq2) (count seq1)
   :else
   (min
     (+
      (if (= (first seq1) (first seq2)) 0 1) ;; if dp find equal 0 else 1,for change is also dst 1 in this case
      (#'med (rest seq1) (rest seq2)))
     (inc (#'med (rest seq1) seq2))
     (inc (#'med seq1 (rest seq2))))))

(def med (memoize med))

(defn word-dst-1? [a b]
;;   (med (str/split a #" ") (str/split b #" "))
  (= 1 (med (str/split a #" ") (str/split b #" "))))

;; (word-dst-1? (first @sentences) (second @sentences))
;; (word-dst-1? (first @sentences) (last @sentences))


(reset! buckets {})
(loop [s @sentences
       i 0]
  (log "storing " i)
  (when-let [ss (first s)]
    (store-bucket (hash ss) i)
    (doseq [sss (word-dst-1 ss)]
      (do
;;         (log "" (str i sss))
        (store-bucket (hash sss) i)))
    (recur (rest s) (inc i))))

;; (def all-count (atom 0))
(def rec
  (for [bucket (vals @buckets)]
    (for [x bucket y bucket :when (> x y)]
      (when (word-dst-1? (nth @sentences x)
             (nth @sentences y))
	     (println "find! " (nth @sentences x) (nth @sentences y))
         1))))

(def all-count (reduce + (filter identity (flatten rec))))
;(println "buckets" @buckets)
(println "all count" all-count)
