(ns myclojure.rand
  (:use [incanter core stats charts io]))

(def ri (repeatedly #(char (rand-int 255))))
(def payload (into-array Character/TYPE (take (* 1024 1000) ri)))
(def ofile (java.io.FileWriter. "somefile.bin"))
(.write ofile payload)
(.close ofile)
