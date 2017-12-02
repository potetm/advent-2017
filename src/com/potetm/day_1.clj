(ns com.potetm.day-1
  (:require [clojure.string :as str]))

(defn parse [s]
  (map #(Long/parseLong %)
       (str/split s #"")))

(defn p1 [s]
  (let [pairs (partition 2
                         1
                         (take (inc (count s))
                               (cycle s)))]
    (transduce (comp (filter (partial apply =))
                     (map first))
               +
               0
               pairs)))

(defn p2 [s]
  (reduce (fn [s [i j]]
            (+ s i j))
          0
          (filter (partial apply =) ;; find matching tuples
                  ;; split in half and zip
                  (apply map
                         vector
                         (split-at (/ (count s)
                                      2)
                                   s)))))

;;; Alternative (i.e. better) based on https://gist.github.com/cmack/38eeecff0d6771d730e671e4a4b4fe51
(defn rotate
  "Rotate a seq s to the right by amount n"
  [n s]
  (let [c (count s)]
    (take c
          (drop (mod n c)
                (cycle s)))))

(defn solve [s n]
  (apply +
         (map (fn [x y]
                (if (= x y)
                  x
                  0))
              s
              (rotate n
                      s))))

(defn p1* [s]
  (solve s 1))

(defn p2* [s]
  (solve s (/ (count s)
              2)))
