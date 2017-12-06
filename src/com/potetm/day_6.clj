(ns com.potetm.day-6
  (:require [clojure.string :as str])
  (:import (clojure.lang APersistentVector)))

(defn parse [s]
  (mapv #(Long/parseLong %)
        (str/split s #"\W+")))

(defn rotate
  "Rotate a seq s to the right by amount n"
  [n s]
  (let [c (count s)]
    (take c
          (drop (mod n c)
                (cycle s)))))

(defn redistribute [^APersistentVector mbs]
  (let [m    (apply max mbs)
        i    (.indexOf mbs m)
        ;; seq of the indexes that need to be inc'd
        idxs (take m
                   (rest (cycle (rotate i
                                        (range 0
                                               (count mbs))))))]
    (reduce (fn [mbs idx]
              (update mbs idx inc))
            (assoc mbs i 0)
            idxs)))

(defn p1 [s]
  (reduce (fn [seen? [i mbs]]
            (if (seen? mbs)
              (reduced i)
              (conj seen? mbs)))
          #{}
          (map-indexed vector
                       (iterate redistribute
                                (parse s)))))

(defn p2 [s]
  (reduce (fn [seen-idx [i mbs]]
            (if-let [si (seen-idx mbs)]
              (reduced (- i si))
              (assoc seen-idx mbs i)))
          {}
          (map-indexed vector
                       (iterate redistribute
                                (parse s)))))
