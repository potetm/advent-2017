(ns com.potetm.day-02
  (:require [clojure.string :as str]))

(defn parse [s]
  (map (fn [l]
         (map #(Long/parseLong %)
              (str/split l #"\W+")))
       (str/split-lines s)))

(defn p1 [ls]
  (apply + (map (fn [l]
                  (- (apply max l)
                     (apply min l)))
                ls)))

(defn p2 [ls]
  (apply +
         (map (fn [l]
                (first (for [x l
                             y l
                             :let [v (cond
                                       (= x y) nil
                                       (zero? (mod x y)) (/ x y)
                                       (zero? (mod y x)) (/ y x))]
                             :when v]
                         v)))
              ls)))
