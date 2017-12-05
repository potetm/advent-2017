(ns com.potetm.day-4
  (:require [clojure.string :as str]))

(defn parse [s]
  (str/split-lines s))

(defn valid? [s]
  (every? (fn [[w c]]
            (= 1 c))
          (frequencies (str/split s
                                  #"\W+"))))

(defn p1 [s]
  (count (filter valid?
                 (parse s))))

(defn valid-2? [s]
  (every? (fn [[_ c]]
            (= 1 c))
          (frequencies (map (comp sort
                                  vec)
                            (str/split s
                                       #"\W+")))))

(defn p2 [s]
  (count (filter valid-2?
                 (parse s))))
