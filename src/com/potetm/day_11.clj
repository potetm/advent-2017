(ns com.potetm.day-11
  (:require [clojure.string :as str]))

(defn parse [s]
  (str/split s #","))

(def origin [0 0 0])

(defn step [pos d]
  (let [∆ (case d
            "n"  [ 0  1 -1]
            "ne" [ 1  0 -1]
            "se" [ 1 -1  0]
            "s"  [ 0 -1  1]
            "sw" [-1  0  1]
            "nw" [-1  1  0])]
    (mapv +
          pos
          ∆)))

(defn taxicab-distance [a b]
  (/ (apply + (map (fn [& coords]
                     (Math/abs ^long (apply - coords)))
                   a
                   b))
     2))

(defn p1 [in]
  (taxicab-distance origin
                    (reduce step
                            origin
                            (parse in))))

(defn p2 [in]
  (apply max
         (map (partial taxicab-distance origin)
              (reductions step
                          origin
                          (parse in)))))

(p1 "ne,ne,ne")
(p1 "ne,ne,sw,sw")
(p1 "ne,ne,s,s")
(p1 "se,sw,se,sw,sw")
