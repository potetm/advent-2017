(ns com.potetm.day-03
  (:require [clojure.string :as str]))

(defn pb [b]
  (let [xs    (keys b)
        min-x (apply min xs)
        max-x (apply max xs)
        ys    (distinct (mapcat keys
                                (vals b)))
        min-y (apply min ys)
        max-y (apply max ys)]
    (map (fn [y]
           (map (fn [x]
                  (get-in b [x y]))
                (range min-x
                       (inc max-x))))
         (range max-y
                (dec min-y)
                -1))))

(defn prb [b]
  (let [max-n (apply max
                     (mapcat vals
                             (vals b)))
        width (int (inc (Math/log10 max-n)))]
    (doseq [r (pb b)]
      (println (str/join " "
                         (map (fn [v]
                                (if (nil? v)
                                  (apply str (repeat width
                                                     " "))
                                  (let [w (int (inc (Math/log10 v)))]
                                    (if (< w width)
                                      (apply str v (repeat (- width w)
                                                           " "))
                                      (str v)))))
                              r))))))

(defn gs [size]
  (let [turn {:d :r
              :r :u
              :u :l
              :l :d}
        ops  {:d [0 -1]
              :r [1 0]
              :u [0 1]
              :l [-1 0]}
        mv   (fn [p dir]
               (map +
                    p
                    (get ops dir)))]
    (reduce (fn [{:keys [point
                         facing
                         board]}
                 n]
              (let [try-f  (turn facing)
                    try-p  (mv point try-f)
                    taken? (get-in board try-p)]
                (if taken?
                  (let [p' (mv point facing)]
                    {:board (assoc-in board p' n)
                     :facing facing
                     :point p'})
                  {:board (assoc-in board try-p n)
                   :facing try-f
                   :point try-p})))
            {:point [0 0]
             :facing :d
             :board {0 {0 1}}}
            (range 2
                   (inc size)))))

(defn taxicab-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs ^long (- x1 x2))
     (Math/abs ^long (- y1 y2))))

(defn p1 [n]
  (taxicab-distance [0 0]
                    (:point (gs n))))

(defn gs2 []
  (let [turn          {:d :r
                       :r :u
                       :u :l
                       :l :d}
        ops           {:d [0 -1]
                       :dr [1 -1]
                       :r [1 0]
                       :ur [1 1]
                       :u [0 1]
                       :ul [-1 1]
                       :l [-1 0]
                       :dl [-1 -1]}
        mv            (fn [p dir]
                        (map +
                             p
                             (get ops dir)))
        sum-neighbors (fn [b p]
                        (apply + (map (fn [[dir]]
                                        (get-in b (mv p dir) 0))
                                      ops)))]
    (iterate (fn [{:keys [point
                          facing
                          board]}]
               (let [try-f  (turn facing)
                     try-p  (mv point try-f)
                     taken? (get-in board try-p)]
                 (if taken?
                   (let [p' (mv point facing)]
                     {:board (assoc-in board p' (sum-neighbors board
                                                               p'))
                      :facing facing
                      :point p'})
                   {:board (assoc-in board try-p (sum-neighbors board
                                                                try-p))
                    :facing try-f
                    :point try-p})))
             {:point [0 0]
              :facing :d
              :board {0 {0 1}}})))

(defn p2 [n]
  (let [{:keys [point
                board]} (first (filter (fn [{:keys [point board]}]
                                         (< n (get-in board point)))
                                       (gs2)))]
    (get-in board point)))
