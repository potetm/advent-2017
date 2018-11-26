(ns com.potetm.day-05
  (:require [clojure.string :as str]
            [criterium.core :as crit]))

(defn parse [s]
  {:offset 0
   :instrs (mapv #(Long/parseLong %)
                 (str/split-lines s))})

(defn next-state-1 [{:keys [offset
                            instrs]}]
  (when-let [v (get instrs offset)]
    {:offset (+ offset v)
     :instrs (assoc instrs
               offset (inc v))}))

(defn p1 [s]
  (dec (count (take-while some?
                          (iterate next-state-1
                                   (parse s))))))

(defn next-state-2 [{:keys [offset
                            instrs]}]
  (when-let [v (get instrs offset)]
    {:offset (+ offset v)
     :instrs (assoc instrs
               offset (if (<= 3 v)
                        (dec v)
                        (inc v)))}))

(defn p2 [s]
  (dec (count (take-while some?
                          (iterate next-state-2
                                   (parse s))))))

(defn p2-fast
  "p2 takes ~8-9s

  This takes ~80ms.

  <3 arrays"
  [s]
  (let [instrs (longs (into-array Long/TYPE
                                  (into []
                                        (map #(Long/parseLong %))
                                        (str/split-lines s))))
        len (alength instrs)]
    (loop [offset 0
           c 0]
      ;; interesting lesson here:
      ;; I originally had (< -1 offset len) here.
      ;; That caused the whole thing to take > 1s.
      ;; So the vararg arity of `<` was a real time sink.
      ;; Probably because it had to construct a whole other
      ;; seq on every cycle.
      (if (< offset len)
        (let [v (aget instrs offset)]
          (aset instrs
                offset
                (if (<= 3 v)
                  (dec v)
                  (inc v)))
          (recur (+ offset v)
                 (inc c)))
        c))))
