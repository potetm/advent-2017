(ns com.potetm.day-5
  (:require [clojure.string :as str]))

(defn parse [s]
  {:offset 0
   :instrs (mapv #(Long/parseLong %)
                 (str/split-lines s))})

(defn n [{:keys [offset
                 instrs]}]
  (when-let [v (get instrs offset)]
    {:offset (+ offset v)
     :instrs (assoc instrs
               offset (inc v))}))

(defn p1 [s]
  (dec (count (take-while some?
                          (iterate n
                                   (parse s))))))

(defn n2 [{:keys [offset
                  instrs]}]
  (when-let [v (get instrs offset)]
    {:offset (+ offset v)
     :instrs (assoc instrs
               offset (if (<= 3 v)
                        (dec v)
                        (inc v)))}))

(defn p2 [s]
  (dec (count (take-while some?
                          (iterate n2
                                   (parse s))))))
