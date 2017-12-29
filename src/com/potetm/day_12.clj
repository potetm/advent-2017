(ns com.potetm.day-12
  (:require [clojure.string :as str]))

(defn parse [s]
  (into {}
        (map (fn [l]
               (let [[_ p cs] (re-find #"([0-9]+) <-> ([0-9, ]+)$"
                                       l)]
                 [(Long/parseLong p)
                  (map #(Long/parseLong %)
                       (str/split cs
                                  #", "))]))
             (str/split-lines s))))

(defn group
  ([ps root]
   (group #{} ps root))
  ([seen ps node]
   (let [seen' (conj seen node)
         rs (get ps node)
         todo (remove seen' rs)]
     (if (seq todo)
       (into seen'
             (mapcat (partial group seen' ps))
             todo)
       seen'))))

(defn p1 [in]
  (count (group (parse in)
                0)))

(defn p2 [in]
  (let [ps (parse in)]
    (count (distinct (map (fn [[n]]
                            (group ps n))
                          ps)))))
