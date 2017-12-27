(ns com.potetm.day-10
  (:require [clojure.string :as str]))

(defn p1-parse [s]
  (map #(Long/parseLong %)
       (str/split s
                  #",\W*")))

(defn p2-parse [s]
  (apply concat
         (repeat 64
                 (concat (when-not (= "" s)
                           (map #(Character/codePointAt ^String %
                                                        0)
                                (str/split (str/trim s)
                                           #"")))
                         (p1-parse "17, 31, 73, 47, 23")))))

(defn state [s]
  {:hash (range s)
   :idx 0
   :skip 0})

(defn step [{:keys [hash
                    idx
                    skip]}
            l]
  (let [c (count hash)
        s' (take c
                 (drop idx
                       (cycle hash)))]
    {:hash (take c
                 (drop (- c idx)
                       (cycle (concat (reverse (take l s'))
                                      (drop l s')))))
     :idx (mod (+ idx l skip)
               (count hash))
     :skip (inc skip)}))

(defn sparse->dense [hash]
  (map (partial apply bit-xor)
       (partition 16
                  hash)))

(defn dense->str [hash]
  (apply str (map (partial format "%02x")
                  hash)))

(defn p1 [s]
  (apply * (take 2
                 (:hash (reduce step
                                (state 256)
                                (p1-parse s))))))

(defn p2 [s]
  (dense->str (sparse->dense (:hash (reduce step
                                            (state 256)
                                            (p2-parse s))))))

(comment (reductions step
                     (state 5)
                     (p1-parse "3, 4, 1, 5")))




