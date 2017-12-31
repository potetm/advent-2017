(ns com.potetm.day-09
  (:require [clojure.string :as str]))

(defn parse [s]
  ;; NEVER do this in real life
  (seq s))

(defn remove-garbage [s]
  (let [drp-gbg (fn [s]
                  (loop [s s]
                    (if (seq s)
                      (case (first s)
                        \! (recur (drop 2 s))
                        \> (next s)
                        (recur (next s)))
                      s)))]
    (loop [s s
           out []]
      (if (seq s)
        (let [c (first s)]
          (if (= c \<)
            (recur (drp-gbg s)
                   out)
            (recur (next s)
                   (conj out c))))
        out))))

(defn count-garbage [s]
  (let [cnt-gbg (fn [s]
                  (loop [s s
                         c 0]
                    (if (seq s)
                      (case (first s)
                        \! (recur (drop 2 s)
                                  c)
                        \> [(next s) c]
                        (recur (next s)
                               (inc c)))
                      s)))]
    (loop [s s
           cnt 0]
      (if (seq s)
        (let [c (first s)]
          (if (= c \<)
            (let [[s cg] (cnt-gbg (next s))]
              (recur s
                     (+ cg cnt)))
            (recur (next s)
                   cnt)))
        cnt))))

(defn countem [s]
  (loop [mult 0
         s s
         c 0]
    (if (seq s)
      (case (first s)
        \{ (recur (inc mult)
                  (next s)
                  c)
        \} (recur (dec mult)
                  (next s)
                  (+ c mult))
        \, (recur mult
                  (next s)
                  c))
      c)))

(countem (remove-garbage (parse "{{<a!>},{<a!>},{<a!>},{<ab>}}")))
