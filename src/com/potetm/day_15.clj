(ns com.potetm.day-15)

(def ^:const a-fact 16807)
(def ^:const b-fact 48271)
(def ^:const a-crit-div 4)
(def ^:const b-crit-div 8)
(def ^:const divisor 2147483647)

(defn gen1 [factor prev]
  (rem (* (long factor)
          (long prev))
       (long divisor)))

(defn gen2 [factor crit-div prev]
  {:pre [(and (pos? (long crit-div))
              (zero? (bit-and (long crit-div)
                              (dec (long crit-div)))))]}
  (let [mask (dec (long crit-div))]
    (loop [g (gen1 factor prev)]
      (if (zero? (bit-and (long g)
                          mask))
        g
        (recur (gen1 factor g))))))

(defn judge [a b]
  (== (bit-shift-left (long a)
                      -16)
      (bit-shift-left (long b)
                      -16)))

(defn p1 [a b]
  (loop [i 0
         c 0
         a a
         b b]
    (if (< i 40000000)
      (recur (inc i)
             (if (judge a b)
               (inc c)
               c)
             (gen1 a-fact a)
             (gen1 b-fact b))
      c)))

(defn p2 [a b]
  (loop [i 0
         c 0
         a a
         b b]
    (if (< i 5000000)
      (recur (inc i)
             (if (judge a b)
               (inc c)
               c)
             (gen2 a-fact
                   a-crit-div
                   a)
             (gen2 b-fact
                   b-crit-div
                   b))
      c)))

(comment

  #_(def a 634)
  #_(def b 301)

  (p1 65 8921)
  (bit-shift-left 245556042 48)
  (bit-shift-left 1431495498 48))
