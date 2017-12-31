(ns com.potetm.day-15)

(def a-fact 16807)
(def b-fact 48271)
(def a-crit-div 4)
(def b-crit-div 8)
(def divisor 2147483647)

(defn gen1 [factor prev]
  (rem (* factor prev)
       divisor))

(defn gen2 [factor crit-div prev]
  {:pre [(and (pos? crit-div)
              (zero? (bit-and crit-div
                              (dec crit-div))))]}
  (let [mask (dec crit-div)]
    (loop [g (gen1 factor prev)]
      (if (zero? (bit-and g mask))
        g
        (recur (gen1 factor g))))))

(defn judge [a b]
  (== (bit-shift-left a
                      -16)
      (bit-shift-left b
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
