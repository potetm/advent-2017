(ns com.potetm.day-14
  (:require [com.potetm.day-10 :as d10]))

(defn inputs [k]
  (map #(str k "-" %)
       (range 128)))

(defn knot-hash->bits [kh]
  (vec (mapcat (fn [i]
                 (mapv (partial bit-test i)
                       (reverse (range 8))))
               kh)))

(defn adjacents [pos]
  (into []
        (comp (map (fn [mv]
                     (mapv +
                           mv
                           pos)))
              (filter (fn [[x y]]
                        (and (<= 0 x 127)
                             (<= 0 y 127)))))
        [[1 0]
         [-1 0]
         [0 1]
         [0 -1]]))

(defn mark-group [b pos gid]
  (if (true? (get-in b pos))
    (reduce (fn [b' p]
              (mark-group b' p gid))
            (assoc-in b pos gid)
            (adjacents pos))
    ;; empty or already marked
    b))

(defn pp [bits]
  (doseq [r bits]
    (println (apply str
                    (map (fn [v]
                           (format "%1$5s"
                                   (get {false "."
                                         true "#"}
                                        v
                                        v)))
                         r)))))

(defn p1 [in]
  (apply +
         (mapcat (fn [i]
                   (map #(Long/bitCount %)
                        (d10/knot-hash i)))
                 (inputs in))))

(defn p2 [in]
  (let [b (mapv (comp knot-hash->bits
                      d10/knot-hash)
                (inputs in))]
    (second (reduce (fn [[b gid] pos]
                      (let [b' (mark-group b pos gid)]
                        (if (= b' b)
                          [b gid]
                          [b' (inc gid)])))
                    [b 0]
                    (for [i (range 128)
                          j (range 128)]
                      (vector i j))))))

(comment
  (def k "flqrgnkx")
  (def b (mapv (comp knot-hash->bits
                     d10/knot-hash)
               (inputs k)))

  (mark-group b [0 3] 7)
  (def foo (d10/knot-hash "abc"))
  (apply str (knot-hash->bits foo))
  (apply str (knot-hash->bits* foo))
  )
