(ns com.potetm.day-17)

(defn spin [step end]
  (reduce (fn [{b :buff
                i :idx}
               v]
            (let [c (count b)
                  i' (inc (mod (+ i step)
                               c))]
              {:buff (vec (concat (subvec b
                                          0
                                          i')
                                  [v]
                                  (subvec b
                                          i')))
               :idx i'}))
          {:buff [0 1]
           :idx 1}
          (range 2
                 end)))

(defn find-second-item [step end]
  (loop [v 1
         i 1
         sec 1]
    (if (< v (long end))
      (let [v' (inc v)
            i' (inc (long (mod (+ i
                                  (long step))
                               v)))]
        (recur v'
               i'
               (if (= 1 i')
                 v
                 sec)))
      sec)))

(defn p1 [step]
  (let [{b :buff
         i :idx} (spin step
                       2018)]
    (get b (inc i))))

(defn p2 [step]
  (find-second-item step
                    50000000))