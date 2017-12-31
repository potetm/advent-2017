(ns com.potetm.day-16
  (:require [clojure.string :as str]))

(defn parse [in]
  (map (fn [s]
         (let [[_
                s sc
                x x1 x2
                p pa pb]
               (re-find #"(s)([0-9]{1,2})|(x)([0-9]{1,2})/([0-9]{1,2})|(p)([a-p])/([a-p])"
                        s)]
           (cond
             s {:instr s
                :cnt (Long/parseLong sc)}
             x {:instr x
                :idx1 (Long/parseLong x1)
                :idx2 (Long/parseLong x2)}
             p {:instr p
                :name1 pa
                :name2 pb})))
       (str/split in #",")))

(defmulti step (fn [g {:keys [:instr]}]
                 instr))

(defmethod step "s"
  [g {:keys [cnt]}]
  (let [c (count g)
        spl (- c cnt)]
    (into []
          (concat (subvec g
                          spl
                          c)
                  (subvec g
                          0
                          spl)))))

(defmethod step "x"
  [g {:keys [idx1 idx2]}]
  (let [v1 (get g idx1)
        v2 (get g idx2)]
    (-> g
        (assoc idx1 v2)
        (assoc idx2 v1))))

(defmethod step "p"
  [g {:keys [name1 name2]}]
  (let [idx1 (.indexOf g name1)
        idx2 (.indexOf g name2)
        v1 (get g idx1)
        v2 (get g idx2)]
    (-> g
        (assoc idx1 v2)
        (assoc idx2 v1))))

(defn game [n]
  (mapv (fn [i]
          (String. (char-array 1
                               [(char i)])))
        (range 97
               (+ n 97))))

(defn p1 [in]
  (apply str
         (reduce step
                 (game 16)
                 (parse in))))

(defn p2 [in]
  (let [instrs (parse in)
        init (game 16)
        cyc (cons init
                  (take-while (complement #{init})
                              (next (iterate (fn [g]
                                               (reduce step
                                                       g
                                                       instrs))
                                             init))))]
    (apply str
           (nth cyc
                (mod 1000000000
                     (count cyc))))))

(comment
  (def g (game 5))
  (game 16)
  (parse "s1,x3/4,pe/b")
  (reduce step
          g
          (parse "s1,x3/4,pe/b"))
  (p1 "s1,x3/4,pe/b")
  (p2 "s1,x3/4,pe/b"))
