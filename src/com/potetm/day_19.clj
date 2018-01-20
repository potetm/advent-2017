(ns com.potetm.day-19
  (:require [clojure.string :as str]))

(defn parse [in]
  (str/split-lines in))

(defn start [b]
  {:pos [0 (.indexOf ^String (get b 0)
                     (int \|))]
   :dir :down
   :board b
   :chars []})

(def a-z (into #{}
               (map char
                    (range 65 91))))

(defn char-at [b p]
  (get-in b p))

(defn mv [d p]
  (mapv +
        p
        (case d
          :up [-1 0]
          :down [1 0]
          :left [0 -1]
          :right [0 1])))

(def dir->char {:up \|
                :down \|
                :left \-
                :right \-})

(defn next-char [b d p]
  (loop [p' (mv d p)
         t 1]
    (let [c (char-at b p')]
      (cond
        (and (= t 1)
             (not= c \space))
        [p' c]

        (and (= t 2)
             (or (contains? a-z c)
                 (= (dir->char d)
                    c)))
        [p' c]

        (= t 2) nil
        :else (recur (mv d p')
                     (inc t))))))

(defn step [{p :pos
             d :dir
             b :board :as state}]
  (let [turns (fn [dir]
                (case dir
                  :up [:up :left :right]
                  :down [:down :left :right]
                  :left [:left :up :down]
                  :right [:right :up :down]))
        [p' c'] (next-char b d p)]
    (cond
      (contains? #{\| \-} c') (assoc state :pos p')
      (contains? a-z c') (-> state
                             (update :chars conj c')
                             (assoc :pos p'))
      (= \+ c') (let [d' (first (filter (fn [d']
                                          (let [[_ c''] (next-char b d' p')]
                                            (or (= (dir->char d')
                                                   c'')
                                                (contains? a-z c''))))
                                        (turns d)))]
                  (assoc state
                    :pos p'
                    :dir d')))))

(defn p1 [in]
  (apply str (:chars (last (take-while some?
                                       (iterate step
                                                (start (parse in))))))))

(defn p2 [in]
  (count (take-while some?
                     (iterate step
                              (start (parse in))))))

(comment
  (def s (start (parse "     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ \n")))
  (next-pos (:board s)
            (:pos s)
            (:dir s))

  (step s)
  )
