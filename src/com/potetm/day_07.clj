(ns com.potetm.day-07
  (:require [clojure.string :as str]))

(defn parse [s]
  (map (fn [l]
         (let [[_ node wgt _mrk dep-str]
               (re-find #"^([a-z]+)\W+\(([0-9]+)\)(\W+->\W(.*))?"
                        l)]
           {:name node
            :weight (Long/parseLong wgt)
            :deps (when dep-str
                    (into []
                          (str/split dep-str
                                     #",\W+")))}))
       (str/split-lines s)))

(defn root [nds]
  (let [is-dep? (into #{}
                      (mapcat :deps
                              nds))]
    (first (filter (comp (complement is-dep?)
                         :name)
                   nds))))

(defn tree
  ([nds]
   (tree (into {}
               (map (juxt :name identity))
               nds)
         (root nds)))
  ([n->node {:keys [deps] :as nd}]
   (if (seq deps)
     (into [nd]
           (mapv (fn [n-name]
                   (tree n->node (n->node n-name)))
                 deps))
     nd)))

(def node first)
(def children rest)
(def has-children? vector?)

(defn total-weight [n]
  (if (has-children? n)
    (apply +
           (:weight (node n))
           (map total-weight
                (children n)))
    (:weight n)))

(defn mark-weights [n]
  (if (has-children? n)
    (let [p (node n)
          c (children n)]
      (into [(assoc p
               :total-weight (total-weight n))]
            (map mark-weights
                 c)))
    (assoc n
      :total-weight (:weight n))))

(defn balanced? [n]
  (if (has-children? n)
    (apply = (map total-weight
                  (children n)))
    false))

(defn problem-child [n]
  (when (has-children? n)
    (let [f (frequencies (map total-weight
                              (children n)))]
      (when (not= 1 (count f))
        (let [[[w _]
               [cw _]] (sort-by val
                                f)]
          {:problem-child (first (filter (comp #{w} total-weight)
                                         (children n)))
           :correct-weight cw})))))

(defn problem-node
  ([r]
   (let [output (fn [n cw]
                  (let [tw (total-weight n)
                        w (:weight (or (node n)
                                       n))]
                    {:problem-node n
                     :correct-weight (+ w (- cw tw))
                     :correct-total-weight cw}))]
     (loop [n r
            prev-correct-weight nil]
       (let [{:keys [problem-child
                     correct-weight] :as pc} (problem-child n)]
         (if pc
           (recur problem-child correct-weight)
           (output n prev-correct-weight)))))))

(def t
  (mark-weights (tree (parse "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"))))
