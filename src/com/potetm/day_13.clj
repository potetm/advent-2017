(ns com.potetm.day-13
  (:require [clojure.string :as str]))

(defn parse [in]
  (let [idx (into {}
                  (map (fn [l]
                         (let [[_ d r] (re-find #"([0-9]+): ([0-9]+)"
                                                l)]
                           [(Long/parseLong d)
                            (Long/parseLong r)]))
                       (str/split-lines in)))]
    {:caught-at []
     :player-layer -1
     :time 0
     :layers (mapv (fn [i]
                     (get idx i))
                   (range (inc (apply max
                                      (keys idx)))))}))

(defn step [{:keys [caught-at
                    player-layer
                    layers
                    time] :as s}]
  (let [pl' (inc player-layer)
        caught? (when-some [r (get layers pl')]
                  (zero? (mod time
                              (* 2 (dec r)))))]
    (when (find layers pl')
      (merge s
             {:time (inc time)
              :caught-at (if caught?
                           (conj caught-at pl')
                           caught-at)
              :player-layer pl'}))))

(defn severity [{:keys [caught-at
                        layers]}]
  (apply + (map (fn [d]
                  (* d (get-in layers [d :range])))
                caught-at)))

(defn game-states [s]
  (take-while some?
              (iterate step
                       s)))

(defn p1 [in]
  (severity (last (game-states (parse in)))))

(defn p2 [in]
  (let [gs (parse in)]
    (ffirst (filter (fn [[_wait gss]]
                      (every? (comp empty? :caught-at)
                              gss))
                    (map (fn [i]
                           [i (game-states (assoc gs :time i))])
                         (range))))))

(comment
  (def g (parse "0: 3\n1: 2\n4: 4\n6: 4"))
  (game-states g)
  (p2 "0: 3\n1: 2\n4: 4\n6: 4"))
