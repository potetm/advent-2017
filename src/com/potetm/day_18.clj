(ns com.potetm.day-18
  (:require [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

(defmethod print-method clojure.lang.PersistentQueue [q, w]
  (print-method (into [] q) w))

(defn parse [in]
  (let [reg-or-num (fn [x]
                     (when x
                       (try
                         (Long/parseLong x)
                         (catch NumberFormatException nfe
                           x))))]
    (mapv (fn [l]
            (let [[_ op x _ y]
                  (re-find #"(snd|set|add|mul|mod|rcv|jgz) ([a-z0-9])( ([-a-z0-9]+))?"
                           l)]
              {:op op
               :x (reg-or-num x)
               :y (reg-or-num y)}))
          (str/split-lines in))))

(defn machine []
  {:regs {}
   :idx 0
   :last-played nil
   :rcv? false})

(defn read-reg [{:keys [regs]} x]
  (if (number? x)
    x
    (get regs x 0)))

(defmulti step (fn [m {:keys [op]}]
                 op))

(defmethod step "snd"
  [{:keys [idx] :as m}
   {:keys [x]}]
  (assoc m
    :last-played (read-reg m x)
    :idx (inc idx)))

(defmethod step "set"
  [{:keys [regs idx] :as m}
   {:keys [x y]}]
  (assoc m
    :regs (assoc regs
            x (read-reg m y))
    :idx (inc idx)))

(defmethod step "add"
  [{:keys [regs idx] :as m}
   {:keys [x y]}]
  (assoc m
    :regs (assoc regs
            x (+ (read-reg m x)
                 (read-reg m y)))
    :idx (inc idx)))

(defmethod step "mul"
  [{:keys [regs idx] :as m}
   {:keys [x y]}]
  (assoc m
    :regs (assoc regs
            x (* (read-reg m x)
                 (read-reg m y)))
    :idx (inc idx)))

(defmethod step "mod"
  [{:keys [regs idx] :as m}
   {:keys [x y]}]
  (assoc m
    :regs (assoc regs
            x (mod (read-reg m x)
                   (read-reg m y)))
    :idx (inc idx)))

(defmethod step "rcv"
  [{:keys [idx] :as m}
   {:keys [x]}]
  (assoc m
    :rcv? (not (zero? (read-reg m x)))
    :idx (inc idx)))

(defmethod step "jgz"
  [{:keys [idx] :as m}
   {:keys [x y]}]
  (assoc m
    :idx (+ idx
            (if (pos? (read-reg m x))
              (read-reg m y)
              1))))

(defn p1 [in]
  (let [instrs (parse in)]
    (loop [{:keys [idx] :as m} (machine)]
      (let [{:keys [rcv?] :as m'}
            (step m
                  (get instrs
                       idx))]
        (if rcv?
          m'
          (recur m'))))))

(defn machine* []
  [{:regs {"p" 0}
    :idx 0
    :q (PersistentQueue/EMPTY)
    :send-cnt 0
    :blocked? false}
   {:regs {"p" 1}
    :idx 0
    :q (PersistentQueue/EMPTY)
    :send-cnt 0
    :blocked? false}])

(defmulti step* (fn [m pid {:keys [op]}]
                  op))

(defmethod step* "snd"
  [m pid {:keys [x]}]
  (-> m
      (update pid
              (fn [{:keys [idx send-cnt] :as pm}]
                (assoc pm
                  :idx (inc idx)
                  :send-cnt (inc send-cnt))))
      (update-in [(mod (inc pid)
                       2)]
                 (fn [{:keys [q] :as pm}]
                   (assoc pm
                     :q (conj q (read-reg (get m pid)
                                          x))
                     :blocked? false)))))

(defmethod step* "rcv"
  [m pid {:keys [x]}]
  (let [v (peek (get-in m [pid :q]))]
    (update m
            pid
            (fn [{:keys [idx q regs] :as pm}]
              (assoc pm
                :idx (if v
                       (inc idx)
                       idx)
                :q (pop q)
                :regs (if v
                        (assoc regs x v)
                        regs)
                :blocked? (not v))))))

(defmethod step* :default
  [m pid inst]
  (assoc m
    pid (step (get m pid)
              inst)))

(defn run-till-blocked [m pid instrs]
  (loop [{{:keys [idx]} pid :as m} m]
    (let [{{:keys [blocked?]} pid :as m'}
          (step* m
                 pid
                 (get instrs idx))]
      (if blocked?
        m'
        (recur m')))))

(defn run-till-deadlock [m instrs]
  (loop [{{blk0? :blocked?} 0
          {blk1? :blocked?} 1 :as m} m
         i 0]
    (cond
      (and blk0? blk1?) m
      blk0? (recur (run-till-blocked m 1 instrs)
                   (inc i))
      blk1? (recur (run-till-blocked m 0 instrs)
                   (inc i))
      :else ;; just pick one
      (recur (run-till-blocked m 0 instrs)
             (inc i)))))

(comment
  (def instrs (parse "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"))
  (def instrs* (parse "snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d"))

  (run-till-blocked (machine*)
                    0
                    instrs*)

  (run-till-deadlock (machine*)
                     instrs*))
