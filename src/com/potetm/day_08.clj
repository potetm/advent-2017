(ns com.potetm.day-08
  (:require [clojure.string :as str]))

(defn parse [s]
  (into []
        (map (fn [l]
               {:post [(every? some?
                               (vals %))]}
               (let [[_ r op c cond-r cond-comp cond-v]
                     (re-find #"^([a-z]+) (inc|dec) (-?[0-9]+) if ([a-z]+) (<|>|<=|>=|==|!=) (-?[0-9]+)$"
                              l)]
                 {:reg r
                  :op (get {"inc" (fnil + 0)
                            "dec" (fnil - 0)}
                           op)
                  :cnt (Long/parseLong c)
                  :cond-reg cond-r
                  :cond-compare (get {"<" <
                                      ">" >
                                      "<=" <=
                                      ">=" >=
                                      "==" ==
                                      "!=" not=}
                                     cond-comp)
                  :cond-val (Long/parseLong cond-v)})))
        (str/split-lines s)))

(defn reg [s r]
  (get-in s [:regs r] 0))

(defn state []
  {:regs {}})

(defn conditional [s {:keys [cond-compare
                             cond-reg
                             cond-val]}]
  (cond-compare (reg s cond-reg)
                cond-val))

(defn op [s {:keys [reg
                    op
                    cnt]}]
  (update-in s
             [:regs reg]
             op
             cnt))

(defn process-instr [s instr]
  (if (conditional s instr)
    (op s instr)
    s))

(comment
  (def ex "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10")

  (reduce process-instr
          (state)
          (parse ex))

  (apply max (vals
               (:regs (reduce process-instr
                              (state)
                              (parse ex)))))

  (apply max (mapcat (comp vals :regs)
                     (reductions process-instr
                                 (state)
                                 (parse ex)))))




