(require '[clojure.string :as string])

(def input
  (string/trimr (slurp "day_18_input.txt")))

(def max-len
  (count input))

(def init-state
  (loop [n 0
         res #{}]
    (if (= n max-len)
        res
        (recur (+ 1 n)
               (if (= \^ (get input n))
                   (conj res n)
                   res)))))

(defn trapped? [n state]
  (let [left (contains? state (- n 1))
        right (contains? state (+ n 1))]
    (not= left right)))

(defn next-gen [state]
  (loop [n 0
         res #{}]
    (if (= n max-len)
        res
        (recur (+ 1 n)
               (if (trapped? n state)
                   (conj res n)
                   res)))))

(defn count-gens [state max-gen]
  (loop [gen 0
         current-state state
         safe-count 0]
    (if (= gen max-gen)
        safe-count
        (recur (+ gen 1)
               (next-gen current-state)
               (+ safe-count (- max-len (count current-state)))))))

(count-gens init-state 40)
(count-gens init-state 400000)
