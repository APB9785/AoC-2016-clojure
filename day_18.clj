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
        center (contains? state n)
        right (contains? state (+ n 1))]
    (or (and left center (not right))
        (and center right (not left))
        (and left (not center) (not right))
        (and right (not center) (not left)))))

(defn next-gen [state]
  (loop [n 0
         res #{}]
    (if (= n max-len)
        res
        (recur (+ 1 n)
               (if (trapped? n state)
                   (conj res n)
                   res)))))

(defn count-40-gens [state]
  (loop [gen 0
         current-state state
         safe-count 0]
    (if (= gen 40)
        safe-count
        (recur (+ gen 1)
               (next-gen current-state)
               (+ safe-count (- max-len (count current-state)))))))

(count-40-gens init-state)
