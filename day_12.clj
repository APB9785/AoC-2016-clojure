(require '[clojure.string :as string])

(def pattern #"(\w+) (\w+) ?([\w-]+)?")

(def input
  (->> (slurp "day_12_input.txt")
       (string/split-lines)
       (map #(rest (re-find pattern %)))
       (vec)))

(defn get-value [key-or-value state]
  (if (contains? #{"a" "b" "c" "d"} key-or-value)
      (get state key-or-value)
      (Integer. key-or-value)))

(defn shift-idx [commands idx state]
  (let [[command p1 p2] (commands idx)]
    (if (= command "jnz")
        (if (= 0 (get-value p1 state))
            (+ 1 idx)
            (+ (Integer. p2) idx))
        (+ 1 idx))))

(defn update-state [commands idx state]
  (let [[command p1 p2] (commands idx)]
    (case command
          "jnz" state
          "inc" (update state p1 #(+ % 1))
          "dec" (update state p1 #(- % 1))
          "cpy" (assoc state p2 (get-value p1 state)))))

(defn run-commands [commands idx state]
  (loop [idx idx
         state state]
    (if (contains? commands idx)
        (recur (shift-idx commands idx state)
               (update-state commands idx state))
        (get state "a"))))

(run-commands input 0 {"a" 0, "b" 0, "c" 0, "d" 0})

(run-commands input 0 {"a" 0, "b" 0, "c" 1, "d" 0})
