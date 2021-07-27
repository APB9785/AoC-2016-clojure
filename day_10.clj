(require '[clojure.string :as string])

(def pattern
  #"(\w+) (\d+) (?:gives low to |goes to )(\w+ \d+) ?(?:and high to )?(\w+ \d+)?")

(def input
  (->> (slurp "day_10_input.txt")
       (string/split-lines)
       (map #(re-find pattern %))
       (map rest)))

(defn give-chip [state [chip recipient]]
  (if (contains? state recipient)
      (update state
              recipient
              (fn [bot-state]
                (update bot-state :has #(conj % (Integer. chip)))))
      (assoc state recipient {:has [(Integer. chip)]})))

(defn setup-bot [state [b id low high]]
  (let [bot (string/join [b " " id])]  
    (if (contains? state bot)
        (update state
                bot
                #(assoc (assoc % :gives-low low) :gives-high high))
        (assoc state
               bot
               {:has [], :gives-low low, :gives-high high}))))

(defn init-state [lines]
  (reduce #(if (= (first %2) "value")
               (give-chip %1 (take 2 (drop 1 %2)))
               (setup-bot %1 %2))
          {}
          lines))

(defn has-61-and-17? [[_ bot]]
  (or (= (bot :has) [61 17])
      (= (bot :has) [17 61])))

(defn parse-ready-bots [acc bot]
  (let [[lo hi] (sort (bot :has))]
    (->> acc
         (cons [(bot :gives-high) hi])
         (cons [(bot :gives-low) lo]))))

(defn bot-full? [[obj-name obj-map]]
  (and (string/starts-with? obj-name "bot")
       (= 2 (count (obj-map :has)))))

(defn empty-bot [state bot-name]
  (update state bot-name #(assoc % :has [])))

(defn run [state]
  (reduce (fn [acc obj]
            (if (bot-full? obj)
                (let [[bot-name bot-map] obj
                      [lo hi] (sort (bot-map :has))]
                  (-> acc
                      (empty-bot bot-name)
                      (give-chip [lo (bot-map :gives-low)])
                      (give-chip [hi (bot-map :gives-high)])))
                acc))
          state
          state))

(defn find-61-17-comparator [state]
  (loop [state state]
    (let [done (filter has-61-and-17? state)]
      (if (< 0 (count done))
          (first (first done))
          (recur (run state))))))

(defn has-outputs? [state]
  (and (contains? state "output 0")
       (contains? state "output 1")
       (contains? state "output 2")))

(defn multiply-outputs [state]
  (* (first (get (state "output 0") :has))
     (first (get (state "output 1") :has))
     (first (get (state "output 2") :has))))

(defn get-outputs [state]
  (loop [state state]
    (if (has-outputs? state)
        (multiply-outputs state)
        (recur (run state)))))

;; Part 1
(-> input (init-state) (find-61-17-comparator))

;; Part 2
(-> input (init-state) (get-outputs))
