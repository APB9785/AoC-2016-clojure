(require '[clojure.string :as string])

(def input (slurp "day_2_input.txt"))

(defn format-result [done] (-> (map str done) (string/join)))

;;; Part 1 Functions

(defn move-if-possible [current bad-vals func steps]
  (if (contains? bad-vals current) current (func current steps)))

(defn move [state direction]
  (case direction
        \U (update state :current move-if-possible #{1 2 3} - 3)
        \D (update state :current move-if-possible #{7 8 9} + 3)
        \L (update state :current move-if-possible #{1 4 7} - 1)
        \R (update state :current move-if-possible #{3 6 9} + 1)
        \newline (update state :done conj (state :current))))

(defn process [commands]
  (reduce (fn [state command] (move state command))
          {:current 5, :done []}
          commands))

;;; Part 2 Functions

(defn up [state]
  (let [n (state :current)]
    (cond
      (contains? #{1 2 4 5 9} n) state
      (contains? #{6 7 8} n) (update state :current - 4)
      (= n 3) (assoc state :current 1)
      (= n \A) (assoc state :current 6)
      (= n \B) (assoc state :current 7)
      (= n \C) (assoc state :current 8)
      (= n \D) (assoc state :current \B))))

(defn down [state]
  (let [n (state :current)]
    (cond
      (contains? #{5 9 \A \C \D} n) state
      (contains? #{2 3 4} n) (update state :current + 4)
      (= n 1) (assoc state :current 3)
      (= n 6) (assoc state :current \A)
      (= n 7) (assoc state :current \B)
      (= n 8) (assoc state :current \C)
      (= n \B) (assoc state :current \D))))

(defn left [state]
  (let [n (state :current)]
    (cond
      (contains? #{1 2 5 \A \D} n) state
      (contains? #{3 4 6 7 8 9} n) (update state :current - 1)
      (= n \B) (assoc state :current \A)
      (= n \C) (assoc state :current \B))))

(defn right [state]
  (let [n (state :current)]
    (cond
      (contains? #{1 4 9 \C \D} n) state
      (contains? #{2 3 5 6 7 8} n) (update state :current + 1)
      (= n \A) (assoc state :current \B)
      (= n \B) (assoc state :current \C))))

(defn process-part-2 [commands]
  (reduce (fn [state command]
            (case command
                  \U (up state)
                  \D (down state)
                  \L (left state)
                  \R (right state)
                  \newline (update state :done conj (state :current))))
          {:current 5, :done []}
          commands))

;;; Part 1 result
(-> input process (get :done) format-result)

;;; Part 2 result
(-> input process-part-2 (get :done) format-result)
