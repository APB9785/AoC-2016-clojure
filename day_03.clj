(use '[clojure.string :only (split)])

(def input
  (let [raw (-> (slurp "day_3_input.txt") (split #"\D+"))]
    (map #(Integer/parseInt %) (filter #(not= % "") raw))))

(defn check-valid [state a b c]
  (let [[x y z] (sort [a b c])
        new-state (assoc state :hold [])]
    (if (< z (+ x y))
        (update new-state :count + 1)
        new-state)))

(defn count-triangles [ints]
  (reduce (fn [acc x]
            (let [hold (acc :hold)]
              (if (= 2 (count hold))
                  (check-valid acc x (get hold 0) (get hold 1))
                  (update acc :hold conj x))))
          {:count 0, :hold []}
          ints))

(defn transpose [state]
  (let [[a b c d e f g h i] (state :hold)]
    (-> state
        (update :done concat [a d g b e h c f i])
        (assoc :hold []))))

(defn reorder-list [ints]
  (get
   (reduce #(let [state (update %1 :hold conj %2)]
             (if (= 9 (count (state :hold))) (transpose state) state))
           {:done [], :hold []}
           ints)
   :done))

;;; Part 1
(-> input count-triangles (get :count))

;;; Part 2
(-> input reorder-list count-triangles (get :count))
