(require '[clojure.string :as str])

(def input
     (-> (slurp "day_1_input.txt")
         (str/trim-newline)
         (str/split #", ")))

(defn turn [current direction]
  (case current
        \N (if (= direction \L) \W \E)
        \E (if (= direction \L) \N \S)
        \S (if (= direction \L) \E \W)
        \W (if (= direction \L) \S \N)))

(defn move [state]
  (case (state :dir)
        \N (update state :y inc)
        \E (update state :x inc)
        \S (update state :y dec)
        \W (update state :x dec)))

(defn get-coords [state] [(state :x) (state :y)])

(defn found-hq? [state]
  (and
   (contains? (state :seen) (get-coords state))
   (= (state :hq) nil)))

(defn add-seen [state]
  (if (found-hq? state)
      (assoc state :hq (get-coords state))
      (update state :seen conj (get-coords state))))

(defn move-all [state count]
  (reduce (fn [acc _] (-> acc move add-seen))
          state
          (range count)))

(defn strs-to-int [strs] (-> strs str/join Integer.))

(defn run-all-commands [input]
  (reduce (fn [acc x]
            (let [[direction & distance] x]
              (move-all
               (update acc :dir turn direction)
               (strs-to-int distance))))
          {:dir \N, :x 0, :y 0, :seen #{}, :hq nil}
          input))

(defn manhattan-dist [coord]
  (let [[x y] coord]
    (+ (Math/abs ^Integer x) (Math/abs ^Integer y))))

;;; Part 1
(-> input run-all-commands get-coords manhattan-dist)

;;; Part 2
(-> input run-all-commands (get :hq) manhattan-dist)
