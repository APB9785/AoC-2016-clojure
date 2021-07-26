(require '[clojure.string :as string]
         '[clojure.set :refer [union]])

(def x-bound 50)
(def y-bound 6)

(def line-pattern
  #"(rect|rotate) (\d+)?x?(\d+)?(column|row)?(?: x=| y=)?(\d+)?(?: by )?(\d+)")

(def parsed-input
  (->> (string/split (slurp "day_8_input.txt") #"\n")
       (map #(re-find line-pattern %))
       (map #(drop 1 %))
       (map #(filter some? %))))

(defn cartesian-product [width height]
  (for [x (range 0 (Integer. width))
        y (range 0 (Integer. height))]
    (vector x y)))

(defn run-rect [state width height]
  (union state (set (cartesian-product width height))))

(defn shift-row [[x y] n] [(mod (+ x n) x-bound) y])

(defn shift-col [[x y] n] [x (mod (+ y n) y-bound)])

(defn run-rotate [state axis index offset]
  (let [idx (Integer. index)
        off (Integer. offset)]
    (case axis
          "row" (set (map #(if (= idx (last %)) (shift-row % off) %)
                          state))
          "column" (set (map #(if (= idx (first %)) (shift-col % off) %)
                             state)))))

(defn run-command [state line]
  (case (first line)
        "rect" (run-rect state (nth line 1) (nth line 2))
        "rotate" (run-rotate state (nth line 1) (nth line 2) (nth line 3))))

(def end-result
  (reduce run-command #{} parsed-input))

(defn check-pixel [state coord]
  (if (contains? state coord) "#" " "))

(defn print-line [state y]
  (->> (range 0 x-bound)
       (reduce #(cons (check-pixel state [%2 y]) %1) [])
       (reverse)
       (string/join)))

;; Part 1
(count end-result)

;; Part 2
(map #(print-line end-result %) (range 0 6))
