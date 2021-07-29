(require '[clojure.set :refer [union]])

(def input 1364)
(def start-loc [1 1])
(def destination [31 39])
(def max-reps 50)

(defn calc-eq [x y]
  (+ (* x x) (* 3 x) (* 2 x y) y (* y y) input))

(defn valid-coord? [[x y]]
  (if (some #(< % 0) [x y])
      false
      (even? (count (filter #(= \1 %)
                            (Integer/toString (calc-eq x y) 2))))))

(defn possible-moves [[x y]]
  (set (filter valid-coord?
               [[(+ 1 x) y] [(- x 1) y] [x (+ y 1)] [x (- y 1)]])))

;; Part 1
(loop [reps 0
       coords #{start-loc}]
  (if (contains? coords destination)
      reps
      (recur (+ 1 reps)
             (reduce #(union %1 (possible-moves %2))
                     #{}
                     coords))))

;; Part 2
(loop [reps 0
       coords #{start-loc}]
  (if (= max-reps reps)
      (count coords)
      (recur (+ 1 reps)
             (reduce #(union %1 (possible-moves %2))
                     coords
                     coords))))
