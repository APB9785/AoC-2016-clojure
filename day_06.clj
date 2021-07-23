(require '[clojure.string :as string])

(def input
  (-> (slurp "day_6_input.txt")
      (string/split #"\n")))

(defn parse-line [[a b c d e f g h]]
  {0 a, 1 b, 2 c, 3 d, 4 e, 5 f, 6 g, 7 h})

(defn group-columns [lines]
  (reduce #(merge-with cons (parse-line %2) %1)
          {0 [], 1 [], 2 [], 3 [], 4 [], 5 [], 6 [], 7 []}
          lines))

(def totals
  (map (fn [[_ elems]] (frequencies elems))
       (group-columns input)))

;; Part 1
(->> totals
     (map #(key (apply max-key val %)))
     (string/join))

;; Part 2
(->> totals
     (map #(key (apply min-key val %)))
     (string/join))
