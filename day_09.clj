(require '[clojure.string :as string])

(def input
  (string/replace (slurp "day_9_input.txt") #"\s" ""))

(defn run [todo]
  (let [[cmd new-todo] (string/split (string/join (rest todo)) #"\)" 2)
        [len reps] (string/split cmd #"x" 2)]
    [(drop (Integer. len) new-todo) (* (Integer. len) (Integer. reps))]))

(defn decompress-v1 [text]
  (loop [todo text
         cnt 0]
    (case (first todo)
          nil cnt
          \( (let [[new-todo n] (run todo)] (recur new-todo (+ n cnt)))
          (recur (rest todo) (+ 1 cnt)))))

;; Part 1
(decompress-v1 input)
