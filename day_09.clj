(require '[clojure.string :as string])

(def input
  (string/replace (slurp "day_9_input.txt") #"\s" ""))

(defn split-cmd [text]
  (string/split (string/join (rest text)) #"\)" 2))

(defn decompress-v1 [text]
  (loop [todo text
         cnt 0]
    (case (first todo)
          nil cnt
          \( (let [[cmd new-todo] (split-cmd todo)
                   [len reps] (string/split cmd #"x" 2)]
               (recur (drop (Integer. len) new-todo)
                      (+ cnt (* (Integer. len) (Integer. reps)))))
          (recur (rest todo)
                 (+ 1 cnt)))))

(defn decompress-v2 [text]
  (loop [todo text
         cnt 0]
    (case (first todo)
          nil cnt
          \( (let [[cmd new-todo] (split-cmd todo)
                   [len reps] (string/split cmd #"x" 2)
                   [inner outer] (split-at (Integer. len) new-todo)]
               (recur outer
                      (+ cnt (* (decompress-v2 inner) (Integer. reps)))))
          (recur (rest todo)
                 (+ 1 cnt)))))

;; Part 1
(decompress-v1 input)

;; Part 2
(decompress-v2 input)
