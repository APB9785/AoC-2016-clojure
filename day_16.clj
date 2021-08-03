(require '[clojure.string :as string])

(def input
  (map #(if (= \1 %) true false)
       "10010000000110000"))

(defn flip-reverse [a]
  (reverse
   (map #(if % false true)
        a)))

(defn shift-join [a]
  (concat a (cons false (flip-reverse a))))

(defn fill-mem [s max-len]
  (loop [res s
         len (count s)]
    (if (>= len max-len)
        (take max-len res)
        (recur (shift-join res) (+ 1 (* 2 len))))))

(defn calc-chunk-size [total-len]
  (loop [n total-len
         i 1]
    (if (even? n)
        (recur (/ n 2) (* 2 i))
        i)))

(defn even-matches? [s]
  (even? (count (filter identity s))))

(defn checksum [len]
  (let [chunk-size (calc-chunk-size len)
        mem (fill-mem input len)
        chunks (partition chunk-size mem)]
    (->> chunks
         (map #(if (even-matches? %) \1 \0))
         (string/join))))

;; Part 1
(checksum 272)

;; Part 2 (may require increase to java max heap size)
(checksum 35651584)
