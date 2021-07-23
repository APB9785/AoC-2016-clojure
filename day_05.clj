(require '[clojure.string :as string])
(import 'java.security.MessageDigest
        'java.math.BigInteger)

(def input "wtnhxymk")

(defn md5 [^String s]
  (->> s
       .getBytes
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))

(defn append-index [n]
  (string/join [input n]))

(def interesting-hashes
  (->> (range)
       (map append-index)
       (map md5)
       (filter #(string/starts-with? % "00000"))))

(def valid-indexes #{\0 \1 \2 \3 \4 \5 \6 \7})

(defn char-to-int [c]
  (Character/digit c 10))

(defn put-new [prev v]
  (if (nil? prev) v prev))

(defn try-put [state h]
  (let [idx (get h 5)
        v (get h 6)]
    (if (contains? valid-indexes idx)
        (update state (char-to-int idx) put-new v)
        state)))

;;; It will take a while to calculate the results so be patient!

;; Part 1
(->> interesting-hashes
     (take 8)
     (map #(get % 5))
     (string/join))

;; Part 2
(->> interesting-hashes
     (reduce #(if (= 8 (count %1)) (reduced %1) (try-put %1 %2)) {})
     (sort-by first)
     (map last)
     (string/join))
