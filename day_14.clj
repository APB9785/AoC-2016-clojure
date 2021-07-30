(require '[clojure.string :as string])
(import 'java.security.MessageDigest
        'java.math.BigInteger)

(def salt "ahsbgdzn")

(defn md5 [^String s]
  (->> s
       .getBytes
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))

(defn md5-stretch [idx reps]
  (loop [n 0
         s (string/join [salt idx])]
    (if (= reps n)
        s
        (recur (+ 1 n) (md5 s)))))

(def p1-seq
  (map #(md5-stretch % 1) (range)))

(def p2-seq
  (map #(md5-stretch % 2017) (range)))

(defn find-triplet [idx all-hashes]
  (let [curr (first (drop idx all-hashes))
        res (re-find #"(.)\1{2}" curr)]
    (first (first res))))

(defn has-fiver? [target s]
  (string/includes? s (string/join (repeat 5 target))))

(defn find-5s [target hashes]
  (some #(has-fiver? target %) hashes))

(defn valid? [idx all-hashes]
  (let [triplet (find-triplet idx all-hashes)
        next-1000-hashes (take 1000 (drop (+ 1 idx) all-hashes))
        fiver (find-5s triplet next-1000-hashes)]
    (and triplet fiver)))

;; Note: both parts take quite a long time to run

;; Part 1
(->> (range)
     (filter #(valid? % p1-seq))
     (drop 63)
     (first))

;; Part 2
(->> (range)
     (filter #(valid? % p2-seq))
     (drop 63)
     (first))

