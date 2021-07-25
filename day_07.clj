(require '[clojure.string :as string])

(def input
  (string/split (slurp "day_7_input.txt") #"\n"))

(defn inside [line]
  (->> line
       (re-seq #"\[(\w+)]")
       (map last)))

(defn outside [line]
  (string/split line #"\[\w+]"))

(defn all-3-subs [line]
  (concat (partition 3 line)
          (partition 3 (drop 1 line))
          (partition 3 (drop 2 line))))

(defn all-4-subs [line]
  (concat (partition 4 line)
          (partition 4 (drop 1 line))
          (partition 4 (drop 2 line))
          (partition 4 (drop 3 line))))

(defn abba? [[a b c d]]
  (and (= a d) (= b c) (not= a b)))

(defn aba? [[a b c]]
  (and (= a c) (not= a b)))

(defn bab [[a b _]]
  [b a b])

(defn complements [strs]
  (map bab strs))

(defn merge-4-subs [matches]
  (->> matches
       (map all-4-subs)
       (reduce concat)))

(defn merge-3-subs [matches]
  (->> matches
       (map all-3-subs)
       (reduce concat)))

(defn valid-tls? [line]
  (let [outs (merge-4-subs (outside line))
        ins (merge-4-subs (inside line))]
    (and (some abba? outs)
         (not (some abba? ins)))))

(defn valid-ssl? [line]
  (let [outs (merge-3-subs (outside line))
        ins (merge-3-subs (inside line))
        babs (set (complements (filter aba? outs)))]
    (some #(contains? babs %) ins)))

;; Part 1
(count (filter valid-tls? input))

(count (filter valid-ssl? input))
