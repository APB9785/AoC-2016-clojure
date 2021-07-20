(require '[clojure.string :as str])

(defn parse-line [str]
  (let [[_ name sector checksum] (re-matches #"([a-z-]+)-(\d+)\[([a-z]+)]" str)]
    {:name name, :sector (Integer. sector), :checksum checksum}))

(def input
  (let [raw (-> (slurp "day_4_input.txt") (str/split #"\n"))]
    (map parse-line raw)))

(defn create-checksum [name]
  (->> name
       (frequencies)
       (filter #(not= \- (first %)))
       (sort-by first)
       (sort-by last >)
       (take 5)
       (map first)
       (str/join)))

(defn valid? [room]
  (= (room :checksum)
     (create-checksum (room :name))))

(defn sum-valid-sectors [rooms]
  (reduce
   #(if (valid? %2) (+ %1 (get %2 :sector)) %1)
   0
   rooms))

(defn shift-char [c n]
  (let [shifted (+ (mod n 26) (int c))]
    (char
     (if (> shifted 122)
         (- shifted 26)
         shifted))))

(defn shift-all [name offset]
  (map
   #(shift-char % offset)
   name))

(defn decode [room]
  (update
   room
   :name
   #(str/join (shift-all (str/replace % #"-" "") (room :sector)))))

(defn find-northpole [rooms]
  (first
   (filter
    #(some? (re-find #"northpole" (get % :name)))
    rooms)))

;;; Part 1
(sum-valid-sectors input)

;;; Part 2
(-> (map decode input) (find-northpole) (get :sector))
