(require '[clojure.string :as string]
         '[clojure.set :refer (union intersection)])
(import 'java.security.MessageDigest
        'java.math.BigInteger)

(def salt "gdjjyniy")

(def init-states #{{:coord [0 0], :path ""}})

(defn md5 [^String s]
  (->> s
       .getBytes
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))

(defn valid? [c]
  (contains? #{\b \c \d \e \f} c))

(defn valid-doors [u d l r]
  [(if (valid? u) \U nil)
   (if (valid? d) \D nil)
   (if (valid? l) \L nil)
   (if (valid? r) \R nil)])

(defn possibilities [coord]
  (cond
    (= coord [0 0]) [\R \D]
    (= coord [3 0]) [\L \D]
    (= coord [0 3]) [\U \R]
    (or (= coord [1 0]) (= coord [2 0])) [\L \R \D]
    (or (= coord [0 1]) (= coord [0 2])) [\U \R \D]
    (or (= coord [3 1]) (= coord [3 2])) [\U \L \D]
    (or (= coord [1 3]) (= coord [2 3])) [\U \L \R]
    :else [\U \D \L \R]))

(defn open-doors [coord path]
  (let [h (md5 (string/join [salt path]))
        [u d l r] (subs h 0 4)
        by-hash (filter some? (valid-doors u d l r))
        by-coord (possibilities coord)]
    (intersection (set by-hash) (set by-coord))))

(defn move [direction state]
  (let [[x y] (state :coord)]
    (case direction
          \U (-> state
                 (assoc :coord [x (- y 1)])
                 (update :path #(string/join [% \U])))
          \D (-> state
                 (assoc :coord [x (+ y 1)])
                 (update :path #(string/join [% \D])))
          \L (-> state
                 (assoc :coord [(- x 1) y])
                 (update :path #(string/join [% \L])))
          \R (-> state
                 (assoc :coord [(+ x 1) y])
                 (update :path #(string/join [% \R]))))))

(defn next-rooms [state]
  (set (map #(move % state)
            (open-doors (state :coord) (state :path)))))

;; Part 1
(loop [states init-states]
  (let [done? (some #(when (= [3 3] (get % :coord)) %) states)]
    (if done?
        (get done? :path)
        (recur (reduce #(union %1 (next-rooms %2))
                       #{}
                       states)))))

;; Part 2
(loop [states init-states
       max-len 0]
  (if (= 0 (count states))
      max-len
      (let [{done true
             todo false} (group-by #(= [3 3] (get % :coord)) states)
            lengths (map #(count (get % :path)) done)]
        (recur (reduce #(union %1 (next-rooms %2))
                       #{}
                       todo)
               (apply max (conj lengths max-len))))))
