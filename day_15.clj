(def pattern
  #"Disc #(\d) has (\d+) positions; at time=0, it is at position (\d+)\.")

(def input
  (->> (slurp "day_15_input.txt")
       (re-seq pattern) 
       (map (fn [res]
              (map #(Integer. %)
                   (rest res))))
       (reduce #(let [[disc total start] %2]
                  (assoc %1 disc [total, start]))
               {})))

(def input-2
  (assoc input 7 [11 0]))

(defn disc-ok? [disc idx discs]
  (if (nil? (get discs disc))  ; This bypasses the check for disc 7
      true                     ; during part 1, when it doesn't exist
      (let [[total start] (get discs disc)]
        (= 0 (mod (+ start disc idx) total)))))

(defn all-discs-ok? [idx discs]
  (and (disc-ok? 1 idx discs)
       (disc-ok? 2 idx discs)
       (disc-ok? 3 idx discs)
       (disc-ok? 4 idx discs)
       (disc-ok? 5 idx discs)
       (disc-ok? 6 idx discs)
       (disc-ok? 7 idx discs)))

;; Part 1
(loop [i 0]
  (if (all-discs-ok? i input)
      i
      (recur (+ 1 i))))

;; Part 2
(loop [i 0]
  (if (all-discs-ok? i input-2)
    i
    (recur (+ 1 i))))


