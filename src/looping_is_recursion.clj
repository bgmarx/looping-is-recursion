(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                     acc
                     (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (cond (empty? a-seq) nil
        (empty? (rest a-seq)) (first a-seq)
        :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (or  (empty? seq1) (empty? seq2)) false
        (= (first seq1) (first seq2))
          (recur (rest seq1) (rest seq2))
        :else false))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         i a-seq]
    (cond (empty? i) nil
          (pred (first i)) acc
          :else (recur (inc acc) (rest i)))))

(defn avg [a-seq]
  (loop [acc 0
         i-seq a-seq]
    (if (empty? i-seq)
        (/ acc (count a-seq))
      (recur (+ acc (first i-seq)) (rest i-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [seq a-seq acc #{}]
    (if (empty? seq)
      acc
      (recur (rest seq) (toggle acc (first seq))))))

(defn fast-fibo [n]
  (loop [a 0 b 1 k 0]
    (if (= n k)
      a
      (recur b ( + a b) (inc k)))))

(defn cut-at-repetition [a-seq]
  (loop [v '[]
         s #{}
         i-seq a-seq]
    (cond (empty? i-seq) v
          (contains? s (first i-seq)) v
     :else (recur
            (conj v (first i-seq))
            (conj s (first i-seq))
            (rest i-seq)))))

          

