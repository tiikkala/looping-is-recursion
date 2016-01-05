(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
        (last a-seq))

(defn seq= [a-seq b-seq]
  (let [a (seq a-seq)
        b (seq b-seq)]
    (= a b)))

(defn find-first-index [pred a-seq]
  (loop [i 0
         b-seq a-seq]
    (cond
     (empty? b-seq) nil
     (pred (first b-seq)) i
     :else (recur (inc i) (rest b-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         i 0
         b-seq a-seq]
    (if (empty? b-seq)
      (/ sum i)
      (recur (+ sum (first b-seq)) (inc i) (rest b-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
        (loop [b-seq a-seq
               a-set #{}]
          (if (empty? b-seq) a-set
          (recur (rest b-seq) (toggle a-set (first b-seq))))))

(defn fast-fibo [n]
        (loop [f2 0
               f1 0
               fn 1
               k 0]
          (cond
           (= n 0) 0
           (= n 1) 1
           (= k n) f1
           :else (recur f1 fn (+ fn f1) (inc k)))))

(defn cut-at-repetition [a-seq]
  (loop [outp []
         sq a-seq]
    (cond
      (empty? sq)
        outp
      (some #(= (first sq) %) outp)
        outp
      :else
        (recur (conj outp (first sq)) (rest sq)))))
