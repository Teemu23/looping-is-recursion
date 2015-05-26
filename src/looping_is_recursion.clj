(ns looping-is-recursion)

(defn power [base exp]
   (let [helper (fn [acc e] (if (zero? e)
                               acc
                               (recur (* acc base)(dec e))))]
     (helper 1 exp)))



(defn last-element [a-seq]
  (let [helper (fn [coll] (if (empty? coll)
                            nil
                            (if (= 1 (count coll))
                              (first coll)
                              (recur (rest coll)))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [a b] (if (and (empty? a) (empty? b))
                           true
                           (if (= (first a) (first b))
                             (recur (rest a) (rest b))
                             false)))]
        (if (not= (count seq1) (count seq2))
                  false
                  (helper seq1 seq2))))

(defn find-first-index [pred a-seq]
  (loop [elem a-seq
         c 0]
    (if (empty? elem)
      nil
      (if (pred (first elem))
        c
        (recur (rest elem) (inc c))))))



(defn avg [a-seq]
  (loop [c1 0
         c2 0
         elem a-seq]
    (if (empty? elem)
      (/ c1 c2)
      (recur (+ c1 (first elem)) (inc c2)(rest elem)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [a (frequencies a-seq)
         b []]
    (if (empty? a)
      b
      (if (odd? (get (first a) 1))
        (recur (rest a)(toggle b (get (first a) 0)))
        (recur (rest a) b)))))


(defn fast-fibo [n]
  (loop [k 2
         f_n 1
         f_n1 0]
    (cond
      (zero? n)
        0
      (= 1 n)
        1
      (= k n)
        (+ f_n f_n1)
      :else
        (recur (inc k) (+ f_n f_n1) f_n))))

(defn cut-at-repetition [a-seq]
  (loop [res []
         a a-seq]
    (cond
      (empty? a)
        res
      (some #(= (first a) %) res)
        res
      :else
        (recur (conj res (first a)) (rest a)))))

