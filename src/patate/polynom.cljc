(ns patate.polynom
  (:require [clojure.string :as str]
            [patate.util :refer [seq-indexed]]))


(defn poly->str
  "Returns a textual representation of the polynom."
  [poly]
  (let [result (->> poly
                    (keep-indexed (fn [degree factor]
                                    (when-not (zero? factor)
                                      (str (if (pos? factor)
                                             "+"
                                             "-")
                                           (when (or (not= (Math/abs factor) 1)
                                                     (zero? degree))
                                             (Math/abs factor))
                                           (case degree
                                             0 ""
                                             1 "x"
                                             (str "x^" degree))))))
                    reverse
                    str/join)]
    (cond
      (str/blank? result) "0"
      (str/starts-with? result "+") (subs result 1)
      :else result)))

#_(poly->str [1 0 3])
#_(poly->str [1 -1 1])
#_(poly->str [0 1 0 5])
#_(poly->str [])

(defn poly-normalize
  "Returns a polynom without the leading zero factors."
  [poly]
  (let [no-leading-zeroes (->> (rseq poly)
                               (drop-while zero?)
                               reverse
                               vec)]
    (if (seq no-leading-zeroes)
      no-leading-zeroes
      [0])))

#_ (poly-normalize [0 1 2 0 3 4 0 0 0])
#_ (poly-normalize [0 0 0 0])

(defn poly-degree? [poly]
  (->> (poly-normalize poly)
       seq-indexed
       last
       first))

#_ (poly-degree? [7 0 0])
#_ (poly-degree? [7 0 1 0 0])

(defn create-poly
  "Returns a random polynom with a specified degree."
  ([degree] (create-poly degree -3 3))
  ([degree min-factor max-factor]
   (->> (repeatedly #(-> (for [_ (range 0 (inc degree))]
                           (+ (rand-int (inc (- max-factor min-factor)))
                              min-factor))
                         vec))
        (filter (comp #{degree} poly-degree?))
        first)))

#_(poly->str (create-poly 2))
#_(create-poly 3 -5 5)

(defn poly-shift
  "Shift all factors higher by n degrees."
  [poly n]
  (vec (concat (repeat n 0) poly)))

#_(poly-shift [1 5 7] 5)
#_(poly-shift [1 5 7] 0)

(defn poly-scalar-mul
  "Multiply the polynom by a scalar."
  [poly n]
  (-> (mapv (fn [x] (* x n)) poly)
      poly-normalize))

#_(poly-scalar-mul [1 2 5] -10)

(defn extend-poly [poly size]
  (->> (concat poly (repeat 0))
       (take (max (count poly) size))
       vec))

#_(extend-poly [1 2 3] 2)

(defn poly-poly-add
  "Returns the addition of 2 polynoms."
  ([] [0])
  ([poly] poly)
  ([poly1 poly2]
   (let [max-count (max (count poly1) (count poly2))]
     (-> (mapv +
               (extend-poly poly1 max-count)
               (extend-poly poly2 max-count))
         poly-normalize)))
  ([poly1 poly2 & more]
   (reduce poly-poly-add poly1 (cons poly2 more))))

#_(poly-poly-add [1 2 3]
                 [100 200 300]
                 [10 20 30 40 50])

(defn poly-poly-sub
  "Returns the subtraction of 2 polynoms."
  ([poly] poly)
  ([poly1 poly2]
   (poly-poly-add poly1 (poly-scalar-mul poly2 -1)))
  ([poly1 poly2 & more]
   (reduce poly-poly-sub poly1 (cons poly2 more))))

#_(poly-poly-sub [100 200 300]
                 [1 2 3]
                 [10 20 30])

(defn poly-poly-mul
  "Returns the product of 2 polynoms."
  [poly1 poly2]
  (->> (for [[degree factor] (seq-indexed poly2)]
         (-> poly1
             (poly-scalar-mul factor)
             (poly-shift degree)))
       (reduce poly-poly-add)))

#_(poly-poly-mul [1 2 3] [4 7 2])

;; -----------------------------------------------

#_
(let [poly1 (create-poly 2)
      poly2 (create-poly 2)
      poly-product (poly-poly-mul poly1 poly2)]
  (println (str "Please divide "
                (poly->str poly-product)
                " by "
                (poly->str poly1))))

(defn highest-term [poly]
  (-> (poly-normalize poly)
      (seq-indexed)
      last))

#_ (highest-term [0 5 4 0])
#_ (highest-term [0])

(defn term-term-div [term1 term2]
  (let [[d1 f1] term1
        [d2 f2] term2]
    [(- d1 d2) (/ f1 f2)]))

#_(term-term-div [4 -6] [2 -2])

(defn term-degree< [term1 term2]
  (let [[d1 f1] term1
        [d2 f2] term2]
    (< d1 d2)))

#_(term-degree< [1 2] [1 1])
#_(term-degree< [1 2] [1 2])
#_(term-degree< [1 2] [1 3])

(defn term->poly [term]
  (let [[degree factor] term]
    (-> (vec (repeat degree 0))
        (conj factor))))

#_(term->poly [2 7])

(defn poly-poly-div-steps [product divisor]
  (loop [product product
         steps []]
    (let [product-highest-term (highest-term product)
          divisor-highest-term (highest-term divisor)]
      (if (term-degree< product-highest-term divisor-highest-term)
        steps
        (let [partial-result-poly (-> (term-term-div product-highest-term divisor-highest-term)
                                      term->poly)
              poly-to-subtract    (poly-poly-mul divisor partial-result-poly)
              poly-rest           (poly-poly-sub product poly-to-subtract)]
          (recur poly-rest (conj steps [partial-result-poly poly-to-subtract poly-rest])))))))

#_(let [poly-product [-2 -3 -8 -5 -6]
        poly1 [-1 -1 -2]
        poly2 [2 1 3]]
    (poly-poly-div-steps poly-product poly1))
