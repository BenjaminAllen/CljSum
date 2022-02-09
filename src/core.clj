(ns core)

(defn convert-op-or-number [n]
  (case n
    "+" :add
    "-" :subtract
    "/" :divide
    "*" :multiply
    "(" :open_bracket
    ")" :close_bracket
    (Integer/parseInt n)))

(defn split-sum [sum]
  (as-> sum sum
    (re-seq #"\d+|[+-\\*\/\(\)]" sum)
    (map convert-op-or-number sum)
    (conj sum :add)))

(defn needs-multiple? [head item]
  (or
   (and (= head :close_bracket)
        (= item :open_bracket))
   (and (= head :close_bracket)
        (number? item))
   (and (number? head)
        (= item :open_bracket))))

(defn add-multiple [stack item]
  (apply conj stack [:multiply item]))

(defn add-explicit-multiple [stack item]
  (let [head (last stack)]
    (if (needs-multiple? head item)
      (add-multiple stack item)
      (conj stack item))))

(defn add-explicit-multiples [sum]
  (reduce add-explicit-multiple [] sum))

(defn split-brackets [sum]
  (loop [[head & tail] sum
         result [:add]
         depth 0]
    (cond
      (= head :close_bracket)
      (if (= depth 0)
        [result tail]
        (recur tail (conj result head) (dec depth)))

      (= head :open_bracket)
      (recur tail (conj result head) (inc depth))

      :else
      (recur tail (conj result head) depth))))

(defn mult-reducer [stack item]
  (let [[op val] item
        [[headOp headVal] & tail] stack]    
    (case op
      :multiply (conj tail (list headOp (* headVal val)))
      :divide (conj tail (list headOp (/ headVal val)))
      (conj stack item))))

(defn solve-multiples [sum]
  (reverse (reduce mult-reducer () sum)))

(defn add-reducer [acc item]
  (let [op (first item)
        val (last item)]
    (case op
      :add (+ acc val)
      (- acc val))))

(defn solve-adds [sum]
  (reduce add-reducer 0 sum))

(defn calculate [sum]
  (let [sum (split-sum sum)]
    (letfn [(solve [sum]
              (->> (add-explicit-multiples sum)
                   (solve-brackets)
                   (partition 2)
                   (solve-multiples)
                   (solve-adds)))
            (solve-brackets [sum]
              (loop [[head & tail] sum
                     stack []]
                (cond
                  (nil? head)
                  stack

                  (= head :open_bracket)
                  (let [[to-solve rest] (split-brackets tail)]
                    (println "to-solve" to-solve "rest" rest)
                    (recur rest (conj stack (solve to-solve))))

                  :else
                  (recur tail (conj stack head)))))]
      (solve sum))))

(comment
  (calculate "7-4-1+(3)8/2")
  )