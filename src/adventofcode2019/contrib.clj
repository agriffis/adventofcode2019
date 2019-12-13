(ns adventofcode2019.contrib)

;;; prime factorization from https://gist.github.com/unclebob/632303

(defn factors-starting-at
  ([f n] (factors-starting-at f n []))
  ([f n result]
   (cond (> f (Math/sqrt n)) (if (= n 1) [] (conj result n))
         (zero? (mod n f)) (recur f (/ n f) (conj result f))
         :else (recur (inc f) n result))))

(defn prime-factors-of
  [n]
  (factors-starting-at 2 n))
