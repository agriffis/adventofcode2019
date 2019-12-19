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

(defmacro cond-let
  "A version of `cond` that allows for `:let` terms.
  From https://clojureverse.org/t/favorite-macros/2716/7"
  [& forms]
  {:pre [(even? (count forms))]}
  (when forms
    (let [[test-exp result-exp & more-forms] forms]
      (if (= :let test-exp)
        `(let ~result-exp
              (cond-let ~@more-forms))
        `(if ~test-exp ~result-exp (cond-let ~@more-forms))))))
