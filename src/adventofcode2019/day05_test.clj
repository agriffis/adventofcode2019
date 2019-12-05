(ns adventofcode2019.day05-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [adventofcode2019.day05 :refer :all]))

;;; ----------------------------------------------------------------------
;;; Part one
;;; ----------------------------------------------------------------------

(def sample-program "1,9,10,3,2,3,11,0,99,30,40,50")

(defmacro with-out-str-result
  "Call a function capturing stdout for testing, returns [result out-str]."
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       (let [r# ~@body]
         [r# (str s#)]))))

(defn test-op
  "Run a single operation of a program in test mode, returning {:result result
  :output output}. This can also accept its own return value for stepping."
  ([program input] (test-op (parse program) input 0))
  ([program input ip]
   (let [[result output] (with-out-str-result (operate program ip input))]
     {:result result :output output}))
  ([{[ip program input] :result}] (test-op program input ip)))

(defn test-program
  "Run a program, storing the last line of output under the key
  :diagnostic-code."
  [program input]
  (let [[result output] (with-out-str-result (execute program input))
        output (if (= "" output) nil (str/split-lines output))]
    (assoc result :diagnostic-code (last output))))

(deftest day05-one
  (is (= (test-op sample-program [])
         {:result [4 (parse "1,9,10,70,2,3,11,0,99,30,40,50") []] :output ""}))
  (is (= (test-op "1002,4,3,4,33" [])
         {:result [4 [1002 4 3 4 99] []] :output ""}))
  (is (= (test-program sample-program []) {:value 3500 :diagnostic-code nil})))

;;; ----------------------------------------------------------------------
;;; Part two
;;; ----------------------------------------------------------------------

(def larger-example
  "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")

(deftest day05-two
  (is (= (test-op "3,3,99,-1" [42]) {:result [2 [3 3 99 42] ()] :output ""}))
  (is (= (test-op "1107,-1,1,4,-1" [])
         {:result [4 [1107 -1 1 4 1] []] :output ""}))
  ;; position mode
  (is (= (test-program "3,9,8,9,10,9,4,9,99,-1,8" [8])
         {:value 3 :diagnostic-code "1"}))
  (is (= (test-program "3,9,8,9,10,9,4,9,99,-1,8" [7])
         {:value 3 :diagnostic-code "0"}))
  (is (= (test-program "3,9,7,9,10,9,4,9,99,-1,8" [8])
         {:value 3 :diagnostic-code "0"}))
  (is (= (test-program "3,9,7,9,10,9,4,9,99,-1,8" [7])
         {:value 3 :diagnostic-code "1"}))
  ;; immediate mode
  (is (= (test-program "3,3,1108,-1,8,3,4,3,99" [8])
         {:value 3 :diagnostic-code "1"}))
  (is (= (test-program "3,3,1108,-1,8,3,4,3,99" [7])
         {:value 3 :diagnostic-code "0"}))
  (is (= (test-program "3,3,1107,-1,8,3,4,3,99" [8])
         {:value 3 :diagnostic-code "0"}))
  (is (= (test-program "3,3,1107,-1,8,3,4,3,99" [7])
         {:value 3 :diagnostic-code "1"}))
  ;; larger example
  (is (= (test-program larger-example [7]) {:value 3 :diagnostic-code "999"}))
  (is (= (test-program larger-example [8]) {:value 3 :diagnostic-code "1000"}))
  (is (= (test-program larger-example [9]) {:value 3 :diagnostic-code "1001"})))
