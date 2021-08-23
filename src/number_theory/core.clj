;; Filename: number_theory.clj
;; Description: Various number theory-related functions I created while
;; refreshing my knowledge of Clojure.
;;
;; Created by: Benjamin M. Singleton
;; Created: 10-03-2020
(ns number-theory.core)

(defn is-prime?
  "Determines if a number is prime or not. Defaults to true."
  [n]
  (loop [divisor 2]
    (if (= 0 (mod n divisor))
      false
      (if (>= divisor (/ n 2))
        true
        (recur (inc divisor))))))

(defn print-is-prime
  "Prints out the result of the is-prime function."
  [n]
  (if (is-prime? n)
    (println (str n " is prime"))
    (println (str n " is not prime"))))

(defn get-divisors
  "Returns all factors of n. Whole numbers assumed."
  [n]
  (loop [divisor 1 all-divisors []]
    (if (> divisor (/ n 2))
      all-divisors
      (let [remainder (mod n divisor) quotient (/ n divisor)]
        (recur
          (inc divisor)
          (if (not= remainder 0)
            all-divisors
            (into all-divisors (vector divisor quotient))))))))

(defn get-unique-divisors
  "Uses the get-divisors function to return a sorted, list of only unique divisors."
  [n]
  (apply list (apply sorted-set (get-divisors n))))

(defn get-proper-divisors
  "Gets the proper divisors of a number, which is all of its divisors, excluding the number itself."
  [n]
  (let [all-divisors (get-unique-divisors n)]
    (take (- (count all-divisors) 1) all-divisors)))

(defn is-amicable-pair?
  "Tests if two numbers are an amicable pair."
  [a b]
  (= b (reduce + (get-proper-divisors a))))

(defn is-perfect-number?
  "Tests if a number is the sum of its proper divisors."
  [n]
  (= n (reduce + (get-proper-divisors n))))

(defn get-aliquot-sum
  "Returns the sum of a number's proper divisors."
  [n]
  (reduce + (get-proper-divisors n)))

(defn get-divisors-sum
  "Returns the sum of all of a number's divisors, including itself."
  [n]
  (reduce + (get-unique-divisors n)))

;; This is a pretty inefficient test that generates every triangular number
;; <= n to see if n is triangular. Should be a much more easier way using the
;; quadratic formula to solve for n = (i * (i + 1)) / 2.
(defn inefficient-is-triangular-number?
  "Tests if a number counts objects arranged in an equilateral triangle."
  [n]
  (loop [i 0]
    (let [ith-number (/ (* i (+ i 1)) 2)]
      (if (> ith-number n)
        false
        (if (= ith-number n)
          true
          (recur (inc i)))))))

;; A more efficient test of a triangular number, checks if
;; (1/2) (sqrt(8*n+1) - 1) is a positive integer. Equivalently, (8*n+1) must
;; be a square number, and sqrt(8*n+1 must be odd.
(defn is-triangular-number?
  "Tests if a number counts objects arranged in an equilateral triangle."
  [n]
  (let [inner-result (+ (* 8 n) 1) root (Math/sqrt inner-result)
  rounded-root (int (Math/floor root))]
    ; check if (8 * n + 1) is a square number
    (if (= inner-result (* rounded-root rounded-root))
      ; check if it's odd (it has to be since we're going to subtract one before
      ; halving it.)
      (if (odd? rounded-root)
        true
        false)
      false)))

;; P(s,n) = ((s-2)*n^2-(s-4)*n)/2
;; for a given s-polygonal number x, you can find n by:
;; n=(sqrt(8*(s-2)*x+(s-4)^2)+(s-4))/(2*(s-2))
;; and you can find s by:
;; s = 2+(2/n)*((x-n)/(n-1))
(defn is-s-polygonal-number?
  "Tests if a number is a polygonal number with s sides. When true, returns n
  indicating that x is the nth s-polygonal number."
  [x s]
  (let [inner-result (+ (* 8 x (- s 2)) (int (Math/floor (Math/pow (- s 4) 2))))
  root (Math/sqrt inner-result) rounded-root (int (Math/floor root))]
    ; (println (str "x = " x ", s = " s ", inner-result = " inner-result ", root = " root ", rounded-root = " rounded-root))
    (if (= inner-result (* rounded-root rounded-root))
      (let [numerator (+ rounded-root (- s 4))
      denominator (* 2 (- s 2))
      n (int (Math/floor (/ numerator denominator)))]
        ; (println (str "numerator = " numerator ", denominator = " denominator ", n = " n))
        (if (= numerator (* denominator n))
          n
          false))
      false)))

;; This is, again, a very inefficient way of doing this, as it tests as many
;; integers as needed instead of just using the generating formula.
(defn inefficient-get-first-n-s-polygonal-numbers
  "Returns the first n many s-polygonal numbers."
  [n s]
  (loop [i 1 all-numbers []]
    (if (= (count all-numbers) n)
      all-numbers
    (recur (inc i)
      (if (is-s-polygonal-number? i s)
        (conj all-numbers i)
        all-numbers)))))

;; Much more efficient way of generating s-polygonal numbers. Uses the formula
;; (((s - 2)* n^2) - ((s - 4)* n)) / 2 instead of trial and error.
(defn get-first-n-s-polygonal-numbers
  "Returns the first n many s-polygonal numbers."
  [n s]
  (loop [i 1 all-numbers []]
    (if (= (count all-numbers) n)
      all-numbers
        (recur (inc i)
          (conj all-numbers (/ (- (* (- s 2) (* i i)) (* (- s 4) i)) 2))))))

(defn is-evil-number?
  "Tests if a non-negative number has an even number of 1s in its binary form."
  [n]
  (even?
    (let [binary (Integer/toBinaryString n) last-position (- (count binary) 1)]
      (loop [ones 0 position 0]
        (if (> position last-position)
          ones
          (recur
            (if (= \1 (get binary position))
              (inc ones)
              ones)
            (inc position)))))))

(defn is-odious-number?
  "Tests if a positive number has an odd number of 1s in its binary form."
  [n]
  (not (is-evil-number? n)))

(defn first-n-odious-numbers
  "Generates n many odious numbers."
  [n]
  (loop [i 0 odious-numbers []]
    (if (= n (count odious-numbers))
      odious-numbers
      (recur (inc i)
        (if (is-odious-number? i)
          (conj odious-numbers i)
          odious-numbers)))))

(defn is-k-multiperfect?
  "Tests if the divisors sum (NOT the aliquot sum) of n is equal to n * k."
  [n k]
  (= (* n k) (get-divisors-sum n)))

(defn is-k-hyperperfect?
  "Tests if n = 1 + k * (aliquot sum - 1). k = 1 gives the perfect numbers."
  [n k]
  ; or (= n (+ 1 (* k (- (get-divisors-sum n) n 1)))))
  (= n (+ 1 (* k (- (get-aliquot-sum n) 1)))))

(defn is-deficient?
  "Tests if a number is greater than the sum of its proper divisors."
  [n]
  (< (get-aliquot-sum n) n))

(defn is-abundant?
  "Tests if a number is smaller than the sum of its proper divisors."
  [n]
  (> (get-aliquot-sum n) n))

(defn is-composite?
  "Tests if a number has at least one divisor other than 1 and itself. In other words, tests that a number is not prime."
  [n]
  (> 0 (- (count (get-proper-divisors n)) 1)))

(defn first-many-type-numbers
  "Given a test for some attribute of integers, finds n many integers that satisfy that test."
  [n is-number-type? max-checks]
  (loop [i 1 all-numbers []]
    (if (or (= (count all-numbers) n) (> i max-checks))
      all-numbers
      (recur (inc i)
        (if (is-number-type? i)
          (conj all-numbers i)
          all-numbers)))))

(defn first-many-type-numbers-custom
  "Given a test for some attribute of integers, finds n many integers that satisfy that test.
  Adds more options such as what number to start with and what to increment by."
  [n is-number-type? max-checks start-at start-with inc-by]
  (loop [i start-at all-numbers [start-with]]
    (if (or (= (count all-numbers) n) (> i max-checks))
      all-numbers
      (recur (+ i inc-by)
        (if (is-number-type? i)
          (conj all-numbers i)
          all-numbers)))))

(defn get-first-n-primes
  "Returns the first n many primes."
  [n max-checks]
  (first-many-type-numbers-custom n #(is-prime? %) max-checks 3 2 2))

(defn get-first-n-evil-numbers
  "Generates n many evil numbers."
  [n max-checks]
  ;; we have to drop the first element (nil) that we set by assigning start-with to nil
  (rest (first-many-type-numbers-custom (+ n 1) #(is-evil-number? %) max-checks 0 nil 1)))

(defn get-first-n-square-numbers
  "Returns a non-lazy list of square numbers."
  [n]
  (map #(* % %) (range n)))

(defn find-next-square-number
  "Given a number x, find n, the smallest square number greater than x.
  Returns the *square root* of n. For example, 9 is the smallest square
  number above 8, so (find-next-square-number 8) returns 3."
  [x]
  (loop [n 1]
    (if (>= (* n n) x)
      n
      (recur (inc n)))))

(defn approximate-square-root
  "Approximates the square root of x through a certain number of
  iterations. Entirely superfluous on any computer that can run Clojure,
  but still a good illustration of the algorithm \"Heron's Method\"."
  ([x iterations]
    ; we need to start with an overestimation, so let's use the sqrt of
    ; the first square number >= x
    (approximate-square-root x iterations (find-next-square-number x)))
  ([x iterations previous]
    (if (= iterations 0)
      previous
      (recur x (- iterations 1)
        (/ (+ previous (/ x previous)) 2)))))

; This doesn't improve the performance of approximate-square-root
; normally, but it does if you call it multiple times with different
; values for iterations.
(def memoized-approximate-square-root
  (memoize approximate-square-root))

(defn get-mediant
  "Find the mathematical mediant of two fractions. Given a/b and c/d, the
  mediant is the fraction (a+c)/(b+d). This is guaranteed to lie somewhere
  between a/b and c/d, although not precisely in the middle."
  [x y]
  (assoc (assoc {} :denominator (+ (:denominator x) (:denominator y)))
    :numerator (+ (:numerator x) (:numerator y))))

(defn absolute-fraction-difference
  "Returns the absolute difference of a decimal number and my weird
  implementation of fractions."
  [decimal-number fraction-map]
  (Math/abs
    (- decimal-number (/ (:numerator fraction-map) (:denominator fraction-map)))))

(defn stringify-fraction
  "Constructs a string representation one of my weird fractions, which are
  maps with keys :numerator and :denominator."
  [x]
  (str "(" (:numerator x) "/" (:denominator x) ")"))

(defn approximate-decimal-number
  "My implementation of using Farey sequences to find the closet fraction to a
  given decimal, within a certain limit of precision."
  [decimal-number denominator-limit]
  (loop [left-bound {:numerator 0 :denominator 1} right-bound {:numerator 1 :denominator 1}]
    (let [mediant (get-mediant left-bound right-bound)]
      ; (println (str "Looping with mediant " (stringify-fraction mediant) ", left-bound " (stringify-fraction left-bound) " right-bound " (stringify-fraction right-bound)))
      (if (> (:denominator mediant) denominator-limit)
        (if (< (absolute-fraction-difference decimal-number left-bound)
          (absolute-fraction-difference decimal-number right-bound))
            left-bound
            right-bound)
        (if (< decimal-number (/ (:numerator mediant) (:denominator mediant)))
          (recur left-bound mediant)
          (recur mediant right-bound))))))

(defn -main
  "Illustrates some of the functions in this library."
  [& args]
  (do
    ;; do not run the following example unless you have time to kill.
    (println "How long to generate the first 2000 hyperperfect numbers.")
    (time (first-many-type-numbers 10 #(is-k-hyperperfect? % 1) 2000))
    (println "The first 100 prime numbers:")
    (println (get-first-n-primes 100 10000))
    (println "The first 100 evil numbers:")
    (println (get-first-n-evil-numbers 100 10000))
    (println (str "Approximating square root of 8 to 10 iterations."))
    (loop [n 0]
      (println (str "Iteration " n ": " (memoized-approximate-square-root 8 n) " ~= " (double (memoized-approximate-square-root 8 n))))
      (if (< n 10)
        (recur (inc n))))
    ))
