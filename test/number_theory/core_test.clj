(ns number-theory.core-test
  (:require [clojure.test :refer :all]
            [number-theory.core :refer :all]))

(deftest is-prime?-test
  (testing "Check if a number is prime or not."
    (is (= (is-prime? 1) true))
    (is (= (is-prime? 2) true))
    (is (= (is-prime? 3) true))
    (is (= (is-prime? 5) true))
    (is (= (is-prime? 7) true))
    (is (= (is-prime? 9) false))
    ))

(deftest get-unique-divisors-test
  (testing "Find all the divisors of a given number."
    (is (= (get-unique-divisors 1) 1))
    (is (= (get-unique-divisors 2) [1 2]))
    (is (= (get-unique-divisors 3) [1 3]))
    (is (= (get-unique-divisors 4) [1 2 4]))
    (is (= (get-unique-divisors 5) [1 5]))
    (is (= (get-unique-divisors 6) [1 2 3 6]))
    (is (= (get-unique-divisors 7) [1 7]))
    (is (= (get-unique-divisors 8) [1 2 4 8]))
    (is (= (get-unique-divisors 9) [1 3 9]))
    (is (= (get-unique-divisors 10) [1 2 5 10]))
    (is (= (get-unique-divisors 60) [1 2 3 4 5 6 10 12 15 20 30 60]))
    (is (= (get-unique-divisors 137) [1 137]))
    (is (= (get-unique-divisors 5080) [1 2 4 5 8 10 20 40 127 254 508 635 1016 1270 2540 5080]))
  ))

(deftest is-amicable-pair?-test
  (testing "Determine if two numbers are an amicable pair, meaning the
  second number is the sum of the first number's proper divisors."
    (is (true? (is-amicable-pair? 1 1)))
    (is (true? (is-amicable-pair? 1 2)))
    (is (true? (is-amicable-pair? 4 7)))
  ))

(deftest find-next-square-number-test
  (testing "Find the square root of the square number above x."
    (is (= (find-next-square-number 8) 3))
    (is (= (find-next-square-number 15) 4))
    (is (= (find-next-square-number 101) 11))
    ))

(deftest approximate-square-root-test-1
  (testing "Approximate the square root of a given number, limiting
  the number of iterations in calculating it."
    (is (= (approximate-square-root 9 1) 3))
    (is (= (approximate-square-root 9 2) 3))

    (is (= (approximate-square-root 16 1) 4))
    (is (= (approximate-square-root 16 2) 4))

    (is (= (approximate-square-root 17 1) (/ 21 5)))
    (is (= (approximate-square-root 17 2) (/ 433 105)))
    (is (= (approximate-square-root 17 3) (/ 187457 45465)))
    (is (= (approximate-square-root 17 4) (/ 35140126337 8522732505)))
    (is (= (approximate-square-root 17 5) (/ 1234828478980320906497 299489896962156484185)))
  ))

(deftest approximate-square-root-test-2
  (testing "Approximate the square root of 2."
    (is (= (approximate-square-root (Math/sqrt 2) 1) 1.3535533905932737))
    (is (= (approximate-square-root (Math/sqrt 2) 2) 1.1991844452241198))
    (is (= (approximate-square-root (Math/sqrt 2) 3) 1.1892486211774982))
    (is (= (approximate-square-root (Math/sqrt 2) 4) 1.189207115727028))
    (is (= (approximate-square-root (Math/sqrt 2) 5) 1.1892071150027212))
    (is (= (approximate-square-root (Math/sqrt 2) 6) 1.1892071150027212))
    (is (= (approximate-square-root (Math/sqrt 2) 10) 1.1892071150027212))
  ))

(deftest approximate-decimal-number-test-1
  (testing "Different approximations of 1/3."
    (is (=
      (approximate-decimal-number 0.333333333333333333 1)
        (clojure.lang.Numbers/toRatio (rationalize 0/1))))
    (is (=
      (approximate-decimal-number 0.333333333333333333 2) (/ 1 2)))
    (is (=
      (approximate-decimal-number 0.333333333333333333 3) (/ 1 3)))
    (is (=
      (approximate-decimal-number 0.333333333333333333 300) (/ 1 3)))
      ))

(deftest approximate-decimal-number-test-2
  (testing "Different approximations of sqrt(2)-1."
    (is (=
      (approximate-decimal-number (- (Math/sqrt 2) 1) 1)
        (clojure.lang.Numbers/toRatio (rationalize 0/1))))
    (is (=
      (approximate-decimal-number (- (Math/sqrt 2) 1) 2) (/ 1 2)))
    (is (=
      (approximate-decimal-number (- (Math/sqrt 2) 1) 3) (/ 1 3)))
    (is (=
      (approximate-decimal-number (- (Math/sqrt 2) 1) 10) (/ 2 5)))
    (is (=
      (approximate-decimal-number (- (Math/sqrt 2) 1) 100) (/ 41 99)))
    (is (=
      (approximate-decimal-number (- (Math/sqrt 2) 1) 200) (/ 70 169)))
    (is (=
      (approximate-decimal-number (- (Math/sqrt 2) 1) 300) (/ 70 169)))
    (is (=
      (approximate-decimal-number (- (Math/sqrt 2) 1) 1000) (/ 408 985)))
    (is (=
      (approximate-decimal-number (- (Math/sqrt 2) 1) 10000) (/ 3363 8119)))
      ))
