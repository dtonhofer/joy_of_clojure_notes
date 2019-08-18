(ns joc.chapter7
   (:require [clojure.test :as test]))

; Load with (load-file "gcd.clj"). The test code will run at once.

; ===
; Naive GCD calculation in Clojure. Based on "The Joy of Clojure" 2nd edition, p. 160: "Why Recur?".
; ===
; Added pre/post conditions and a stack depth counter.
; https://en.wikipedia.org/wiki/Greatest_common_divisor#Using_Euclid's_algorithm

(defn gcd-with-plumbing [x y mydepth]
	{
		:pre  [ (integer? x) (integer? y) (> x 0) (> y 0) ]
	;	:post [ (= 0 ( mod x % )) (= 0 ( mod y % )) ]
	}
	(cond
		(> x y) (recur (- x y) y (inc mydepth))
		(< x y) (recur x (- y x) (inc mydepth))
	:else { :gcd x :calldepth mydepth } ))

(defn gcd [x y] (gcd-with-plumbing x y 1))

(test/is (= (gcd 13 13) { :gcd 13 :calldepth 1 }) )
(test/is (= (gcd 13 (* 2 13)) { :gcd 13 :calldepth 2 } ))
(test/is (= (gcd 57029458945729857458023 5894765894983425904)) { :gcd 1 :calldepth 9839 } )
(test/is (= (gcd (* 1N 2 43 857 304193 218760654189823) (* 1N 2 2 2 2 43 52747 162435279439)) { :gcd 86 :calldepth 832274 } ))
