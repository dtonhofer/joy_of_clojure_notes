(ns joc.chapter2
   (:require [clojure.test :as test]))

; N.B.: It's "Truthyness" because there is "Truthy" and "Falsy" values.
; "Truthiness" which is something you will find at the NYT or at demonstrations.

; A simple way to define two functions that give the "truthy" or "falsy"
; value as an actual boolean (i.e. either the value 'true' or the value
; 'false' for an arbitrary Clojure 'thing'. These functions are _total_
; (i.e. defined for everything and never raise exceptions or return nil)

(defn truthy? [x] (if x true false))
(defn falsy?  [x] (if x false true))

; clojure.test/is checks whether the value it is given is "truthy", not
; whether it is equal to the boolean 'true'!

(test/is true)  ; passes 
(test/is 1)     ; passes
(test/is 0)     ; passes
(test/is ())    ; passes
(test/is nil)   ; ** fails **
(test/is false) ; ** fails **

; We will perform explicit comparison against the boolean values instead of
; testing for truthyness.

; WHAT'S FALSE?
; The function 'false?' gives 'true' only on 'false'
; Anything else is 'NOT false' (but not necessarily 'true') 

(test/is (= false (false? '()     )))
(test/is (= false (false? []      )))
(test/is (= false (false? {}      )))
(test/is (= false (false? #{}     )))
(test/is (= false (false? nil     )))
(test/is (= true  (false? false   ))) ; only false is false
(test/is (= false (false? true    )))
(test/is (= false (false? 0       )))
(test/is (= false (false? 1       )))
(test/is (= false (false? "false" )))
(test/is (= false (false? "true"  )))
(test/is (= false (false? ""      )))
(test/is (= false (false? (fn []) ))) ; yeah that's far-fetched
(test/is (= false (false? (Boolean. false)))) ; an evil boxedJavaFalse is NOT false
(test/is (= false (false? (Boolean. true))))  ; an evil boxedJavaTrue  is NOT false

; WHAT'S TRUE?
; The function 'true?' gives 'true' only on 'true'
; Anything else is 'NOT true' (but not necessarily 'false') 

(test/is (= false (true? '()     )))
(test/is (= false (true? []      )))
(test/is (= false (true? {}      )))
(test/is (= false (true? #{}     )))
(test/is (= false (true? nil     )))
(test/is (= false (true? false   )))
(test/is (= true  (true? true    ))) ; only true is true
(test/is (= false (true? 0       )))
(test/is (= false (true? 1       )))
(test/is (= false (true? "false" )))
(test/is (= false (true? "true"  )))
(test/is (= false (true? ""      )))
(test/is (= false (true? (fn []) ))) ; yeah that's far-fetched
(test/is (= false (true? (Boolean. false)))) ; an evil boxedJavaFalse is also NOT true
(test/is (= false (true? (Boolean. true))))  ; an evil boxedJavaTrue  is also NOT true

; What's TRUTHY?
; Every 'thing' is 'truthy' except 'nil' and 'false'

(test/is (= true  (truthy? '()     )))
(test/is (= true  (truthy? []      )))
(test/is (= true  (truthy? {}      )))
(test/is (= true  (truthy? #{}     )))
(test/is (= false (truthy? nil     ))) ; only nil and false are not truthy (i.e. falsy)
(test/is (= false (truthy? false   ))) ; only nil and false are not truthy (i.e. falsy)
(test/is (= true  (truthy? true    )))
(test/is (= true  (truthy? 0       )))
(test/is (= true  (truthy? 1       )))
(test/is (= true  (truthy? "false" )))
(test/is (= true  (truthy? "true"  )))
(test/is (= true  (truthy? ""      )))
(test/is (= true  (truthy? (fn []) ))) ; yeah that's far-fetched
(test/is (= true  (truthy? (Boolean. false)))) ; an evil boxedJavaFalse is also truthy
(test/is (= true  (truthy? (Boolean. true))))  ; an evil boxedJavaTrue  is also truthy

; What's FALSY
; By construction, no 'thing' is 'falsy' except 'nil' and 'false'

(map (fn [x] (test/is (= (falsy? x) (not (truthy? x)))))
     [ () [] {} #{} nil false true 0 1 "false" "true" "" (fn []) (Boolean. false) (Boolean. true)])

; "not" of a 'thing' yields exactly "falsy?" of thing

(map (fn [x] (test/is (= (falsy? x) (not x))))
     [ () [] {} #{} nil false true 0 1 "false" "true" "" (fn []) (Boolean. false) (Boolean. true)])

; "not∘not" of a thing yields exactly "truthy?" of a thing
; this corresponds to JavaScript's "!!" -- "bang bang, you are a boolean!"
; which, given an object, yields that object's truthy boolean value. 

(map (fn [x] (test/is (= (truthy? x) (not (not x)))))
     [ () [] {} #{} nil false true 0 1 "false" "true" "" (fn []) (Boolean. false) (Boolean. true)])

