(ns joc.chapter6
   (:require [clojure.test :as test]))

; Run using (load-file "quicksort.clj"). The test code will run at once.

; ===
; Neverending quicksort fun!!!
; Since 1959 in fact: https://en.wikipedia.org/wiki/Quicksort#History
; ===

; The quicksort example in "Joy of Clojure" 2nd edition, page 133 feels too code-golfy
; (or too much the result of several skillfull rewrites) to really know what is
; happening in there. One really has to think a bit to determine what this program
; might do although the image on page 135 helps. The fact that there is no typing,
; assertions or test code adds to the general unease.

; Here, I try to work on that code a bit.

; ===
; Generate n pseudo-random integers from the interval [0..maxx[
; ===
; We will use this for exercising the code.

(defn rand-ints [n maxx]
        { :pre [ (int? n) (>= n 0) (int? maxx) (> maxx 0) ] }
        ; as expected lazyness destroys the ability to do post-condition checks
        ; so we don't do those!
        (take n (repeatedly #(rand-int maxx))))

; ===
; A function to test whether a sequence of numbers is sorted
; ===
; Note that there already is "sorted?" in clojure/core whch checks for interface
; implementation instead.

(defn my-sorted-inner? [p s]
  { :pre [ (number? p) (sequential? s) ] }
  (or (empty? s)
     (let [f (first s) r (rest s)]
        (assert (number? f))
        (and (<= p f) (my-sorted-inner? f r)))))

(defn my-sorted? [s]
  { :pre [ (sequential? s) ] }
  (or (empty? s)
      (my-sorted-inner? (first s) (rest s))))

(test/is (my-sorted? []))
(test/is (my-sorted? [1]))
(test/is (my-sorted? [1 2]))
(test/is (my-sorted? [1 2 3]))
(test/is (my-sorted? [3 3 3]))
(test/is (not (my-sorted? [3 2 1])))

(def _ (test/is (thrown? AssertionError (my-sorted? :a))))         ; "def" to suppress stack trace
(def _ (test/is (thrown? AssertionError (my-sorted? [ 1 2 :a ])))) ; "def" to suppress stack trace

; ===
; Original quicksort from "Joy Of Clojure" p.133 (but with a different function name.)
; ===
; How can we be sure that it performs a quicksort correctly??
; Note a special case:
; (qort-joc-inner [[]]) returns (), which is correct...
; But only because everything is inside the "lazy-seq" ... and (lazy-seq nil) resolves to ().
; If there were no lazy-seq, (qort-joc-inner [[]]) would be nil, which would be wrong.

(defn qsort-joc-inner [work]
  (lazy-seq
    (loop [[part & parts] work]
      (if-let [[pivot & xs] (seq part)]
        (let [smaller? #(< % pivot)]
          (recur (list*
                  (filter smaller? xs)
                  pivot
                  (remove smaller? xs)
                  parts)))
      (when-let [[x & parts] parts]
        (cons x (qsort-joc-inner parts)))))))

(defn qsort-joc [ xs ]
   (qsort-joc-inner (list xs)))

(test/is (my-sorted? (qsort-joc [])))
(test/is (my-sorted? (qsort-joc [1])))
(test/is (my-sorted? (qsort-joc [1 2])))
(test/is (my-sorted? (qsort-joc [1 2 3])))
(test/is (my-sorted? (qsort-joc [3 2 1])))
(test/is (my-sorted? (qsort-joc [3 3 3])))
(test/is (my-sorted? (qsort-joc (rand-ints 100 1000)))) ; 100 ints from 0...999
(test/is (my-sorted? (qsort-joc (rand-ints 1000 100)))) ; 1000 ints from 0..99

; Now for a more "examinable" quicksort
; - - - - - - - - - - - - - - - - - - -

; ===
; Verification code.
; ===
; Check whether an intermediary result has the correct format: A sequence of
; sequences of numbers, separated by exactly 1 number (the pivot)

(defn number-seq? [coll]
   (and
      (sequential? coll)
      (or
         (empty? coll)
         (let [ fc (first coll) rc (rest coll) ]
            (and
               (number? fc)
               (number-seq? rc))))))

(defn work-seq? [coll]
   (and
      (sequential? coll)
      (or
         (empty? coll)
         (let [ fc (first coll) rc (rest coll) ]
            (and
               (number-seq? fc)
               (or
                  (empty? rc)
                  (let [ frc (first rc) rrc (rest rc)  ]
                     (and
                        (number? frc)
                        (not (empty? rrc))
                        (work-seq? rrc)))))))))

(test/is (number-seq?       []))
(test/is (number-seq?       [1 2 3 4 5]))
(test/is (number-seq?       [1]))
(test/is (not (number-seq?  1)))
(test/is (not (number-seq?  :a)))
(test/is (not (number-seq?  [1 2 3 :a 5])))

(test/is (not (work-seq? :a)))
(test/is (work-seq?      []))
(test/is (work-seq?      [[ 1 2 3 ]]))
(test/is (work-seq?      [[ 1 ] 2 [ 3 ]]))
(test/is (work-seq?      [[ 1 ] 2 [ 3 4 ] 5 [ 6 7 ]]))
(test/is (not (work-seq? [[ 1 ] 2 [ 3 4 ] [ 6 7 ]])))
(test/is (not (work-seq? [[ 1 ] 2 [ :a ] [ 6 7 ]])))
(test/is (not (work-seq? [ 1 [ 2 ] 3 [ 3 4 ] 5 [ 6 7 ]])))
(test/is (not (work-seq? [ [ 2 ] 3 [ 3 4 ] 5 [ 6 7 ] 8 ])))

; ===
; Explicit quicksort
; ===
; Quicksort that does not return lazy-seq instantiations, has assertions and
; prints out what it is doing (to lazify it, we must avoid printouts) and checks
; intermediate results.

(defn qsort-two-explicit-inner [work]

   { :pre [ (work-seq? work) ] }

   (println "Received work" work "of class" (class work))

   ; In the original destructuring and looping is done on the same line, which is confusing.
   ; Let's make this explicit! Original value is "work" and what is changing in the loop
   ; is "loopwork".

   (loop [loopwork work]

      (println "In loop with loopwork" loopwork "of class" (class loopwork))

      ; Decompose "loopwork" into a head "part" and a tail called "partz".
      ; If "loopwork" cannot be seq-ed, the attempt at destructuring fails and an Exception is thrown!!

      (let [[ part & partz ] loopwork ]

         ; "part" is the "leftmost sequential" (empty or nonempty) of loopwork, never an old pivot (a number)

         (assert (number-seq? part))
         (println "Examining leftmost part" part "of class" (class part))

         ; Try to make a seq out of "part".
         ; If making a seq succeeds, choose the head as "pivot" and put the rest into "valuez".
         ; If making a seq fails (case of "part" empty), the "else" part (the 'when-let') is taken.

         (if-let [[pivot & valuez] (seq part)]

            ; "if-let" success branch.
            ; Separate by pivot and then sort the bunch of smaller elements (which may be the empty seq)

            (do (println "if-let succeeds with pivot=" pivot)

               (let [ smaller? #(< % pivot)
                      smz      (filter smaller? valuez)
                      lgz      (remove smaller? valuez)
                      nxxt     (list* smz pivot lgz partz) ]
                     (recur nxxt))) ; ### loop around to sort the new leftmost sequential ("smz")

            ; "if-let" failure branch.
            ; Happens if "part" is actually empty. It can be abandoned.
            ; The "partz" is now an old pivot on the leftmost position (correctly sorted) and more stuff to the right.

            (do (println "if-let fails because part=" part)

               (assert (= part ()))

               (if-let [[oldpivot & rightpartz] partz]
                        ; partz can be decomposed ... abandon loop and do a fully recursive call
                        (do (println "Keeping oldpivot=" oldpivot "and doing recursive call with" rightpartz)
                            (cons oldpivot (qsort-two-explicit-inner rightpartz)))
                        ; partz cannot be decomposed ... it's empty. Return an empty sequential as result
                        (do (assert (empty? partz))
                            (println "There is nothing left; partz=" partz)
                            [])))))))

(defn qsort-two-explicit [ xs ]
   (qsort-two-explicit-inner (list xs)))

(test/is (my-sorted? (qsort-two-explicit [])))
(test/is (my-sorted? (qsort-two-explicit [1])))
(test/is (my-sorted? (qsort-two-explicit [1 2])))
(test/is (my-sorted? (qsort-two-explicit [1 2 3])))
(test/is (my-sorted? (qsort-two-explicit [3 2 1])))
(test/is (my-sorted? (qsort-two-explicit [3 3 3])))
(test/is (my-sorted? (qsort-two-explicit (rand-ints 10 100)))) ; 10 ints from 0...99
(test/is (my-sorted? (qsort-two-explicit (rand-ints 100 10)))) ; 100 ints from 0..9

; ===
; Compact quicksort, rebuild
; ===
; So let's remove all the extras and add the lazy-seq wrapping

(defn qsort-two-inner [work]
   { :pre [ (work-seq? work) ] }
   (lazy-seq
      (loop [loopwork work]
         (let [[ part & partz ] loopwork ]
            (if-let [[pivot & valuez] (seq part)]
                  (let [ smaller? #(< % pivot)
                         smz      (filter smaller? valuez)
                         lgz      (remove smaller? valuez)
                         nxxt     (list* smz pivot lgz partz) ]
                        (recur nxxt))
                  (if-let [[oldpivot & rightpartz] partz]
                          (cons oldpivot (qsort-two-inner rightpartz))
                          []))))))

(defn qsort-two [ xs ]
   (qsort-two-inner (list xs)))

(test/is (my-sorted? (qsort-two [] )))
(test/is (my-sorted? (qsort-two [1] )))
(test/is (my-sorted? (qsort-two [1 2] )))
(test/is (my-sorted? (qsort-two [1 2 3] )))
(test/is (my-sorted? (qsort-two [3 2 1] )))
(test/is (my-sorted? (qsort-two [3 3 3] )))
(test/is (my-sorted? (qsort-two (rand-ints 10 100)))) ; 10 ints from 0...99
(test/is (my-sorted? (qsort-two (rand-ints 100 10)))) ; 100 ints from 0..9


