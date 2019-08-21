; ===
; "neighbor" function from Chapter 5 (page 94) of the Joy Of Clojure, 2nd edition.
; ...modified and spec-ed!
;
; Applicable license is the unlicense: http://unlicense.org
; except for code taken from "Joy of Clojure, 2nd edition" (i.e. "neighbors-orig"),
; which is distributed under the Eclipse License.
;
; In a Leiningen project called "neighbor", this will be file src/neighbor/core.clj
; The project needs to declare these dependencies:
;  :dependencies [[org.clojure/clojure "1.10.0"]
;                 [org.clojure/test.check "0.9.0"]
; ===

(ns neighbor-app.core
  "Simple implementation of 'matrix neighborhood'
   From 'The Joy of Clojure' 2nd edition
   Chapter 5, page 94."
  (:gen-class)
  (:require
   [clojure.spec.alpha            :as spec]     ; https://clojure.org/guides/spec                                       : spec creation
   [clojure.spec.test.alpha       :as instr]    ; https://clojure.github.io/spec.alpha/clojure.spec.test.alpha-api.html : instrumentation of functions
   [clojure.test.check            :as chk]      ; https://github.com/clojure/test.check/blob/master/README.md           : property-based generative testing
   [clojure.test.check.generators :as chkgen]   ; https://github.com/clojure/test.check/blob/master/README.md           : property-based generative testing
   [clojure.test.check.properties :as chkprop]  ; https://github.com/clojure/test.check/blob/master/README.md           : property-based generative testing
   [clojure.test                  :as test]     ; https://clojure.github.io/clojure/clojure.test-api.html               : unit testing framework
   [clojure.set                   :as cljset]   ; https://clojure.github.io/clojure/clojure.set-api.html                : set operations
   [clojure.string                :as cljstr])) ; https://clojure.github.io/clojure/clojure.string-api.html             : string operations

; ------------------------
; As is the custom in math, matrix "cell designators" are written: [row col].
; Counting starts from 0. Only square matrixes are considered.
;
; Example matrix cell designators:
;
;    +----colum--------------->
;    |
;    |   [0 0] [0 1] [0 2]
;   row  [1 0] [1 1] [1 2]
;    |   [2 0] [2 1] [2 2]
;    V

; ===
; Matrix "cell deltas" for stepping "up", "down", "left", "right"
; ===
; Includes the "inverted map" as used in postcondition checks.
; The original function from "The Joy of Clojure" does not use these.

(def deltas { ::up    [-1  0]
              ::down  [ 1  0]
              ::left  [ 0 -1]
              ::right [ 0  1] })

(def deltas-inv (cljset/map-invert deltas))

; ===
; specs
; ===

; "Must be a sequential collection of exactly 2 integers"

(spec/def ::mustbe-seql-of-2-int
   (spec/coll-of integer? :kind sequential? :count 2))

; "Must be a vector of exactly 2 integers"

(spec/def ::mustbe-vec-of-2-int
   (spec/coll-of integer? :kind vector? :count 2))

; "Must be a an integer [0..100]"
; The upper limit is added because we want to generate test data
; within that limited range only.

(spec/def ::mustbe-matrix-size-int
   (spec/and integer? #(<= 0 % 100)))

; This just prints the argument passed to the spec and returns true
; Mute this by redefining "::justprint" directly below.

(spec/def ::justprint
   #(do
      ; "vec" to realize the LazySeq, which is not realized by join
      (print (cljstr/join [ "::justprint ▶ " (vec %) "\n"] ))
      true))

; (spec/def ::justprint (fn [_] true))

; "Must be a sequential of 0..4 vectors of vectors of exactly 2 integers".
; Note that the "pred" passed to "spec/coll-of" is actually a "spec" here.
; This really works!!

(spec/def ::mustbe-seql-of-vec-of-2-int
   (spec/and
      ::justprint
      (spec/coll-of
         ::mustbe-vec-of-2-int
         :kind sequential?
         :min-count 0
         :max-count 4)))

; "Must be a collection of [row col] where the difference to the input
; [row col] 'rc' is a known delta"

(spec/def ::mustbe-known-delta
   (spec/and
      ::justprint
      (fn [x]
         (let [ [retval rc]  x ; dstructure "x" into "return value" and "rc" as possed in :post
                subtract-rcs (fn [rc1 rc2] (map - rc1 rc2))
                delta-coll   (map (partial subtract-rcs rc) retval) ]
            ; (print "We have this delta-collection now: " delta-coll "\n")
            ; 1) predicate over collection, short version
            (every? #(contains? deltas-inv %) delta-coll)
            ; 2) predicate over collection, long version with printout
            ; (every? (fn [drc] (let [drx (get deltas-inv drc)] (print drc "->" drx "\n") drx)) delta-coll)
            ; 3) calling a spec (not recommended I reckon)
            ; spec/valid? (spec/coll-of #(contains? deltas-inv %)) delta-coll)
            ))))

; ===
; "neighbors" function, original from "The Joy of Clojure", page 95.
; (neighbors-orig size [y x])
; ===
; This original "neighbors" function seems hard to read.
;
; Note the function overloading.
;
; At this point in the book, the overloading is not necessary, but it
; will become necessary on page 229, when other neighborhoods than
; [[-1 0] [1 0] [0 -1] [0 1]] are considered.
;
; "pre:" and "post:" checks have been added here, they are not in the original.
;
; For the postcondition, we need to pass the returned value via '%'
; and the original rc argument; create a structure holding those.
; Note that the postcondition has no '#' that might go with the '%'.
;
; size: The size of the square matrix to consider, an integer >= 0.
; yx  : A sequence of "row" and "column" giving the cell in whose neighborhood
;       we are interested.

(defn neighbors-orig

   ([size yx]

        {:pre  [ (spec/valid? ::mustbe-seql-of-2-int yx)
                 (spec/valid? ::mustbe-matrix-size-int size) ]

         :post [ (spec/valid? ::mustbe-seql-of-vec-of-2-int %)
                 (spec/valid? ::mustbe-known-delta [% yx])] }

       (neighbors-orig [[-1 0] [1 0] [0 -1] [0 1]]
                       size
                       yx))

   ([deltas size yx]
      (filter (fn [new-yx]
                 (every? #(< -1 % size) new-yx))
              (map #(vec (map + yx %))
                   deltas))))

; ===
; "neighbor" function, 1st generation re-implementation.
; (neighbors-orig sqmsz [row col])
; ===
; The same as above but with a more extensive let to better show what's
; going on. This function still uses overloading, but overloading seems
; awkward once you use the "let" block.
;
; sqmsz: The size of the square matrix to consider, an integer >= 0.
; rc   : A sequence of "row" and "column" giving the cell in whose neighborhood
;        we are interested.

(defn neighbors-gen1

     ([sqmsz rc]

        {:pre  [ (spec/valid? ::mustbe-seql-of-2-int rc)
                 (spec/valid? ::mustbe-matrix-size-int sqmsz) ]

         :post [ (spec/valid? ::mustbe-seql-of-vec-of-2-int %)
                 (spec/valid? ::mustbe-known-delta [% rc])] }

        (neighbors-gen1 [[-1 0] [1 0] [0 -1] [0 1]]
                        sqmsz
                        rc))

     ([deltas sqmsz rc]
        (let [ in-sq-matrix?     (fn [x]        (and (<= 0 x) (< x sqmsz)))
               in-sq-matrix-rc?  (fn [rc]       (every? in-sq-matrix? rc))
               add-two-rc        (fn [rc1 rc2]  (vec (map + rc1 rc2)))
               get-rc-neighbors  (fn [rc]       (map (partial add-two-rc rc) deltas)) ]
           (filter in-sq-matrix-rc? (get-rc-neighbors rc))))
)

; ===
; "neighbor" function, 2nd generation re-implementation.
; (neighbors-orig sqmsz [row col])
; ===
; We are using the "deltas" map to explicitly name the directions.
; There is no overloading anymore. It is smoother to just add a "let"
; with the directions (delta-rcs) to the single function instead of
; creating a three-argument inner function that is passed the directions.

(defn neighbors-gen2 [sqmsz rc]

     {:pre  [ (spec/valid? ::mustbe-seql-of-2-int rc)
              (spec/valid? ::mustbe-matrix-size-int sqmsz) ]

      :post [ (spec/valid? ::mustbe-seql-of-vec-of-2-int %)
              (spec/valid? ::mustbe-known-delta [% rc])] }

     (let [ delta-rcs         [ (::up deltas) (::down deltas) (::left deltas) (::right deltas) ]
            in-sq-matrix?     (fn [x]        (and (<= 0 x) (< x sqmsz)))
            in-sq-matrix-rc?  (fn [rc]       (every? in-sq-matrix? rc))
            add-two-rc        (fn [rc1 rc2]  (vec (map + rc1 rc2))) ; "vec" realizes the LazySeqs into vectors
            get-rc-neighbors  (fn [rc]       (map (partial add-two-rc rc) delta-rcs)) ]
        (filter in-sq-matrix-rc? (get-rc-neighbors rc))))


; ===
; "neighbor" function, 3rd generation re-implementation.
; (neighbors-orig sqmsz [row col])
; ===
; We are using the "deltas" map to explicitly name the directions.
; There is no overloading anymore. It is smoother to just add a "let"
; with the directions (delta-rcs) to the single function instead of
; creating a three-argument inner function that is passed the directions.
;
; This is mostly useful if you want to generate input values
; for testing: ":ret" and ":fn" are for property generation only!
;
; https://blog.taylorwood.io/2018/10/15/clojure-spec-faq.html
;
; "If you instrument a function spec’d with fdef or fspec, only its :args spec will
;  be checked during each invocation."

(defn neighbors-gen3 [sqmsz rc]

     (let [ delta-rcs         [ (::up deltas) (::down deltas) (::left deltas) (::right deltas) ]
            in-sq-matrix?     (fn [x]        (and (<= 0 x) (< x sqmsz)))
            in-sq-matrix-rc?  (fn [rc]       (every? in-sq-matrix? rc))
            add-two-rc        (fn [rc1 rc2]  (vec (map + rc1 rc2))) ; "vec" realizes the LazySeqs into vectors
            get-rc-neighbors  (fn [rc]       (map (partial add-two-rc rc) delta-rcs)) ]
        (filter in-sq-matrix-rc? (get-rc-neighbors rc)) ))

(spec/fdef neighbors-gen3
    :args  (spec/cat :sqmsz ::mustbe-matrix-size-int
                     :rc    ::mustbe-seql-of-2-int)
    :ret   ::mustbe-seql-of-vec-of-2-int)  ; only used during property-based testing
;    :fn    ::must-be-known-delta ; only used during property-based testing TODO

; ===
; Put a collection of [row col] into an expected form.
; ===
; This is used to run the test code

(defn canon [rc-coll]
   (let [ cmp (fn [rc1 rc2]
                  (let [ [r1 c1] rc1     ; destructure
                         [r2 c2] rc2 ]   ; destructure
                     (cond (< r1 r2) -1  ; sort by row
                           (> r1 r2) +1
                           (< c1 c2) -1  ; then by column
                           (> c1 c2) +1
                           true       0))) ]
      (vec (sort cmp rc-coll))))

; ===
; Testing
; ===

(defn test-success [ sqmsz rc expected txt ]
   (do
      (print (cljstr/join [ "test-success ▶ " "'" txt "'" "\n" ]))
      (doseq [ funame '[
                          neighbors-orig
                          neighbors-gen1
                          neighbors-gen2
                          neighbors-gen3
                       ]]
         (let [ f  (ns-resolve 'neighbor-app.core funame) ] ; TODO Can the namespace symbol be made dynamic?
            (test/is (= (canon (f sqmsz rc)) expected) txt)
            (print (cljstr/join [ "    " funame " ✓\n" ]))))))

; ===
; main just runs the tests
; ===

(defn -main
  "Just run the tests"
  [& args]
  (test-success  0  [0 0]  []                         "Zero-size matrix, rowcol outside, #1")
  (test-success  0  [1 1]  []                         "Zero-size matrix, rowcol outside, #2")
  (test-success  1  [0 0]  []                         "One-size matrix, rowcol inside")
  (test-success  1  [1 0]  [[0 0]]                    "One-size matrix, rowcol outside")
  (test-success  5  [0 0]  [[0 1] [1 0]]              "Top left")
  (test-success  5  [1 0]  [[0 0] [1 1] [2 0]]        "Left edge")
  (test-success  5  [1 1]  [[0 1] [1 0] [1 2] [2 1]]  "Diagonal #1")
  (test-success  5  [2 2]  [[1 2] [2 1] [2 3] [3 2]]  "Diagonal #2")
  (test-success  5  [3 3]  [[2 3] [3 2] [3 4] [4 3]]  "Diagonal #3")
  (test-success  5  [4 4]  [[3 4] [4 3]]              "Bottom right")
  (test-success  5  [5 5]  []                         "Rowcol outside, #1")
  (test-success  5  [5 4]  [[4 4]]                    "Rowcol outside, #2")
  (test-success  5  [4 3]  [[3 3] [4 2] [4 4]]        "Bottom edge")

  ; Need some work for good reporting on these; they are supposed to fail
  ; TODO

  ;(test-success -1 [0 0]  []                        "Bad matrix size")
  ;(test-success  5 [0 0 0] []                       "Bad second arg")

  ; instrument the "neighbors-gen3" function, which means "spec/fdef neighbors-gen3"
  ; will be applied (but how?)
  ; TODO

  (instr/instrument `neighbors-gen3)

)
