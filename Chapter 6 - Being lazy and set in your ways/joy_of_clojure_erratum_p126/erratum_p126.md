In Chapter 6 of _"The Joy of Clojure"_, several paragraphs on p.126 highlight the differences between
`rest` and `next`, with `rest` being described as "very lazy", and `next` as "less lazy".

This is no longer the case in Clojure 1.10 (and probabyl earlier):

`next` is as conservative ("lazy") in "realizing" the next element hidden inside a `seq` as is `rest` (though I'm not fully in the clear on how this works).

Let's try it.

The function also has a more informative printout than just `.` as used in the book, it prints `i` enclosed in brackets:

    (doc iterate)
    ;=> Returns a lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects

    (def very-lazy (->
              (iterate #(do (print "[" % "]") (inc %)) 1)
              rest rest rest))

    ;=> [ 1 ][ 2 ]#'user/very-lazy

    (class very-lazy)
    ;=> clojure.lang.Iterate

    (seq? very-lazy)
    ;=> true

    ; the final "rest" is not yet run because we don't really need the result!
    ; ...but it is when we run "first"

    (first very-lazy)
    ;=> [ 3 ]4

And `next` has the same behaviour in this context:

    (def less-lazy (->
           (iterate #(do (print "[" % "]") (inc %)) 1)
           next next next))

    ;=> [ 1 ][ 2 ]#'user/less-lazy

    (first less-lazy)
    ;=> [ 3 ]4

