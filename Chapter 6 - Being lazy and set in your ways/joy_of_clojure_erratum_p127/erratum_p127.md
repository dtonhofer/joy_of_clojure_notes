In *"The Joy of Clojure"*, page 127 , we write a function which creates an imbricated sequence from something
that is `seq`-able.

For example the vector '[:a :b :c]' shall be transformed into the sequence '(:a (:b (:c [])))'

This is done lazily so long input sequences can be processed while avoiding stack overflow, which would occur
if the function where doing recursive calls in a naive fashion.

So we have this function. It is not tail-recursive. It uses `lazy-seq` to generate an unevaluated
"suspension" (is that right word for a structure can be further evaluated a bit later?) which
(if I understand correctly) is basically a closure of the body enclosed by `lazy-seq` with `s` appropriately bound
(finally: "thinking in code" instead of "thinking in compiler-issued blobs")

Originally:

    (defn lz-rec-step [s]
       (lazy-seq
          (if (seq s)
                [(first s) (lz-rec-step (rest s))]
                [])))

If have added a few print statements to get some visual feedback:

    (defn lz-rec-step [s]
       (lazy-seq
          (if (seq s)
             (do
                (print "\u2207")
                [(first s) (lz-rec-step (rest s))])
             (do
                (print "\u0394")
                []))))

So, a deep recursive call taking stack space is transformed into a shallow call that returns
the `lazy-seq` "suspension" to the top level control loop. The stack frames are transformed
into structures on the heap.

Here is an example of the iterative evaluation of the `laz-seq` suspensions for `(lz-rec-step [:a :b :c])`,
emitting printed deltas and nablas:

![lz-rec-step](https://user-images.githubusercontent.com/483879/60338272-0be14400-9995-11e9-865c-90507561779e.png)

And indeed on the command line, we can test this:

Full evaluation because the REPL needs to print out the sequence:

    (lz-rec-step [:a :b :c])
    ;=> (∇:a (∇:b (∇:c (Δ))))

Using `first` just needs one evaluation:

    (first (lz-rec-step [:a :b :c]))
    ;=> ∇:a

Or we can assign the result to `_` to see how many evaluations we need:

    (def _ (lz-rec-step [:a :b :c]))
    ;=> #'user/_

    (def _ (first (lz-rec-step [:a :b :c])))
    ∇#'user/_

    (def _ (first (second (lz-rec-step [:a :b :c]))))
    ∇∇#'user/_

    (def _ (first (second (second (lz-rec-step [:a :b :c])))))
    ∇∇∇#'user/_

    (def _ (first (second (second (second (lz-rec-step [:a :b :c]))))))
    ∇∇∇Δ#'user/_

Now for some larger input:

    (lz-rec-step (range 10))
    (∇0 (∇1 (∇2 (∇3 (∇4 (∇5 (∇6 (∇7 (∇8 (∇9 (Δ)))))))))))

All is well. At the end, the book says that running

    (dorun (lz-rec-step (range 10000)))

"no longer produces stack overflow". But this an error: `dorun` is supposed to traverse a lazy sequence
to the end and return `nil` ... and this case, the sequence has length 2, so we are done very quickly.

Indeed:

    (dorun (lz-rec-step (range 10000)))
    ;=> ∇nil

We need another function!

    (defn follow [s]
       (if (seq s)
           (recur (second s))
           nil))

Now it is working:

     (follow (lz-rec-step (range 10000)))
     ;=> ....∇∇∇∇∇∇∇∇∇∇∇Δnil

Maybe not surprisingly:

    (def x (lz-rec-step (range 10000)))
    (println x)

causes `Execution error (StackOverflowError) at (REPL:1)`. I suppose the stack overflow
actually happens in `println` as it tries to follow the imbricated structure down 10'000 steps.

The non-tail-recursive, eager, naively recursive version generates a stack overflow, of course:

    (defn rec-step [[x & xs]]  ; destructure 1 parameter into x & xs
       (if x
          (do
             (print "\u2207")
             [x (rec-step xs)])
          (do
             (print "\u0394")
             [])))

    (rec-step [:a :b :c])
    ;=> ∇∇∇Δ[:a [:b [:c []]]]

And

    (rec-step (range 10000))

will not work (although the REPL may not properly show the exception)
