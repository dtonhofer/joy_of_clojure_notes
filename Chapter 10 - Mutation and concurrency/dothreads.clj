; From Joy of Clojure 2nd edition, page 227
; "An example of dothreads function"
; Code modified to suit my tastes better.

(ns joy.mutation
   (:import java.util.concurrent.Executors))

; (use 'joy.mutation) on the REPL to access this stuff

; ---
; Get currently available processors from java.lang.Runtime
; ---
; https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/lang/Runtime.html#availableProcessors()

(defn avail-procs []
   (.availableProcessors (Runtime/getRuntime)))

; ---
; Create thread pool larger than the number of available processors
; ---
; Uses "java.util.concurrent.Executors" factory.
; https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/util/concurrent/Executors.html#newFixedThreadPool(int)
; this returns an
; https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/util/concurrent/ExecutorService.html

(def thread-pool
   (Executors/newFixedThreadPool (+ (avail-procs) 2)))

; ---
; dothreads!
; ---

; Run "threadc" threads in "thread-pool", which each thread
; running function "foo" "timesc" times.
; Note on parameter passing: "Don't fall for expanded opts"
; https://tonsky.me/blog/readable-clojure/#dont-fall-for-expanded-opts
; We use an explicitly passed map instead.

; Note! 
;
; Here I hit a problem because I had misspelled a variable, breaking everything,
; and Clojure just happily compiled w/o making any noise. Given the lack of 
; compile-time checking, a good linter becomes survivalware!!!

; Submit a parameterless function to the thread-pool: 
;   [0...threadc-1] times (the "x")
; The parameterless function calls "foo(x,y)":
;   [0...timesc-1] times (the "y")
; ".submit" takes a Java "Runnable" and returns a Java "Future". See
; https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/util/concurrent/ExecutorService.html#submit(java.lang.Runnable)
; A "Runnable" is an interface with the paramterless "run()" method, to which
; Clojure can fit the parameterless function.
 
(defn dothreads!
   [foo  {threadc :threads timesc :times :or {threadc 1 timesc 1}}]
   (dotimes [x threadc]
      (.submit thread-pool
         #(dotimes [y timesc] (foo x y)))))

; ---
; Run it
; ---
; If you run it as "clj dothreads.clj" the program won't 
; exit. One has to close the thread-pool first using (.shutdown thread-pool)

(do
   (dothreads!
      (fn [x y] 
         ; Using println as in (println "thread:" x "count:" y) results in messy output.
         ; But "System.out.println" happens to be synchronized. At least on OpenJDK, but 
         ; maybe not on other JVMs. This is nice! See also:
         ; https://stackoverflow.com/questions/9459657/is-multi-thread-output-from-system-out-println-interleaved
         (.println System/out (str "thread: " x " count: " y)))
      { :threads (* 2 (avail-procs)) :times 10 })
   (.shutdown thread-pool)
)

