; From Joy of Clojure 2nd edition, page 227
; "An example of dothreads function"
; Code modified to suit my tastes better.

(ns joy.mutation
   (:import java.util.concurrent.Executors))

; Get currently available processors from java.lang.Runtime
; https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/lang/Runtime.html#availableProcessors()

(defn avail-procs [] (.availableProcessors (Runtime/getRuntime)))

; Create thread pool with java.util.concurrent.Executors factory
; https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/util/concurrent/Executors.html#newFixedThreadPool(int)
; this returns an
; https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/util/concurrent/ExecutorService.html

(def thread-pool
   (Executors/newFixedThreadPool (+ (avail-procs) 2)))

; Run "threadc" threads in "thread-pool", which each thread
; running function "foo" "timesc" times.
; Note on parameter passing: "Don't fall for expanded opts"
; https://tonsky.me/blog/readable-clojure/#dont-fall-for-expanded-opts
; We use an explicitly passed map instead.

(defn dothreads!
   [foo
    { threadc :threads
      timesc  :times
      :or 
      { thread-c 1 times-c 1}}]
   ; Submit a parameterless function to the thread-pool repeatedly: 0...threadc-1 times.
   ; Each function calls "foo" timesc times.
   ; .submit takes a "Runnable" and returns a "Future"
   ; https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/util/concurrent/ExecutorService.html#submit(java.lang.Runnable)
   ; and a "Runnable" is an interface with the paramterless "run()" method.
   (dotimes [x threadc]
      (.submit thread-pool
         ; Alternative notations:
         ; #(dotimes [y timesc] (foo x y)))))
         (fn [] (dotimes [y timesc] (foo x y))))))

(dothreads!
   (fn [x y] 
      ; This results in messy output
      ; (println "thread:" x "count:" y)
      ; For some reason, printint to stdout is synchronized with this:
      (.println System/out (str "thread: " x " count: " y)))
   { :threads (* 2 (avail-procs)) :times 2 })


