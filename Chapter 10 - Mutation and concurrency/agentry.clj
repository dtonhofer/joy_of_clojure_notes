; From Joy of Clojure 2nd edition, page 240ff.
; "When to use agents"
; ---

(ns joy.agentry
   (:require [clojure.test :as test])
   (:import java.util.concurrent.Executors))

(comment

(def agent-x (agent ["Hello"]))

(send agent-x conj "World")

(defn slow-conj [coll item]
   (Thread/sleep 1000)
   (conj coll item))

(def agent-y (send agent-x slow-conj "World"))

; agent-y and agent-x are the same agent!

)

; Controlling I/O with an Agent

(def log-agent (agent 0))

(defn do-log [msg-id message]
   (println msg-id ":" message)
   (inc msg-id))

(defn avail-procs []
   (.availableProcessors (Runtime/getRuntime)))

(def thread-pool
   (Executors/newFixedThreadPool (+ (avail-procs) 2)))

(defn dothreads!
   [foo  {thread-count :threads times-count :times :or {thread-count 1 times-count 1}}]
   (dotimes [i thread-count]
      (.submit thread-pool
         (fn []
            (try
               (dotimes [j times-count] (foo)) ; execute parameterless foo
            (catch Throwable e (.println System/out (str "Thread " i ": " (.getMessage e)))))))))

(defn do-step [channel message]
   (Thread/sleep 1)
   (send-off log-agent do-log (str channel "/" message)))

(defn three-step [channel]
   (do-step channel "step 0")
   (do-step channel "step 1")
   (do-step channel "step 2")
   (do-step channel "step 3"))

(defn all-together-now []
   (dothreads! #(three-step "alpha") {})
   (dothreads! #(three-step "bravo") {})
   (dothreads! #(three-step "charlie") {}))


