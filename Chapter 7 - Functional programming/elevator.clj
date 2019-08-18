(ns joc.chapter7
   (:require [clojure.test :as test]))

; Elevator state machine / recognizer code from "Joy of Clojure" 2nd edition,
; page 161 (Listing 7.2).
; Load with (load-file "elevator.clj"). The test code will run at once.

(defn elevator [commands]
(letfn
   [(ff-open [[op &  r]]
	#(case op
	   :close  (ff-closed r)
	   :done   true
	   false))
    (ff-closed [[op & r]]
	#(case op
	   :open (ff-open r)
	   :up   (sf-closed r)
           false))
    (sf-closed [[op & r]]
        #(case op
           :down (ff-closed r)
           :open (sf-open r)
           false))
    (sf-open [[op & r]]
        #(case op
           :close (sf-closed r)
           :done  true
           false))]
   (trampoline ff-open commands)))

(test/is (not (elevator [:close :open :close :up :open :open :done]))) 
(test/is (elevator [:close :up :open :close :down :open :done]))

