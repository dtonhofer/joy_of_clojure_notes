(ns joc.chapter9
   (:require [clojure.test :as test]))

; ===
; Based on "Listing 9.4" on page 215 of "Joy of Clojure", 2nd edition
; -> Fixed BUG
; -> Added test code
; -> Generally made code mor agreeable (to me at least)
; ===

(defprotocol FIXO
   (fixo-push   [fixo value]) ; insert a thing ("value") into a fixo
   (fixo-pop    [fixo])       ; remove a thing from a fixo, returning the new fixo
   (fixo-peek   [fixo])       ; see what would be removed if you removed a thing from a fixo
   (fixo-vec    [fixo])       ; returns a vector representation of FIXO
   (fixo-empty? [fixo]))      ; returns true on empty

; ===
; Minting fixed-length fixos
; ===
; "mint-fixed-fixo" is a function with two signatures:
; 
; 1) an outer "factory call" (fixed-fixo limit)
; 2) an inner "factory call" (fixed-fixo limit vector)
;
; The inner factory call returns a thing which can be manipulated by the FIXO
; prototype's functions.
;
; It has inner state based on: 
;
; - an initially empty vector for the initial reification.
; - whatever the vector is on subsequent reifications.
;
; Technically, this is could be implemented as an anonymous object implementing
; the interface of FIXO (fast, mapped to Java), or a system-wide dispatch table into
; which an entry for [concrete-fixo-thing method-name method-implemented] is dynamically 
; added and removed (slow, flexible).

(defn mint-fixed-fixo

   ; Outer "factory call" function

   ([limit] (mint-fixed-fixo limit []))

   ; Inner "factory call" function.
   ; It creates a "thing" that can be manipulated by FIXO functions.
   ; It "re-ifies" the FIXO protocol.
   ;
   ; The returned "thing" is associated with the given method implementations
   ; in a global dispatch table (maybe, this is implementation dependent,
   ; but one can imagine this to be the case).
   ;
   ; The method implementations are closures over the values passed
   ; at reification time.
   ;
   ; So the "this" value is not needed, we have already access to the
   ; all the contextual stuff we need!
   ;     

   ([limit vek]
      (reify FIXO

         (fixo-push [this value]
            (if (< (count vek) limit)
               (mint-fixed-fixo limit (conj vek value)) ; factory call if processing ok
               this))                              ; return unchanged otherwise

         ; peeking an empty vector yields nil

         (fixo-peek [_]        ; we don't care about whatever the "this" fixo is
            (peek vek))        ; ...because we can use vek passed at creation time 

         ; Popping en empty vector leads to exception;
         ; We are more lenient and return nil instead (alternative, one could 
         ; just return "this")

         ; ** BUG Originally on p.215:
         ; ** (fixo-pop [_]
         ; **    (pop vector))
         ; ** This is not going to work as we need to return "fixed fixo", not a vector!!

         (fixo-pop [_]         ; we don't care about whatever the "this" fixo is
            (when (not (empty? vek))
               (mint-fixed-fixo limit (pop vek))))
 
         (fixo-vec [_]         ; we don't care about whatever the "this" fixo is
            vek)

         (fixo-empty? [_]
            (empty? vek)))))

; ===
; Testing
; ===

(defn operate [maxlen thing ops]

   (when-let [[op data & more] ops]
      (println "Operation" op "on"
         (if (some? thing) 
            (str (fixo-vec thing) " " (type thing))
            "*nil*"))
      (case op
         :mint
            (let [newthing (mint-fixed-fixo maxlen data)] 
               (test/is (= data (fixo-vec newthing)) 
                        (str "The thing build should be " data))
               (operate maxlen newthing more))
         :pop
            (let [[shall-vek shall-peek] data
                  newthing (fixo-pop thing)]
               (test/is (= shall-peek (fixo-peek thing))
                        (str "Peeking at " (fixo-vec thing) " should give " shall-peek))
               (if (nil? shall-vek)
                  (test/is (nil? newthing)
                           "The thing resulting from pop should be nil")
                  ; else
                  (test/is (= shall-vek (fixo-vec newthing))
                           (str "The thing resulting from pop should be " shall-vek)))
               (operate maxlen newthing more))
         :push
            (let [[pval shall-vek] data
                  newthing (fixo-push thing pval)]
               (test/is (= shall-vek (fixo-vec newthing))
                        (str "The thing resulting from push should be " shall-vek))
               (operate maxlen newthing more)))))
 
(operate 5 nil [
     :mint   [5 6 0 1 8]       ; mint a fixo thing from [5 6 0 1 8]
     :pop    [[5 6 0 1] 8]     ; pop a fixo thing, expecting [5 6 0 1] and 8
     :pop    [[5 6 0] 1]       ; etc...
     :pop    [[5 6] 0]
     :pop    [[5] 6]
     :pop    [[] 5]
     :pop    [nil nil]         ; pop the empty thing, expecting nil and nil
     :mint   []                ; mint from empty 
     :push   [1 [1]]           ; push 1, expecting [1]
     :push   [2 [1 2]]         ; etc...
     :push   [3 [1 2 3]]
     :push   [4 [1 2 3 4]]
     :push   [5 [1 2 3 4 5]]
     :push   [6 [1 2 3 4 5]]])

