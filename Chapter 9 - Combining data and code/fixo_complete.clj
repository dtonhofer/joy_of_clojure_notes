(ns joc.chapter9
   (:require [clojure.test :as test]))

; ***
; USAGE:
; ***
; Load with (load-file "fixo_complete.clj"). This will run the test code at once.

; ===
; Description
; ===
; Complete "extend-type with FIXO" code from "The Joy of Clojure", 2nd edition, page 213.
; Slightly modified 

; ===
; We have a binary tree based on records, holding a val and having left and right subtrees
; ===

(defrecord TreeNode [val left right])

; ===
; Function to build a tree
; ===
; "xconj" basically is insertion sort; inserts value v into tree t. 
; > The code in JoC is more compact; here, "explicited" with "get" for readability.

(defn xconj [t v]
   (cond
      (nil? t)            (TreeNode. v nil nil)
      (< v (get t :val))  (TreeNode. (get t :val)
                                     (xconj (get t :left) v)
                                     (get t :right))
      :else               (TreeNode. (get t :val)
                                     (get t :left)
                                     (xconj (get t :right) v))))

; ===
; Function to serialize a tree
; ===
; xseq with a let and separate lazy-seq on the leftward and rightward descent.

(defn xseq [t]
      (when (some? t)
         (let [ left   (get t :left)
                v      (get t :val)
                right  (get t :right)
                l-seq  (lazy-seq (xseq left ))
                r-seq  (lazy-seq (xseq right)) ]
            (concat l-seq [v] r-seq))))

; ===
; Protocol for "FIFO/FILO" (thus "FIXO")
; ===

(defprotocol FIXO
   (fixo-push [fixo value]) ; insert a thing
   (fixo-pop  [fixo])       ; remove a thing
   (fixo-peek [fixo]))      ; see what would be removed if you removed a thing

; ===
; Define all the FIXO protocol functions on TreeNode
; ===

(extend-type TreeNode
   FIXO
   (fixo-push [tree v]
      (xconj tree v))
   (fixo-peek [tree]
      (let [left (get tree :left)] 
         (if (some? left)
            (recur left)
            (get tree :val))))
   (fixo-pop [tree]
      (let [{v :val left :left right :right} tree]
         (if (some? left)
            (TreeNode. v (fixo-pop left) right) ; return tree with leftmost node removed
            right))))                           ; return tree with current node removed

; ===
; Define all the FIXO protocol functions on vector
; ===

(extend-type clojure.lang.IPersistentVector
   FIXO
   (fixo-push [vector v]
      (conj vector v))
   (fixo-peek [vector]
      (peek vector))
   (fixo-pop [vector]
      (pop vector)))

; ===
; In case fixo-push is used on nil, mint a new TreeNode
; ===

(extend-type nil
   FIXO
   (fixo-push [_ v]
      (TreeNode. v nil nil)))

; ===
; Testing
; ===

(defn n-times [n f] (apply comp (repeat n f)))

(let [ a-vector [3 5 2 4 6]
       a-tree   (reduce xconj nil a-vector) ]
 
   ; test multiple pops/peek of the same "a-tree"

   (doseq [[pop-count pop-result]
            { 0 [2 3 4 5 6]
              1   [3 4 5 6]
              2     [4 5 6]
              3       [5 6]
              4         [6]
              5         nil }]
      (let [ multi-pop   (n-times pop-count fixo-pop) 
             popped-tree (multi-pop a-tree) ]
         ; ok if popped-tree is nil
         (test/is (= (xseq popped-tree) pop-result))
         ; can't run if popped-tree is nil
         (when (some? popped-tree)
            (test/is (= (fixo-peek popped-tree) (first pop-result))))))

   ; test multiple pops/peek of the same "a-vector"

   (doseq [[pop-count pop-result]
            { 0 [3 5 2 4 6]
              1 [3 5 2 4]
              2 [3 5 2]
              3 [3 5]
              4 [3]
              5 nil }]
      (let [ multi-pop     (n-times pop-count fixo-pop) 
             popped-vector (multi-pop a-vector) ]
         ; ok if popped-vector is nil
         (test/is (= (seq popped-vector) pop-result))
         ; can't run if popped-vector is nil
         (when (some? popped-vector)
            (test/is (= (fixo-peek popped-vector) (last pop-result)))))))

