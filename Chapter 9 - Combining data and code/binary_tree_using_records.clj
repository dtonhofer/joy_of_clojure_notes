(ns joc.chapter9
   (:require [clojure.test :as test]))

; Run using (load-file "binary_tree_using_records.clj")
; The test code is run at once!

; ===
; Based on code in Chapter 9.3 (page 206ff) of "Joy of Clojure", 2nd edition
; -> Programmed out
; -> Added test code
; -> Generally made code more agreeable (to me at least)
; ===

; ===
; We have a binary tree based on records, holding a val and having left
; and right subtrees ... and we are using "records" (technically, immutable
; Java Beans created on an ad-hoc basis)
; ===

(defrecord TreeNode [val left right])

; ===
; Building the tree
; ===
; "xconj" basically is insertion sort; inserts value "v" into tree "t".
; The code in JoC is more compact; here, "explicited" for readability.
; See also https://tonsky.me/blog/readable-clojure/

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
; Serializing the tree into a seq
; ===
; Convert a tree into a seqs (in-order traversal, so the seq will spit
; out the integers in order sorted ascending).

; ---
; Original version (code made less compact though)
; ---

; This function returns a "lazy seq" as "concat" returns "clojure.lang.LazySeq"
; But to make the sequence really lazy and avoid evaluation of the arguments
; at construction time, one has to use "lazy-cat" instead.

(defn xseq-orig [t]
   (when (some? t)
      (concat (xseq-orig (get t :left))
              [ (get t :val) ]
              (xseq-orig (get t :right)))))

; ---
; Version with explicit naming and a "println"
; ---

(defn xseq [t]
   (when (some? t)
      (let [ left  (get t :left)
             val   (get t :val)
             right (get t :right) ]
         (println left val right)
         (concat (xseq left) [val] (xseq right)))))

; ---
; Version with aggressive printing
; ---

; "xseq" is a bit mute; add some printout to probe behaviour (watching
; out to not destroy laziness when doing so) In the printout,
; "([3]◀[2]▶ ▼ ⊥" for example means "found 3, went left, found 2, went
; right, now at nil"

(defn xseq-p1 [t k]
   (if (nil? t) (println k "▼" "⊥") (println k "▼" (get t :val)))
   (when (some? t)
      (concat (xseq-p1 (get t :left) (str k "[" (get t :val) "]" "◀"))
              [ (get t :val) ]
              (xseq-p1 (get t :right) (str k "[" (get t :val) "]" "▶")))))

; ---
; Version with additionaly lazy-seq around the returned result.
; ---

; This behaves "lazily" as can be seen in the printout; the orginal version
; of xseq does not!

(defn xseq-p2 [t k]
   (if (nil? t) (println k "▼" "⊥") (println k "▼" (get t :val)))
   (when (some? t)
      (lazy-seq
         (concat (xseq-p2 (get t :left) (str k "[" (get t :val) "]" "◀"))
                 [ (get t :val) ]
                 (xseq-p2 (get t :right) (str k "[" (get t :val) "]" "▶"))))))

; ---
; Version with a let and separate lazy-seq on the leftward and rightward descent.
; ---

; This behaves "lazily" as can be seen in the printout; the orginal version
; of xseq does not!

(defn xseq-p3 [t k]
   (if (nil? t) (println k "▼" "⊥") (println k "▼" (get t :val)))
   (when (some? t)
      (let [ left   (get t :left)
             v      (get t :val)
             right  (get t :right)
             l-seq  (lazy-seq (xseq-p3 left  (str k "[" v "]" "◀")))
             r-seq  (lazy-seq (xseq-p3 right (str k "[" v "]" "▶"))) ]
         (concat l-seq [v] r-seq))))

; ===
; Testing
; ===

; create a tree for testing

(def a-tree (reduce xconj nil [3 5 2 4 6]))

(test/is (= [2 3 4 5 6] (xseq-p1 a-tree "")))
(test/is (= [2 3 4 5 6] (xseq-p3 a-tree "")))

; xseq-p1 is NOT lazy:

; (first (joc.chapter9/xseq-p1 joc.chapter9/a-tree ""))
; => ▼ 3
; => [3]◀ ▼ 2
; => [3]◀[2]◀ ▼ ⊥
; => [3]◀[2]▶ ▼ ⊥
; => [3]▶ ▼ 5
; => [3]▶[5]◀ ▼ 4
; => [3]▶[5]◀[4]◀ ▼ ⊥
; => [3]▶[5]◀[4]▶ ▼ ⊥
; => [3]▶[5]▶ ▼ 6
; => [3]▶[5]▶[6]◀ ▼ ⊥
; => [3]▶[5]▶[6]▶ ▼ ⊥
; => 2

; xseq-p3 is lazy:

; (first (joc.chapter9/xseq-p3 joc.chapter9/a-tree ""))
; => ▼ 3
; => [3]◀ ▼ 2
; => [3]◀[2]◀ ▼ ⊥
; => 2


