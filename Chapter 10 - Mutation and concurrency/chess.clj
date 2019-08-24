; From Joy of Clojure 2nd edition, page 228
; "3 x 3 chess board representation using Clojure refs"
; ---
; This code intentionally has problems with "transactionality"
; in case several threads modify the board concurrently.
; ---
; This code pulls in the parts sprayed over the book into a
; single source file, tries to make things more with better
; naming and "let" blocks, and adds some printing.
;
; This should be written using records, really:
; There is not enough structure/naming in arguments to 
; preclude confusion or lengthy cogitation of what the
; intention of the code and structures is.
; ---

(ns joy.mutation
   (:require [clojure.test :as test])
   (:import java.util.concurrent.Executors))

; ===
; The "neighbors" function originally from page 95.
; Modified for readability.
; ===

; Here, we need the 3-arg implementation as we consider another
; neighborhood than [[-1 0] [1 0] [0 -1] [0 1]]

; sqmsz: The size of the square matrix to consider, an integer >= 0.
; rc   : A sequence of "row" and "column" giving the cell in whose neighborhood
;        we are interested.

(defn neighbors

   ; 2 args, with hardcoded "neighborhood cells"

   ([sqmsz rc]

        (neighbors [[-1 0] [1 0] [0 -1] [0 1]]
                   sqmsz
                   rc))

   ; 3 args, whereby you have to pass the vector of "neighborhood cells"

   ([deltas sqmsz rc]
        (let [ in-sq-matrix?     (fn [x]        (and (<= 0 x) (< x sqmsz)))
               in-sq-matrix-rc?  (fn [rc]       (every? in-sq-matrix? rc))
               add-two-rc        (fn [rc1 rc2]  (vec (map + rc1 rc2)))
               get-rc-neighbors  (fn [rc]       (map (partial add-two-rc rc) deltas)) ]
           (filter in-sq-matrix-rc? (get-rc-neighbors rc)))))

; ===
; The "dothreads" function from page 227.
; Modified for readability.
; ===
; Run "threadc" threads in "thread-pool", with each thread running
; function "foo" "timesc" times. Parameters are passed through a 
; map defined at call site (unlike originally done in JoC, where 
; an open-ended series of keyword value pairs are passed)
; See https://tonsky.me/blog/readable-clojure/#dont-fall-for-expanded-opts

(defn avail-procs []
   (.availableProcessors (Runtime/getRuntime)))

(def thread-pool
   (Executors/newFixedThreadPool (+ (avail-procs) 2)))

(defn dothreads!
   [foo  {thread-count :threads times-count :times :or {thread-count 1 times-count 1}}]
   (dotimes [i thread-count]
      (.submit thread-pool
         #(dotimes [j times-count] 
            ; (.println System/out (str "thread: " i " count: " j))
            (foo i j)))))

; ===
; Chess problem
; ===

; ---
; Basic 3x3 chess board
; ---

(def initial-board
   [[:-  :k  :-]    ; lowercase k: black king
    [:-  :-  :-]
    [:-  :K  :-]])  ; uppercase K: white king

; Map "foo" to every cell of the board, creating a new board full of refs

(defn board-map [foo board]
   (vec (map 
            #(vec (for [pos %] (foo pos)))
            board)))

; ---
; Define mutable state consisting of refs and collections of refs
; ---
; The "to-move" ref holds a vector of (at least) two records: "what piece 
; shall move next and where is that piece now". As this game has 
; alternating turns, the color of the pieces must alternate, too.

(defn reset-board!
   "Resets the board state. Generally these types of functions are a
    bad idea, but matters of page count force our hand."
   []
   (def board     (board-map ref initial-board))  ; board representation (array of array of refs)
   (def to-move   (ref [[:K [2 1]] [:k [0 1]]]))  ; pieces-to-move representation; next :K at [2 1] will move!
   (def num-moves (ref 0)))                       ; count the moves

; ---
; Moving
; ---

; "get-king-moves" is a function taking a board position (cell) and
; returning a sequence of neighboring cells. 
; "get-king-moves" sounds better than JoC's original "king-moves".
; For example:
; (get-king-moves [0 0]) 
; ;=> ([0 1] [1 0] [1 1])

(def get-king-moves
   (partial neighbors
      [[-1 -1] [-1 0] [-1 +1]
       [ 0 -1]        [ 0 +1]
       [+1 -1] [+1 0] [+1 +1]] 3))

; Check whether a move to "to" is possible if enemy is on "enemy-sq"
; This is the case of "to" is unoccupied

(defn good-move? [to enemy-sq]
   (when (not= to enemy-sq)
      to))

; 1) The code in the book is hard to read; let's use "let" and name things!
; 2) This function uses "shuffle" to generate non-determinism.
;    Instead of "choose-move", let's call it "choose-move-randomly".
;    Or we could put a ♠ symbol at the end of the name.
; 3) It is passed the (dereferenced) "to-move" ref, which is destructured:
;    [:K [2 1]] [:k [0 1]]
; 4) It returns something like [:K [2 0]]

(defn choose-move-randomly
   "Randomly choose a legal move"
   [[[my-piece my-pos] [_ enemy-pos]]]
   (let [ acceptable-move?  #(good-move? % enemy-pos)        ; is a move acceptable given enemy-pos?
          potential-moves   (get-king-moves my-pos)          ; vector of potential moves given my-pos
          move-proposals    (shuffle potential-moves)        ; shuffled vector of potential moves 
          dest-pos          (some acceptable-move? move-proposals) ]
        [my-piece dest-pos]))

; ---
; Manual testing of "choose-move-randomly": just generating random moves
; ---

(comment
   (reset-board!)
   (take 5 (repeatedly #(choose-move-randomly @to-move))))

;=> ([:K [1 1]] [:K [2 2]] [:K [2 0]] [:K [1 2]] [:K [1 0]])

; ---
; Updating board state
; ---
; 1) Renamed "move-piece" to "update-board", which is really what happens.
; 2) Added assert to check what's passed in.
; 3) The "place" function, passed to "alter" to modify board state to "to",
;    can be defined locally if we have a "let". Let's do that.

; "update-board" is called like this:
; (move-piece [:K [1 2]]          ; the piece move
;             [[:K [1 1]] ... ]   ; the to-move vector
; the pieces should correspond, but that is not checked...
; "move-piece" should be called "update-board", which
; is really what happens.

(defn update-board [[piece-l dest-pos] [[piece-r src-pos] _]]

   (assert (= piece-l piece-r) 
           (str "piece of selected move and piece of next move must be equal: " piece-l " vs. " piece-r))

   (let [dest-cell-ref (get-in board dest-pos)
         src-cell-ref  (get-in board src-pos)
         place         (fn [from to] to)]
    
      ; If any of the conditions below are true, the board is messed up due to failure in correctly
      ; synchronizing access by concurrent threads!
      ; If we put asserts here, the thread failing the assert dies silently (not even a printout)!
      ; Does that happen in Java??
      ; Thus just print for illustrative purposes.
       
      (when
         (not= @src-cell-ref piece-l)
         (println (str "piece on board and piece seleced must be equal: piece " piece-l " vs. board " @src-cell-ref)))

      (when
         (not= @dest-cell-ref :-)
         (println (str "dest position on board must be empty but is: " @dest-cell-ref)))

      ; as function "update-board" is called inside a "dosync", we can alter safely

      (alter dest-cell-ref place piece-l ) ; "piece-l" shall be at "dest-cell"
      (alter src-cell-ref  place :-      ) ; no piece shall be at "src-cell"
      (alter num-moves inc)))

; Update the array of pieces to move

(defn update-to-move [next-move]
   (alter to-move #(vector (second %) next-move)))

; Apply deref to every cell of the board, returning a standard board (for printing)

(defn raw-board []
   (board-map deref board)) 

; "make-move" is a originally parameterless function that can be passed to 
; "dothreads!" We add two optional parameters "thnum" (thread number starting from 0)
; and "tinum" (times number starting from 0) to allow for printout.

(defn make-move [& [thnum tinum]]
   (let [thnum (or thnum 0)
         tinum (or tinum 0)]
      ; (.println System/out (str "make-move for thread = " thnum ", times = " tinum))
      ; ****
      ; We are not doing everything in a single transaction, so this
      ; will not work if two threads change the refs concurrently!
      ; ***
      (let [chosen-move (dosync (choose-move-randomly @to-move))] ; if you forget to "dosync" this, all the threads lock up!
         (dosync (update-board chosen-move @to-move))
         (dosync (update-to-move chosen-move)) 
         (dosync (.println System/out (raw-board))))))

; ---
; Manual testing
; ---

; Load code in this file like this:

(comment
   (load-file "chess.clj")
   (use 'joy.mutation)
)

; Run one board-move like this. "make-move" prints at the end

(comment
   (reset-board!)
   (make-move)
)

; Run multiple board-moves concurrently like this.
; Observe the board getting into bad shape and warnings being issued.

(comment
   (reset-board!)
   (dothreads! make-move { :threads 100 :times 100 })
)


