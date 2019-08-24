; From Joy of Clojure 2nd edition, page 235, where
; the code is fixed for correct "transactionality".
; "3 x 3 chess board representation using Clojure refs"

; This should be written using records, really.
; There is not enough structure in arguments to preclude confusion.

(ns joy.mutation
   (:require [clojure.test :as test])
   (:import java.util.concurrent.Executors))

; ===
; The "neighbors" function from page 95.
; ===

; Here, we need its 3-arg implementation as we consider another
; neighborhood than [[-1 0] [1 0] [0 -1] [0 1]]

(defn neighbors

   ([size yx]
       (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))

   ([deltas size yx]
      (filter (fn [new-yx]
                 (every? #(< -1 % size) new-yx))
              (map #(vec (map + yx %))
                   deltas))))

; ===
; The "dothreads" function from page 227
; ===
; Run "threadc" threads in "thread-pool", which each thread running
; function "foo" "timesc" times. Parameters are passed through a 
; map defined at call site (unlike originally done in JoC)

(defn avail-procs []
   (.availableProcessors (Runtime/getRuntime)))

(def thread-pool
   (Executors/newFixedThreadPool (+ (avail-procs) 2)))

(defn dothreads!
   [foo  {threadc :threads timesc :times :or {threadc 1 timesc 1}}]
   (dotimes [x threadc]
      (.submit thread-pool
         #(dotimes [y timesc] 
            ; (.println System/out (str "thread: " x " count: " y))
            (foo x y)))))

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
; 
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

; 1) The code in the book is hard to read; let's name things.
; 2) This function uses "shuffle" to generate non-determinism.
;    It should really be called "choose-move-randomly"!
; 3) It is passed the (dereferenced) "to-move" ref, which is destructured:
;    [:K [2 1]] [:k [0 1]]
; 4) It returns something like [:K [2 0]]

(defn choose-move-rnd
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
; "place" is a function passed to "alter" to modify board state to "to"
; ---


; 1) Renamed "move-piece" to "update-board", which is really what happens.
; 2) Added assert to check what's passed in.
; 3) The "place" function can be defined locally if we have a "let".

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
    
      ; if any of the conditions below are true, the board is messed up!
      ; if we do asserts here, the thread dies silently; instead just print for illustrative purposes
       
      (when
         (not= @src-cell-ref piece-l)
         (println (str "piece on board and piece seleced must be equal: piece " piece-l " vs. board " @src-cell-ref)))

      (when
         (not= @dest-cell-ref :-)
         (println (str "dest position on board must be empty but is: " @dest-cell-ref)))

      ; as this function is called in a dosync, we can alter safely

      (alter dest-cell-ref place piece-l ) ; piece at destination
      (alter src-cell-ref  place :-      ) ; no piece at src
      (alter num-moves inc)))

; Update the array of pieces to move

(defn update-to-move [next-move]
   (alter to-move #(vector (second %) next-move)))

; Apply deref to every cell of the board, returning a standard board!

(defn raw-board []
   (board-map deref board)) 

; "make-move" is a originally parameterless function that can be passed to 
; "dothreads!" Our "dothreads!" calls this with the counter values [thread times]

(defn make-move [x y]
   ; (println "make-move thread =" x "times =" y)
   (let [chosen-move (dosync (choose-move-rnd @to-move))] ; if you don't dosync this, all the threads lock up!
      ; we are not doing everything in a single transaction, so this
      ; wont work if two threads change the refs concurrently
      (dosync (update-board chosen-move @to-move))
      (dosync (update-to-move chosen-move)) 
      (dosync (.println System/out (raw-board)))))

; ---
; Manual testing
; ---

(comment
   (reset-board!)
   (make-move 0 0)
   (board-map deref board)
)

(comment
   (reset-board!)
   (dothreads! make-move { :threads 100 :times 100 })
   (board-map deref board)
)

(comment
   (load-file "chess.clj")
   (use 'joy.mutation)
)

