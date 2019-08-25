; From Joy of Clojure 2nd edition, page 228.
; "3 x 3 chess board representation using Clojure refs".
; ---
; This code INTENTIONALLY has problems with "transactionality"
; in case several threads modify the board concurrently.
; ---
; This code pulls in the parts sprayed over the book into a
; single source file, tries to make things more readable with
; better naming and "let" blocks, and adds some printing.
;
; This should be written using records, really:
;
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

   ; 2 args, with hardcoded N/S/W/E "neighborhood cells"

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
; The "dothreads!" function from page 227. It launches a function "foo"
; concurrently using a Java Thread Pool.
; ===
; Run "thread-count" threads in "thread-pool", with each thread running
; function "foo" "times-count" times. "foo" is supposed to take the thread
; and the times index.
;
; If a thread dies with an exception, it does so silently, no printout.
; Here we catch any Throwable at the top and write the message out.
;
; (Parameters to dothreads! are passed through a map defined at call site,
;  unlike originally done in JoC, where a series of keyword-value pairs is
;  passed. https://tonsky.me/blog/readable-clojure/#dont-fall-for-expanded-opts)

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
               (dotimes [j times-count] (foo i j))
            (catch Throwable e (.println System/out (str "Thread " i ": " (.getMessage e)))))))))

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
            #(vec (for [cell %] (foo cell)))
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

; "get-king-moves" is a function taking a board cell [row col] and
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
; 3) It is passed the (dereferenced) "to-move" ref, which is destructured:
;    [:K [2 1]] [:k [0 1]]
; 4) It returns something like [:K [2 0]]

(defn choose-move-randomly
   [[[my-piece my-cell] [_ enemy-cell]]]
   (let [ acceptable-move?  #(good-move? % enemy-cell)        ; is a move acceptable given enemy-cell?
          potential-moves   (get-king-moves my-cell)          ; vector of potential moves given my-cell
          move-proposals    (shuffle potential-moves)        ; shuffled vector of potential moves 
          dest-cell         (some acceptable-move? move-proposals) ] ; what happens if there are none?
        [my-piece dest-cell]))

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

; If any of the assertion fail, the board is messed up due to failure in correctly
; synchronizing access by concurrent threads!
; The thread would die silently (not even a printout) after an Exception, so we need to 
; catch & print in "dothreads!"

(defn update-board [[piece-l dest-cell] [[piece-r src-cell] _]]
 
   (assert (= piece-l piece-r) 
           (str "piece of selected move and piece of next move must be equal but are not: "
                piece-l " vs. " piece-r))

   (let [dest-cell-ref (get-in board dest-cell)
         src-cell-ref  (get-in board src-cell)
         place         (fn [from to] to)]
    
      (assert
         (= @src-cell-ref piece-l)
         (str "piece on board and piece seleced must be equal but are not: piece "
              piece-l " vs. board " @src-cell-ref))

      (assert
         (= @dest-cell-ref :-)
         (str "dest cell " dest-cell " on board must be empty but is actually: " @dest-cell-ref))

      (alter dest-cell-ref place piece-l ) ; "piece-l" shall be at "dest-cell"
      (alter src-cell-ref  place :-      ) ; no piece shall be at "src-cell"
      (alter num-moves inc)))

; ---
; Update the array of pieces to move
; ---

(defn update-to-move [[piece-next cell-next :as next-move]]
   (if-let [[[piece-0 _] [piece-1 _]] @to-move]
      (do
         (assert 
              (not= piece-1 piece-next) ; TODO this needs refinment
              (str "piece of next move and piece of move before that must not be equal but are"))
         (alter to-move #(vector (second %) next-move)))
      ; else
      (assert false (str "to-move has incorrect structure: " @to-move))))

; Apply deref to every cell of the board, returning a standard board (for printing)

(defn raw-board []
   (board-map deref board)) 

; ---
; The function that is run in multiple threads, multiple times
; ---
; "make-move" is a originally parameterless function that can be passed to 
; "dothreads!" We add two optional parameters "th-num" ("thread number" starting from 0)
; and "ti-num" ("times number" starting from 0) to allow for printout.

(defn make-move [& [th-num ti-num]]
   (let [th-num (or th-num 0)
         ti-num (or ti-num 0)]
      (.println System/out (str "make-move for thread = " th-num ", times = " ti-num))
      ; ****
      ; Intentionally wrong:
      ; We are not doing everything in a single transaction, so this
      ; will not work if two threads change the refs concurrently!
      ; (The operation in the "let" is not originally dosync-ed; let's
      ; do this here, although it is not necessary as it only reads.
      ; Write operations w/o dosync however lead to exceptions:
      ; "No transaction running")
      ; ***
      (let [chosen-move (dosync (choose-move-randomly @to-move))]
         (dosync (update-board chosen-move @to-move)) ; checks invariants and throws if not upheld
         (dosync (update-to-move chosen-move))        ; checks invariants and throws if not upheld 
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

(defn onemove []
   (reset-board!)
   (make-move)
)

; Run multiple board-moves concurrently like this.
; Observe the board getting into bad shape and warnings being issued.

(defn run10 [] 
   (reset-board!)
   (dothreads! make-move { :threads 10 :times 10 })
)

; to shut down the thread pool:

(comment
   (.shutdownNow thread-pool)
)

; to get the number of active threads in the thread pool:

(comment
   (.getActiveCount thread-pool)
)


