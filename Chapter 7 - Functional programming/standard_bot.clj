(ns joc.chapter7.bot
   (:require [clojure.test :as test]))

; ***
; USAGE:
; ***
; Load with (load-file "standard_bot.clj"). This will run the test code at once
;
; Run the code that "moves a bot around at random 100 steps" by calling
; (joc.chapter7.bot/move-bot (joc.chapter7.bot/mint-bot 0 0 :north) 100)


; === 
; Description
; ===
; A little development on the "bot" function from "The Joy of Clojure", 2nd 
; edition, p.153. 
;
; This bot has additional features to print itself and change its bearing. It
; also uses a map to find what's "left" and "right" for any given bearing instead
; of doing awkward numeric index lookups (and thus being based on an implicit
; function). There is also some unit test code.
;
; The code is kept rather self-explanatory, so we don't use the compressed
; syntax for map access, instead using an explicit "get" instead. See also:
; https://tonsky.me/blog/readable-clojure/
;
; 2019-05-22, 2019-05-26, revised 2019-08-17
; ===

; ===
; Pretty much self-explanatory map of bearings
; ===
;
;     y ^
;       |      N
;       |   W--+--E
;       |      S
;       +--------------> x
  
(def bearings {
    :north  { :dx  0 , :dy  1 , :left :west  , :right :east  }
    :east   { :dx  1 , :dy  0 , :left :north , :right :south }
    :south  { :dx  0 , :dy -1 , :left :east  , :right :west  }
    :west   { :dx -1 , :dy  0 , :left :south , :right :north } })

(defn b-left-of  [bearing] (get (get bearings bearing) :left))
(defn b-right-of [bearing] (get (get bearings bearing) :right))
(defn b-deltas   [bearing] (let [{dx :dx dy :dy} (get bearings bearing)] [dx dy]))

; ===
; A "bot" (which represents state) is "minted" by the function "mint-bot".
;
; > In JoC, the "mint-bot" function is called "bot". This seems inappropriate as
; > it is not "thingy" like a map or a record. It should be named with a verb. 
; > "mint-bot" sounds good, as we "mint" new bots (actually new dispatch functions)
;
; The returned structure is a map. The map stores "OO methods", keyed by
; method name (or "OO message"), each of them a closure that has closed 
; over the values passed to mint-bot, so having accessible information about
; the bot state at "mint-bot" call time in their context. This automatically
; makes the bot state completely private to other functions.
;
; In Scheme, that map would be a function: the "dispatch function, responsible
; for calling a method depending on message. See for example:
; http://people.cs.aau.dk/~normark/prog3-03/html/notes/oop-scheme_themes-classes-objects-sec.html
; We could do the same in Clojure, but in Clojure, a map is an avatar of a 
; function so returning a map instead of a function is a good solution.
;
; Note that the dispatch function is rather wild; its domain is ill-defined
; and so is it codomain. Not really what the word "function" would evoke.
;
; To avoid having "map retrieve and method call" pieces of code inlined
; and make clear that "OO message sending" is going on, an explicit "send-msg"
; function has been added.
;
; The map (or dispatch function) should really contain only functions (only
; return functions) for uniformity (as opposed to storing OO fields, too for
; example). This will keep the "send-msg" function simple. 
; === 

(defn mint-bot [x y bearing]

   { :pre [ (int? x) (int? y)
            (contains? #{:north :east :south :west} bearing) ]}

   ; Return the dispatch function, or rather an avatar of it in the form of a map
   ; Note the "bot-state-change" functions (their keys are marked with unicode 0x25C6,
   ; black diamond). They all call "mint-bot", thus returning new dispatch 
   ; functions based on new state.

   { :get-coords   (fn [] [x y])
     :get-bearing  (fn [] bearing)
     :to-string    (fn [] (str "⟦" [x y] " " bearing "⟧")) 
     :◆nop         (fn [] (mint-bot x y bearing))
     :◆new-bearing (fn [newb] (mint-bot x y newb))
     :◆turn-left   (fn [] (mint-bot x y (b-left-of  bearing)))
     :◆turn-right  (fn [] (mint-bot x y (b-right-of bearing)))
     :◆forward     (fn [] 
                     (let [[dx dy] (b-deltas bearing)]
                          (mint-bot (+ x dx) (+ y dy) bearing)))})

; ===
; Sending an OO message to the bot (i.e. call its dispatch function)
; Very polymorphic as it must be able to accept all kinds of values and
; return all kins of values.
; ===

(defn send-msg [bot msg & params]
   ; for extreme "duck typing" able to accept dispatch functions both in
   ; the map and function form, one would use the syntax:
   ; (let [method (bot msg)] ...
   (let [method (get bot msg)]
      (if (some? method)
          (apply method params)              ; call method with params
          (println "No such method:" msg)))) ; else protest & return nil
               
; ===
; Generate a random bearing. This is evidently not a pure function
; as we pull data from some kind of oracle.
; ===

(def random-bearing 
   (fn [] (get [:north :east :south :west] (int (rand 4)))))

; ===
; Move a bot around randomly for "count" steps. 
; ===
; A "step" means: change bearing randomly, then move forward one position.
; Run like this:
; (joc.chapter7.bot/move-bot (joc.chapter7.bot/mint-bot 0 0 :north) 100)

(defn move-bot [bot count]
   (if (< 0 count)
      ; step the bot and recur
      ; syntax is explicit here; one can code-golf it later
      (let [ bot+  (send-msg bot  :◆new-bearing (random-bearing))
             bot++ (send-msg bot+ :◆forward) ]
        (println (send-msg bot :to-string) "---->" (send-msg bot++ :to-string))
        (recur bot++ (dec count)))
      ; else return bot as is
      bot))

; ===
; Testing
; ===

; "test-bot" sends (parameterless) "msg" to "bot". The "msg" is supposed to 
; activate a method returning a new bot.
; That new bot's coordinates and bearing are then compared to the passed
; "shall-coords" and "shall-bearing".
; In the end, the new bot is returned.

(defn test-bot [ bot [msg shall-coords shall-bearing] ]
    { :pre [ (map? bot) 
             (contains? #{:◆nop :◆turn-left :◆turn-right :◆forward} msg) ]}
    (let [bot+ (send-msg bot msg)] ; 
       (println (send-msg bot :to-string)  "-->" msg "-->" (send-msg bot+ :to-string))
       (test/is (send-msg bot+ :get-bearing) shall-bearing)
       (test/is (send-msg bot+ :get-coords)  shall-coords)
       bot+))

; Mint a new bot as "current bot" then perform something like 
;   current-bot := (test-bot "current bot" "current test data from array")
; repeatedly using "reduce"

(reduce test-bot
    (mint-bot 0 0 :north) 
    [[:◆nop        [0 0]   :north]
     [:◆turn-right [0 0]   :east]
     [:◆turn-left  [0 0]   :north]
     [:◆turn-left  [0 0]   :west]
     [:◆forward    [-1 0]  :west]
     [:◆turn-left  [-1 0]  :south]
     [:◆forward    [-1 -1] :south] ])

