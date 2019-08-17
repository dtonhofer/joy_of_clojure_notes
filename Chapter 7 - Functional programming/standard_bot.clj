(ns joc.chapter7.bot
   (:require [clojure.test :as test]))

; load with (load-file "standard_bot.clj"); this will run the test at once

; === 
; A little development on the "bot" function from "The Joy of Clojure", 2nd 
; edition, p.153. 
;
; This bot has additional features to print itself and change
; the bearing. It also uses a map to find what's "left" and "right" for any 
; bearing instead of doing awkward numeric index lookups. There is also some 
; unit test code.
;
; 2019-05-22, 2019-05-26, revised 2019-08-17
; ===

(def bearing-map {
    :north  { :x  0 , :y  1 , :left :west  , :right :east  }
    :east   { :x  1 , :y  0 , :left :north , :right :south }
    :south  { :x  0 , :y -1 , :left :east  , :right :west  }
    :west   { :x -1 , :y  0 , :left :south , :right :north } })

; ===
; "State" is represented by a function. This function generates a new function
; representing new state whenever the state is being stepped. 
;
; The function evidently has "inner state" (i.e. is parameterized). The inner
; state, together with parameters passed to it during state stepping flows into
; the "inner state" of the new function generated.
;
; In JoC, the function is called "bot", but this does not seems appropriate as
; it is not "thingy" like a map or a record. It should be named with a verb. 
; "mint-bot" sounds good, as we "mint" new bots (actually new "mint-bot"
; functions).
; === 

(defn mint-bot [x y bearing]

   ; Check passed parameters

   { :pre [ (int? x) 
            (int? y)
            (contains? #{:north :east :south :west} bearing) ]}

   ; Return a map holding the equivalent of:
   ; 1) OO style members/fields/attributes/properties:
   ;    These can be accessed directly in the "bot" map (But *modifying* them
   ;    means creating a new map; and in that case the modified values are
   ;    out of step with the values closed-over in the closures also stored
   ;    in the map! So don't do that!)
   ;    For fun, we mark these with ◆ (unicode 0x2526)
   ; 2) OO style methods
   ;    These close over the parameters passed to "mint-bot". This is their
   ;    sole parametrization: They cannot access anything what would be a 
   ;    "this" in OO style. 
   ;    The "state-change" functions create & return a new "bot" map by
   ;    calling "mint-bot".

   { :◆coords     [x y]

     :◆bearing    bearing

     :string      (fn [] ; use fn to avoid eval at construction time
                     (str "<" [x y] " " bearing ">")) 

     :new-bearing (fn [newb] 
                     (mint-bot x y newb))

     :nop         (fn [] 
                     (mint-bot x y bearing))

     :forward     (fn [] 
                     (let [ {dx :x dy :y} (get bearing-map bearing)
                             x-next (+ x dx)        
                             y-next (+ y dy) ]
                          (mint-bot x-next y-next bearing)))

     :turn-left   (fn []
                     (mint-bot x y (get (get bearing-map bearing) :left)))

     :turn-right  (fn []
                     (mint-bot x y (get (get bearing-map bearing) :right)))})


; ===
; Generate a random bearing. This is evidently not a pure function
; as we literally pull data from an oracle.
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
      (let [ bot+  ((get bot :new-bearing) (random-bearing))
             bot++ ((get bot+ :forward)) ]
        (println ((get bot :string)) "---->" ((get bot++ :string)))
        (recur bot++ (dec count)))
      ; else stop, returning bot
      bot))

; ===
; Testing
; ===

(defn test-bot [ bot [op expc-coords expc-bearing] ]
    (comment (println op expc-coords expc-bearing))
    ; operation + test call; mint a next bot, test it, return it
    (let [bot+ ((get bot op))]
       (println ((get bot :string)) "-->" op "-->" ((get bot+ :string)))
       (test/is (get bot+ :◆bearing) expc-bearing)
       (test/is (get bot+ :◆coords)  expc-coords)
       bot+))

(reduce test-bot (mint-bot 0 0 :north) 
                 [ [:nop         [0 0]   :north]
                   [:turn-right  [0 0]   :east]
                   [:turn-left   [0 0]   :north]
                   [:turn-left   [0 0]   :west]
                   [:forward     [-1 0]  :west]
                   [:turn-left   [-1 0]  :south]
                   [:forward     [-1 -1] :south] ])



