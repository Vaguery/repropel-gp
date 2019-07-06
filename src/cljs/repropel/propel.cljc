(ns repropel.propel
  (:require
    [clojure.string :as string]
    #?(:cljs [cljs.reader :refer [read-string]])
    ))


;; TO DO
;; - bring tournament-size into specification of tournament selection directly (like :parent-selection (tournament 5))



;; CLI defaults:
;; - :instructions default-instructions
;;   - [specified collection of symbols]
;; - :target-problem :simple-cubic
;;   - :simple-quadratic
;;   - :birthday-quadratic
;;   - :random-integer-data
;;   - :contains-T?
;;   - :contains-TA-or-AT?
;; :population-size 200
;; :max-generations 100
;; :max-initial-plushy-size 50
;; :step-limit 100
;; :parent-selection :tournament
;;   - :lexicase
;; :misbehavior-penalty 1000000
;; :tournament-size 5


; Instructions must all be either functions that take one Push state and return another or constant literals.


(def opens ; number of blocks opened by instructions (default = 0)
  {'exec_dup 1
   'exec_if 2})

;;;;;;;;;
;; Utilities

(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
   :boolean '()
   :input {}})

(defn abs
  "Absolute value."
  [x]
  (if (neg? x)
    (- x)
    x))

(defn push-to-stack
  "Pushes item onto stack in state"
  [state stack item]
  (update state stack conj item))

(defn pop-stack
  "Removes top item of stack."
  [state stack]
  (update state stack rest))

(defn peek-stack
  "Returns top item on a stack."
  [state stack]
  (if (empty? (get state stack))
    :no-stack-item
    (first (get state stack))))

(defn empty-stack?
  "Returns true if the stack is empty."
  [state stack]
  (empty? (get state stack)))

(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map of the form {:state :args}, where
  :state is the new state and :args is a list of args from the stacks. If there
  aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))

(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))

;;;;;;;;;
;; Instructions

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:in1 (:input state))))

(defn integer_+
  [state]
  (make-push-instruction
    state #?@(:clj [+'] :cljs [+]) [:integer :integer] :integer))

(defn integer_-
  [state]
  (make-push-instruction
    state #?@(:clj [-'] :cljs [-]) [:integer :integer] :integer))

(defn integer_*
  [state]
  (make-push-instruction
    state #?@(:clj [*'] :cljs [*]) [:integer :integer] :integer))

(defn integer_%
  [state]
  (make-push-instruction state
                         (fn [int1 int2]
                           (if (zero? int2)
                             int1
                             (quot int1 int2)))
                         [:integer :integer]
                         :integer))

(defn integer_=
  [state]
  (make-push-instruction state = [:integer :integer] :boolean))

(defn exec_dup
  [state]
  (if (empty-stack? state :exec)
    state
    (push-to-stack state :exec (first (:exec state)))))

(defn exec_if
  [state]
  (make-push-instruction state
                         #(if %1 %3 %2)
                         [:boolean :exec :exec]
                         :exec))

(defn boolean_and
  [state]
  (make-push-instruction state #(and %1 %2) [:boolean :boolean] :boolean))

(defn boolean_or
  [state]
  (make-push-instruction state #(or %1 %2) [:boolean :boolean] :boolean))

(defn boolean_not
  [state]
  (make-push-instruction state not [:boolean] :boolean))

(defn boolean_=
  [state]
  (make-push-instruction state = [:boolean :boolean] :boolean))

(defn string_=
  [state]
  (make-push-instruction state = [:string :string] :boolean))

(defn string_take
  [state]
  (make-push-instruction state
                         #(apply str (take %1 %2))
                         [:integer :string]
                         :string))

(defn string_drop
  [state]
  (make-push-instruction state
                         #(apply str (drop %1 %2))
                         [:integer :string]
                         :string))

(defn string_reverse
  [state]
  (make-push-instruction state
                         #(apply str (reverse %))
                         [:string]
                         :string))

(defn string_concat
  [state]
  (make-push-instruction state
                         #(apply str (concat %1 %2))
                         [:string :string]
                         :string))

(defn string_length
  [state]
  (make-push-instruction state count [:string] :integer))

(defn string_includes?
  [state]
  (make-push-instruction state clojure.string/includes? [:string :string] :boolean))


(def push-instruction-registry
  {'in1 in1
   'integer_+ integer_+
   'integer_- integer_-
   'integer_* integer_*
   'integer_% integer_%
   'integer_= integer_=
   'exec_dup exec_dup
   'exec_if exec_if
   'boolean_and boolean_and
   'boolean_or boolean_or
   'boolean_not boolean_not
   'boolean_= boolean_=
   'string_= string_=
   'string_take string_take
   'string_drop string_drop
   'string_reverse string_reverse
   'string_concat string_concat
   'string_length string_length
   'string_includes? string_includes?
   })

(def default-instructions
  (concat (keys push-instruction-registry)
    ['close
     0
     1
     10
     100
     789
     true
     false
     ""
     ; "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
     "A"
     "C"
     "G"
     "T"
     ]))

;;;;;;;;;
;; Interpreter

(defn cljc-throw
  [item]
  (throw
    (#?@(:clj [Exception.]
         :cljs [js/Error.])
      (str
        "Unrecognized Push instruction in program: "
        item))))

(defn push-instruction?
  [item]
  (contains? push-instruction-registry item))


(defn interpret-one-step
  "Takes a Push state and executes the next instruction on the exec stack."
  [state]
  (let [popped-state (pop-stack state :exec)
        first-instruction (first (:exec state))]

    (cond
      (push-instruction? first-instruction)
      ((get push-instruction-registry first-instruction) popped-state)
      ;
      (integer? first-instruction)
      (push-to-stack popped-state :integer first-instruction)
      ;
      (string? first-instruction)
      (push-to-stack popped-state :string first-instruction)
      ;
      (seq? first-instruction)
      (update popped-state :exec #(concat %2 %1) first-instruction)
      ;
      (or (= first-instruction true) (= first-instruction false))
      (push-to-stack popped-state :boolean first-instruction)
      ;
      :else
      (cljc-throw first-instruction)
      )))


(defn interpret-program
  "Runs the given problem starting with the stacks in start-state."
  [program start-state step-limit]
  (loop [state (assoc start-state :exec program :step 1)]
    (if (or (empty? (:exec state))
            (> (:step state) step-limit))
      state
      (recur (update (interpret-one-step state) :step inc)))))


(defn push-from-plushy
  "Returns the Push program expressed by the given plushy representation."
  [plushy]
  (let [opener? #(and (vector? %) (= (first %) 'open))] ;; [open <n>] marks opens
    (loop [push () ;; iteratively build the Push program from the plushy
           plushy (mapcat #(if-let [n (get opens %)] [% ['open n]] [%]) plushy)]
      (if (empty? plushy)       ;; maybe we're done?
        (if (some opener? push) ;; done with plushy, but unclosed open
          (recur push '(close)) ;; recur with one more close
          push)                 ;; otherwise, really done, return push
        (let [i (first plushy)]
          (if (= i 'close)
            (if (some opener? push) ;; process a close when there's an open
              (recur (let [post-open (reverse (take-while (comp not opener?)
                                                          (reverse push)))
                           open-index (- (count push) (count post-open) 1)
                           num-open (second (nth push open-index))
                           pre-open (take open-index push)]
                       (if (= 1 num-open)
                         (concat pre-open [post-open])
                         (concat pre-open [post-open ['open (dec num-open)]])))
                     (rest plushy))
              (recur push (rest plushy))) ;; unmatched close, ignore
            (recur (concat push [i]) (rest plushy)))))))) ;; anything else

;;;;;;;;;
;; GP


(defn make-random-plushy
  "Creates and returns a new plushy."
  [instructions max-initial-plushy-size]
  (repeatedly (rand-int max-initial-plushy-size)
              #(rand-nth instructions)))


(defn tournament-selection
  "Selects an individual from the population using a tournament."
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-error tournament-set)))


(defn lexicase-selection
  "Selects an individual from the population using lexicase selection."
  [pop argmap]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let
        [min-err-for-case
          (apply min
            (map #(nth % (first cases)) (map :errors survivors)))]
        (recur
          (filter
            #(= (nth (:errors %) (first cases)) min-err-for-case)
            survivors)
            (rest cases)
            )))))


(defn select-parent
  "Selects a parent from the population using the specified method."
  [pop argmap]
  (case (:parent-selection argmap)
    :tournament (tournament-selection pop argmap)
    :lexicase (lexicase-selection pop argmap)))


(defn crossover
  "Crosses over two individuals using uniform crossover. Pads shorter one."
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                 plushy-b
                 plushy-a)
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    (remove #(= % :crossover-padding)
            (map #(if (< (rand) 0.5) %1 %2)
                 shorter-padded
                 longer))))


(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the plushy) with some probability."
  [plushy instructions]
  (let [rand-code (repeatedly (inc (count plushy))
                              (fn []
                                (if (< (rand) 0.05)
                                  (rand-nth instructions)
                                  :mutation-padding)))]
    (remove #(= % :mutation-padding)
            (interleave (conj plushy :mutation-padding)
                        rand-code))))


(defn uniform-deletion
  "Randomly deletes instructions from plushy at some rate."
  [plushy]
  (remove (fn [x] (< (rand) 0.05))
          plushy))

(defn new-uuid
  "conditional UUID producer"
  []
  (str #?(:clj (java.util.UUID/randomUUID) :cljs (random-uuid))))

(defn new-individual
  "Returns a new individual produced by selection and variation of
  individuals in the population."
  [pop argmap]
  {:id (new-uuid)
   :plushy
   (let [prob (rand)]
     (cond
       (< prob 0.5) (crossover (:plushy (select-parent pop argmap))
                               (:plushy (select-parent pop argmap)))
       (< prob 0.75) (uniform-addition (:plushy (select-parent pop argmap))
                                       (:instructions argmap))
       :else (uniform-deletion (:plushy (select-parent pop argmap)))))})


(defn random-individual
  "Produce one random individual"
  [instructions max-size]
  (hash-map
    :id (new-uuid)
    :plushy
      (make-random-plushy instructions max-size)
      ))


(defn random-population
  "Produce a random population of the given size"
  [popsize instructions max-size]
  (repeatedly popsize #(random-individual instructions max-size)))


(defn score-sorted-population
  "Given a population and an error function, score the individuals (writing their scores and behaviors to them in the process) and sort by total-error"
  [population error-fxn args]
  (sort-by
    :total-error
    (map (partial error-fxn args) population)
    ))

;;;;;;;;;;;;;;;; persistence

(def population-atom (atom [])) ;; stores population between steps
(def args-atom (atom {})) ;; stores arg hash
(def pause-atom (atom false)) ;; intended to be overridden externally
(def counter-atom (atom 0)) ;; to manage generation limits on runs

;;;;;;;;;;;;;;;;;; reporting

(defn behavior-map
  [problem individual]
  (into (sorted-map) (zipmap (:args problem) (:behaviors individual))))

(defn report-generation
  "Reports information each generation."
  [pop generation]
  (let [best (first pop)]
    (println "-------------------------------------------------------")
    (println "               Report for Generation" generation)
    (println "-------------------------------------------------------")
    (print "Best plushy: ") (prn (:plushy best))
    (print "Best program: ") (prn (push-from-plushy (:plushy best)))
    (println "Best total error:" (:total-error best))
    (println "Best errors:" (:errors best))
    (println "Behavior of best:"
      (behavior-map (:training-function @args-atom) best))
    (println)
    ))


(defn report-starting-line
  [args] (println "Starting GP with args:" args))

;;;;;;;;;;;;;;;;;; search

(defn propel-setup!
  "Build an initial population using the specified arguments, placing it in the specified atom"
  [pop-atom popsize instructions max-size]
    (reset! pop-atom
      (random-population popsize instructions max-size)
      ))

(defn propel-population-step
  "Takes an existing population and a pile of arguments, and produces a next population of the same size, according to the specified parameters."
  [population argmap]
  (let
    [fxn (:fxn (:training-function argmap))
     errfxn (:error-function fxn)
     instructions (:instructions argmap)
     evaluated-pop (if (:errors (first population))
          population
          (score-sorted-population population errfxn argmap))]
    (repeatedly
      (:population-size argmap)
      #(new-individual evaluated-pop argmap)
      )))

(defn propel-gp!
  "Main GP loop, rewritten to use a population atom and a dotimes."
  [pop-atom arg-atom pause-atom counter-atom]

  (let [pop-size (:population-size @arg-atom)
        instructions (:instructions @arg-atom)
        max-plushy (:max-initial-plushy-size @arg-atom)
        max-gens (:max-generations @arg-atom)
        error-fxn (:error-function @arg-atom)]

    (report-starting-line @arg-atom)
    (propel-setup! pop-atom
                   pop-size
                   instructions
                   max-plushy)
    (while (and (not @pause-atom) (< @counter-atom max-gens))
      (let [evaluated-pop
             (if (:errors (first @pop-atom))
                @pop-atom
                (score-sorted-population @pop-atom error-fxn @arg-atom))]
        (report-generation evaluated-pop @counter-atom)
        (swap! counter-atom inc)
        (reset! pop-atom
          (repeatedly pop-size
                      #(new-individual evaluated-pop @arg-atom)
                      ))))))


(defn run-once
  "Run the program with the given initial state, until the step limit is reached."
  [program initial-state step-limit]
  (interpret-program program initial-state step-limit))


(defn program-behavior
  "Run the program with the specified input value (only one allowed), and return the item present at the top of the specified stack when done (or :no-stack-item if nothing is present there)."
  [program in-value behavior-stack step-limit]
  (peek-stack
    (run-once program
              (assoc empty-push-state :input {:in1 in-value})
              step-limit)
    behavior-stack
    ))

(defn behavior-vector
  "Produces an ordered collection of behavior values taken from the specified stack top, using the specified inputs"
  [program fxn limit]
  (let [inputs (:args fxn)
        behavior-stack (:behavior fxn)]
    (map
      (fn [i] (program-behavior program i behavior-stack limit))
      inputs
      )))

(defn absolute-errors
  "Produces elementwise absolute difference between two numerical vectors (of the same length), or the penalty if no value is returned in the second."
  [expected observed penalty]
  (map
    (fn [v1 v2]
      (if (= :no-stack-item v2)
        penalty
        (abs (- v1 v2))
        ))
    expected
    observed
    ))

(defn regression-error-function
  "Runs the individual's program over the input values specified by the problem, comparing the resulting behaviors (also specified in the :training-function) to the goals, and saving the behavior and error vectors, plus :total-error, in the individual. Returns an updated individual."
  [argmap individual]
  (let [program (push-from-plushy (:plushy individual))
        fxn (:training-function argmap)
        limit (:step-limit argmap)
        penalty (:misbehavior-penalty argmap)
        outputs (behavior-vector program fxn limit)
        desired (map (:fxn fxn) (:args fxn))
        errors (absolute-errors desired outputs penalty)]
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error (apply #?@(:clj [+'] :cljs [+]) errors))
           ))

(defn classification-errors
  "Produces 0 for each matching value, 1 for each mismatch, or the penalty if no value is returned at all. Basically Hamming Distance with a penalty for missing elements."
  [expected observed penalty]
  (map
    (fn [v1 v2]
      (if (= :no-stack-item v2)
        penalty
        (if (= v1 v2) 0 1)
        ))
    expected
    observed
    ))

(defn binary-classification-error-function
  "Runs the individual's program over the input values specified by the problem, comparing the resulting behaviors (also specified in the :training-function) to the goals, and saving the behavior and error vectors, plus :total-error, in the individual. Returns an updated individual."
  [argmap individual]
  (let [program (push-from-plushy (:plushy individual))
        fxn (:training-function argmap)
        limit (:step-limit argmap)
        penalty (:misbehavior-penalty argmap)
        outputs (behavior-vector program fxn limit)
        desired (map (:fxn fxn) (:args fxn))
        errors (classification-errors desired outputs penalty)]
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error (apply #?@(:clj [+'] :cljs [+]) errors))
           ))

;; example problems
;; some symbolic regression problems

(def simple-quadratic-demo
  "Target function: f(x) = 7x^2 - 20x + 13, over the range [-10,11), with the result on :integer"
  {:fxn #?(:clj (fn [x] (+' (*' 7 x x) (*' -20 x) 13))
           :cljs (fn [x] (+ (* 7 x x) (* -20 x) 13)))
   :args (range -10 11)
   :behavior :integer
   :error-function regression-error-function
   })


(def simple-cubic-demo
  "Target function: f(x) = x^3 + x + 3, over the range [-10,11), with the result on :integer"
  {:fxn #?(:clj (fn [x] (+' (*' x x x) x 3))
           :cljs (fn [x] (+ (* x x x) x 3)))
   :args (range -10 11)
   :behavior :integer
   :error-function regression-error-function
   })


(def birthday-args
  (take 10 (random-sample 0.01 (range))))

(def birthday-quadratic-demo
  "Target function: f(x) = 1964 - 11*x + 9x^2, over a random collection of integer arguments, with the result on :integer"
  {:fxn #?(:clj (fn [x] (+' (*' 9 x x) (*' -11 x) 1964))
           :cljs (fn [x] (+ (* 9 x x) (* -11 x) 1964)))
   :args birthday-args
   :behavior :integer
   :error-function regression-error-function
   })

(def random-integer-pairs
  (let [xs (sort (take 4 (shuffle (range -10 10))))]
    (reduce #(assoc %1 %2 (rand-int 100)) {} xs)))

 (def random-integer-demo
   "Target function: random integers in [0,100) mapped to a random collection of 4 integer arguments, with the result on :integer"
   {:fxn #(get random-integer-pairs % 0)
    :args (keys random-integer-pairs)
    :behavior :integer
    :error-function regression-error-function
    })

;; string classification problems

(def contains-T?-demo
  "Return true if the string contains at least one 'T' character, over the specified collection of inputs, with the result on :boolean"
  {:fxn (fn [s] (boolean (re-find #"T" s)))
   :args (sort ["GCG" "GACAG" "AGAAG" "CCCA" "GATTACA" "TAGG" "GACT"])
   :behavior :boolean
   :error-function binary-classification-error-function
   })

(defn random-string
  [letters max-size]
  (clojure.string/join
    (repeatedly
      (+ 2 (rand-int (- max-size 2)))
      #(rand-nth letters)
      )))

(def string-args
  (sort (repeatedly 30 #(random-string ["A" "C" "G" "T"] 10)))
  )

(def contains-TA-or-AT?-demo
  "Return true if the string contains substring 'TA' or 'AT', over a run-specific random sample of inputs, with the result on :boolean"
  {:fxn (fn [s] (or (boolean (re-find #"AT" s)) (boolean (re-find #"TA" s))))
   :args string-args
   :behavior :boolean
   :error-function binary-classification-error-function
   })


(def demo-problems
  "Convenience to enable CLI specification of problem by keyword rather than symbol name"
  {:simple-cubic simple-cubic-demo
   :simple-quadratic simple-quadratic-demo
   :birthday-quadratic birthday-quadratic-demo
   :random-regression random-integer-demo
   :contains-T? contains-T?-demo
   :contains-TA-or-AT? contains-TA-or-AT?-demo
   })

;;;;;;;;;;;;;;;;;;

(def default-args
  "Default argument hash, which is modified by a hash and CLI args."
   {:instructions default-instructions
    :target-problem :simple-cubic
    :population-size 200
    :max-initial-plushy-size 50
    :step-limit 100
    :parent-selection :tournament
    :misbehavior-penalty 1000000
    :tournament-size 5
    :max-generations 100
    })

(defn cljc-read-string
  [string]
  #?(:clj (read-string string)
     :cljs (cljs.reader/read-string string)
     ))

(defn parse-cli-args
  [arg-strings]
  (->> arg-strings
      (map cljc-read-string ,)
      (apply hash-map ,)
      ))


(defn update-derived-args
  [arg-hash]
  (let [demo (get demo-problems (:target-problem arg-hash) :UNKNOWN-PROBLEM)]
    (-> arg-hash
      (assoc , :training-function demo)
      (assoc , :error-function (:error-function demo))
      )))


(defn collect-the-args!
  "Omnibus function to merge all the arguments in play, and store them in the atom specified.

  If no optional arguments are passed in, the default-args result. The other named arguments are :cli-hash (for parsed arguments from a command line) and :override-hash (for a hash of arguments passed in programmatically). These are merged into default-args in that order. Finally, some necessary derived arguments are inserted."
  [arg-atom & {:keys [cli-hash override-hash] :or {cli-hash {} override-hash {}}}]
  (let [merged (merge default-args cli-hash override-hash)]
    (reset! arg-atom (update-derived-args merged))
    ))

;;;;;;;;;;;;;;

(defn repl-main
  "Provided as a simple hands-on check for somebody working in a cljs REPL. Accepts no arguments! Just to see that things are \"working\"..."
  []
  (collect-the-args! args-atom :override-hash {:max-generations 30})
  (propel-gp! population-atom
              args-atom
              pause-atom
              counter-atom)
  )

#?(:clj
    (defn -main
      "Runs propel-gp! from command line, giving it a map of arguments. Use function calls to 'propel-setup! and 'propel-population-step to walk through search using an external caller."
      [& cli-args]
      (binding [*ns* (the-ns 'propel.core)]
        (collect-the-args! args-atom
          :cli-hash (parse-cli-args cli-args))
        (println @args-atom)
        (propel-gp! population-atom
                    args-atom
                    pause-atom
                    counter-atom)
        )))
