(ns repropel.core
  (:require
   [reagent.core :as r]
   [reagent.session :as session]
   [reitit.frontend :as reitit]
   [clerk.core :as clerk]
   [accountant.core :as accountant]
   [repropel.propel :as propel]
   [reagent-forms.core :as rf]
   [cljs.core.async :as async]
   [goog.string :as gstring]
   [goog.string.format]
   ))

;; -------------------------
;; Routes

(def router
  (reitit/router
   [["/" :index]
    ["/dude/:uuid" :individual]
    ["/about" :about]
    ]))

(defn path-for [route & [params]]
  (if params
    (:path (reitit/match-by-name router route params))
    (:path (reitit/match-by-name router route))))

;; -------------------------
;; Page components


(defonce population-atom (r/atom []))
(defonce arg-atom (r/atom {}))
(defonce pause-atom (r/atom false))
(defonce counter-atom (r/atom 0))


;;;;;; arg view

(defn row [label input]
  [:div
    [:span input]
    [:label label]
    ])


(def fancy-args-template
  [:div
    [:p "Population size"]
      [:input {:field :numeric :id :population-size}]

    ; [:p "Max initial Plushy size"]
    ;   [:input {:field :numeric :id :max-initial-plushy-size}]

    [:p "Target Problem"]
      (row "Simple cubic regression" [:input {:field :radio :value :simple-cubic :name :target-problem :checked true}])
      (row "Simple quadratic regression" [:input {:field :radio :value :simple-quadratic :name :target-problem}])
      (row "Birthday quadratic regression" [:input {:field :radio :value :birthday-quadratic :name :target-problem}])
      (row "Random regression" [:input {:field :radio :value :random-regression :name :target-problem}])
      (row "Contains-T?" [:input {:field :radio :value :contains-T? :name :target-problem}])
      (row "Contains-TA-or-AT?" [:input {:field :radio :value :contains-TA-or-AT? :name :target-problem}])
      (row "Contains-CC-or-AT?" [:input {:field :radio :value :contains-CC-or-AT? :name :target-problem}])

    [:p "Parent selection"]
      (row "Tournament" [:input {:field :radio :value :tournament :name :parent-selection :checked true}])
      (row "Lexicase" [:input {:field :radio :value :lexicase :name :parent-selection}])
      ])




(defn fancy-argmap-ui
  [arg-atom]
  (fn []
    [:div {:style {:background-color "linen" :font-family :monospace}}
      [:h2 "Argmap:" ]
      [rf/bind-fields
        fancy-args-template
        arg-atom
        (fn [id path value {:keys [target-problem] :as args}]
          (when (= id :target-problem)
            (merge @arg-atom (propel/update-derived-args @arg-atom))))
            ]
      [:p (str @arg-atom)]
      ]))


(defn argmap-view
  [arg-atom]
  [:div {:style {:background-color "linen" :font-family :monospace}}
    [:h2 "Argmap:" ]
    [:p (str @arg-atom)]
    ])


;;;;;;;;;

(defn score-pop!
  [pop-atom arg-atom]
  (swap!
    pop-atom
    #(sort-by
      :total-error
      (propel/score-sorted-population
        %
        (:error-function @arg-atom)
        @arg-atom
        ))))

(defn reset-propel!
  [pop-atom arg-atom]
  (do
    (propel/collect-the-args!
      arg-atom
      :override-hash {:target-problem :simple-cubic :population-size 100 :misbehavior-penalty +1e10})
      (propel/propel-setup! pop-atom
                     (:population-size @arg-atom)
                     (:instructions @arg-atom)
                     (:max-initial-plushy-size @arg-atom))
      (score-pop! pop-atom arg-atom)
      (reset! counter-atom 0)
      ))

(reset-propel! population-atom arg-atom)

(defn short-id
  [uuid]
  (str (first (clojure.string/split (str uuid) #"-")) "...")
  )

(defn dude-list [dudes]
  [:ol
   (for [dude dudes]
     (let [uuid (str (:id dude))]
     ^{:key uuid}
      [:li
        [:a {:href (path-for :individual {:uuid uuid})}
        (short-id (:id dude))
        ]
        " => "

        (:total-error dude)
        " : "
        (str (propel/push-from-plushy (:plushy dude)))
        ]))])


(defn population-view
  [pop-atom]
  [:div
    [:h3 "Population:"]
    [:div
      {:style
        {:background-color "linen" :font-family :monospace}}
      [dude-list @pop-atom]
      ]])

(defn behavior-view
  [dude]
  (let [b (seq (propel/behavior-map (:training-function @arg-atom) dude))
        e (:errors dude)
        both (map conj b e)]
    [:ul
      (for [i both]
        ^{:key i}
        [:li
          [:pre
            (gstring/format
              "input: %s produces '%s\n(error: %d)"
              (first i)
              (second i)
              (last i)
            )]])]))

(defn reset-button
  [pop-atom arg-atom]
  [:button
    {:on-click #(reset-propel! pop-atom arg-atom)}
    "Reset!"
    ])

(defn loader
  []
  [:div.loader]
  )

(defn forward-and-score
  [pop-atom arg-atom]
      (when-not @pause-atom
        (swap! pop-atom #(propel/propel-population-step % @arg-atom))
      (score-pop! pop-atom arg-atom)
      (swap! counter-atom inc)
      ))

(defn step-button
  [pop-atom arg-atom]
  [:button
    {:on-click
      #(forward-and-score pop-atom arg-atom)}
    "Step!"
    ])

(defn step-N
  [pop-atom arg-atom gens]
  (dotimes [g gens]
    (forward-and-score pop-atom arg-atom)
    ))

(defn step-10-button
  [pop-atom arg-atom]
  [:button
    {:on-click
      #(step-N pop-atom arg-atom 10)}
    "Step 10x!"
    ])

(defn pause-button
  [pauser]
  (row "Paused?"
    [:input.toggle
      {:type "checkbox"
      :checked @pause-atom
      :on-change #(swap! pause-atom not)}
      ]))

(reset-propel! population-atom arg-atom)


(defn show-counter
  []
  [:span ": " @counter-atom])


;; PAGE definitions

(defn home-page []
  (fn []
    [:span.main
      [:h1 "Welcome to repropel"]
      [:div
        [fancy-argmap-ui arg-atom]
        [reset-button population-atom arg-atom]
        [step-button population-atom arg-atom]
        [step-10-button population-atom arg-atom]
        [pause-button pause-atom]
        [show-counter]
        [population-view population-atom]
        ]]))


(defn about-page []
  (fn [] [:span.main
          [:h1 "About repropel"]]
          ))

(defn dude-page []
  (fn []
    (let [routing-data (session/get :route)
          uuid (get-in routing-data [:route-params :uuid])
          dude (first (filter #(= (str (:id %)) uuid) @population-atom))
          push (propel/push-from-plushy (:plushy dude))]

      [:span.main
       [:h2 "Individual"]
       [:code (str uuid)]

       [:h3 "Genome"]
       [:code (str (:plushy dude))]

       [:h3 "Program"]
       [:code (str push)]

       [:h3 "Behavior"]
       [behavior-view dude]
       ])))

;; -------------------------
;; Translate routes -> page components

(defn page-for [route]
  (case route
    :index #'home-page
    :about #'about-page
    :individual #'dude-page
    ))


;; -------------------------
;; Page mounting component

(defn current-page []
  (fn []
    (let [page (:current-page (session/get :route))]
      [:div
       [:header
        [:p [:a {:href (path-for :index)} "Home"] " | "
         [:a {:href (path-for :about)} "About repropel"]]]
       [page]
       [:footer
        [:p "That was it"]
        ]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (clerk/initialize!)
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (let [match (reitit/match-by-path router path)
            current-page (:name (:data  match))
            route-params (:path-params match)]
        (r/after-render clerk/after-render!)
        (session/put! :route {:current-page (page-for current-page)
                              :route-params route-params})
        (clerk/navigate-page! path)
        ))
    :path-exists?
    (fn [path]
      (boolean (reitit/match-by-path router path)))})
  (accountant/dispatch-current!)
  (mount-root))
