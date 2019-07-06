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
   ))

;; -------------------------
;; Routes

(def router
  (reitit/router
   [["/" :index]
    ["/about" :about]]
    ))

(defn path-for [route & [params]]
  (if params
    (:path (reitit/match-by-name router route params))
    (:path (reitit/match-by-name router route))))

(path-for :about)
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
      :override-hash {:target-problem :simple-quadratic :population-size 100 :misbehavior-penalty +1e10})
      (propel/propel-setup! pop-atom
                     (:population-size @arg-atom)
                     (:instructions @arg-atom)
                     (:max-initial-plushy-size @arg-atom))
      (score-pop! pop-atom arg-atom)
      (reset! counter-atom 0)
      ))

(reset-propel! population-atom arg-atom)


(defn dude-list [items]
  [:ol
   (for [item items]
     ^{:key (:id item)}
      [:li
        (str (propel/push-from-plushy (:plushy item))
              " => "
              (:total-error item)
              " : "
              (propel/behavior-map (:training-function @arg-atom) item)
              )])])

(defn population-view
  [pop-atom]
  [:div
    [:h3 "Population:"]
    [:div
      {:style
        {:background-color "linen" :font-family :monospace}}
      [dude-list @pop-atom]
      ]])


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
    (async/go
      (when-not @pause-atom
        (swap! pop-atom #(propel/propel-population-step % @arg-atom))
      (score-pop! pop-atom arg-atom)
      (swap! counter-atom inc))
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

(defn home-page []
  (let [])
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

(defn dude-page [dude]
  (fn [] [:span.main
          [:h1 "Individual " (:id dude) ]]
          ))

;; -------------------------
;; Translate routes -> page components

(defn page-for [route]
  (case route
    :index #'home-page
    :about #'about-page
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
