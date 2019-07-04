(ns repropel.core
  (:require
   [reagent.core :as r]
   [reagent.session :as session]
   [reitit.frontend :as reitit]
   [clerk.core :as clerk]
   [accountant.core :as accountant]
   [repropel.propel :as propel]))

;; -------------------------
;; Routes

(def router
  (reitit/router
   [["/" :index]
    ["/about" :about]]))

(defn path-for [route & [params]]
  (if params
    (:path (reitit/match-by-name router route params))
    (:path (reitit/match-by-name router route))))

(path-for :about)
;; -------------------------
;; Page components


(def population-atom (r/atom []))

(def arg-atom (r/atom {}))

(def counter-atom (r/atom 0))

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
      :override-hash {:target-problem :simple-cubic})
      (propel/propel-setup! pop-atom
                     (:population-size @arg-atom)
                     (:instructions @arg-atom)
                     (:max-initial-plushy-size @arg-atom))
      (score-pop! pop-atom arg-atom)
      ))

(reset-propel! population-atom arg-atom)

(defn just-the-errors
  [population]
  (clojure.string/join
    " "
    (map :total-error population))
    )

(defn argmap-view
  [arg-atom]
  [:h3 "Argmap:"]
  [:p
    {:style
      {:background-color "linen" :font-family :monospace}}
    (str @arg-atom)
    ])


(defn dude-list [items]
  [:ol
   (for [item items]
     ^{:key item} [:li (str (propel/push-from-plushy (:plushy item)) " => " (:total-error item))]
      )])


(defn population-view
  [pop-atom]
  [:h3 "Population:"]
  [:div
    {:style
      {:background-color "linen" :font-family :monospace}}
    [dude-list @pop-atom]
    ])
;
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
    (do
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

;
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
      [argmap-view arg-atom]
      [reset-button population-atom arg-atom]
      [step-button population-atom arg-atom]
      ; [loader]
      [show-counter]
      [population-view population-atom]
      ]]))


(defn about-page []
  (fn [] [:span.main
          [:h1 "About repropel"]]
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
