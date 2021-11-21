(ns patate.core
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            ["react-latex-next" :as Latex]
            [patate.polynom :as p]
            [patate.util :refer [seq-indexed]]))

(defn make-serie [n-exercises degree1 degree2]
  (-> (for [index (range n-exercises)]
        (let [poly1 (p/create-poly degree1)
              poly2 (p/create-poly degree2)
              poly-product (p/poly-poly-mul poly1 poly2)]
          {:id           (inc index)
           :poly1        poly1
           :poly2        poly2
           :poly-product poly-product}))
      vec))

(defonce state
  (r/atom
    {:selected-serie-id :basic-serie
     :serie-ids         [:basic-serie
                         :easy-serie
                         :normal-serie
                         :hard-serie]
     :basic-serie       (make-serie 5 1 1)
     :easy-serie        (make-serie 5 1 2)
     :normal-serie      (make-serie 5 2 2)
     :hard-serie        (make-serie 5 3 3)}))

(def latex (r/adapt-react-class Latex))

(def light-pink "#ffd0d0")
(def solution-color "#fea")
(def event-step-color "#ddf")
(def odd-step-color "#eaeaff")

(defn root []
  (let [selected-serie-id (-> @state :selected-serie-id)]
    [:main
     [:h1 "Polynom Division"]
     [:div
      (for [serie-id (-> @state :serie-ids)]
        ^{:key serie-id}
        [:button {:style (when (= serie-id selected-serie-id)
                           {:background-color "yellow"})
                  :on-click (fn [_]
                              (swap! state assoc :selected-serie-id serie-id))}
         (-> serie-id name (str/replace "-" " ") str/capitalize)])]

     [:div
      (for [[index exercise] (seq-indexed (-> @state (get selected-serie-id)))]
        ^{:key (:id exercise)}
        [:div
         "Exercise " (:id exercise) ":"
         [latex (str "$$"
                     "\\frac{"
                     (-> exercise :poly-product p/poly->str)
                     "}{"
                     (-> exercise :poly1 p/poly->str)
                     "}"
                     "$$")]

         (let [solution-shown (:solution-shown exercise false)]
           [:div
            [:div
             [:button {:style {:background-color solution-color}
                       :on-click (fn [_]
                                   (swap! state update-in
                                          [selected-serie-id index :solution-shown]
                                          not))}
              (if solution-shown "Hide Solution" "Show Solution")]]
            (when solution-shown
              [:div
               [:table.step-table
                [:tbody
                 [:tr {:style {:background-color solution-color}}
                  [:td [latex (str "$"
                                   (-> exercise :poly2 p/poly->str)
                                   "$")]]]]]])])

         (let [steps-shown (:steps-shown exercise false)]
           [:div
            [:div
             [:button {:style    {:background-color event-step-color}
                       :on-click (fn [_]
                                   (swap! state update-in
                                          [selected-serie-id index :steps-shown]
                                          not))}
              (if steps-shown "Hide Steps" "Show Steps")]]
            (when steps-shown
              [:div
               [:table.step-table
                [:tbody
                 (for [[index step] (seq-indexed (p/poly-poly-div-steps (:poly-product exercise)
                                                                        (:poly1 exercise)))
                       :let [[partial-result-poly poly-to-sub poly-rest] step
                             step-color (if (even? index) event-step-color odd-step-color)]]
                   ^{:key index}
                   [:<>
                      [:tr {:style {:background-color step-color}}
                       [:td {:align "right"} "result: "]
                       [:td [latex (str "$" (p/poly->str partial-result-poly) "$")]]]
                      [:tr {:style {:background-color step-color}}
                       [:td {:align "right"} "to subtract: "]
                       [:td [latex (str "$" (p/poly->str poly-to-sub) "$")]]]
                      [:tr {:style {:background-color step-color}}
                       [:td {:align "right"} "remaining: "]
                       [:td [latex (str "$" (p/poly->str poly-rest) "$")]]]])]]])])
         [:hr]])]]))

(defn render []
  (rdom/render [root] (js/document.getElementById "app")))

(defn init []
  (println "(init)")
  (render))

(defn ^:dev/before-load stop []
  (println "(stop)"))

(defn ^:dev/after-load start []
  (println "(start)")
  (render))
