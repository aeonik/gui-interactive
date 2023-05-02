(ns gui-interactive
  (:require [cljfx.api :as fx]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [file-operations :as file-operations])
  (:import (org.apache.commons.io FileUtils FilenameUtils)))

;;TODO remove current-directory from state
;;TODO Add the tree to the state
(def *state
  (atom {:gravity  10
         :friction 0.4
         ;:current-directory (System/getProperty "user.home")
         :current-directory (.getCanonicalPath (clojure.java.io/file "resources"))
         ::expanded #{}
         ::tree {}}))

(println @*state)

;(defn update-tree-items [state id event]
;  (-> state
;      (update ::expanded (if event conj disj) id)
;      (cond-> (and event (not (get-in state [::tree id])))
;              (assoc-in [::tree id] (file-operations/fetch-subdirs id (clojure.string/join "/" id)))
;              (some #(= "dummy" (first %)) (get-in state [::tree id]))
;              (update-in [::tree id] #(filter (fn [item] (not= "dummy" (first item))) %)))))

(defmulti event-handler :event/type)

(defmethod event-handler ::set-friction [e]
  (swap! *state assoc :friction (:fx/event e)))

(defmethod event-handler ::set-gravity [e]
  (println e)
  (swap! *state assoc :gravity (:fx/event e)))

(defmethod event-handler ::on-expanded-changed [e]
  (println "Entered on-expanded-changed")
  (let [expanded-path (get e :id)
        is-expanded? (get e :fx/event)]
    (println e)
    (println expanded-path)
    (println is-expanded?)
    (if is-expanded?
      (do
        (println "Before conj to expanded:" @*state)
        (swap! *state update ::expanded conj expanded-path)
        (println "After conj to expanded:" @*state))
      (do
        (println "Before disj from expanded:" @*state)
        (swap! *state update ::expanded disj expanded-path)
        (println "After disj from expanded:" @*state)))))


;; For deebugging purposes, print the state to a text box
(defn state-text-area [state-atom]
  (pprint/pprint state-atom)
  {:fx/type  :text-area
   ;:text "test"
   :text (with-out-str (pprint/pprint @*state))
   ;:text     (fx/sub-val state-atom
   ;                  (fn [state] (pr-str state)))
   :editable false})

(defn simulate-step [{:keys [velocity y]} gravity friction]
  (let [new-velocity (* (- velocity gravity) (- 1 friction))
        new-y        (+ y new-velocity)]
    (if (neg? new-y)
      {:velocity (- new-velocity) :y 0}
      {:velocity new-velocity :y new-y})))

(defn chart-view [{:keys [gravity friction]}]
  {:fx/type :line-chart
   :x-axis  {:fx/type :number-axis
             :label   "Time"}
   :y-axis  {:fx/type :number-axis
             :label   "Y"}
   :data    [{:fx/type :xy-chart-series
              :name    "Position by time"
              :data    (->> {:velocity 0 :y 100}
                            (iterate #(simulate-step % gravity friction))
                            (take 100)
                            (map-indexed (fn [index {:keys [y]}]
                                           {:fx/type :xy-chart-data
                                            :x-value index
                                            :y-value y})))}]})
(defn list-files [path]
  (->> (file-seq (clojure.java.io/file path))
       (filter #(or (.isFile %) (.isDirectory %)))
       (map #(.getCanonicalPath %))))
(take 10 (list-files (System/getProperty "user.home")))

;;TODO - Modify create-file-tree-item to use recur instead
;;TODO - Need to make the tree expanded? stateful
(defn create-tree-item [file expanded?]
  (let [canonical-path (.getCanonicalPath file)
        is-dir (.isDirectory file)]
    {:fx/type :tree-item
     :value (str (FilenameUtils/getBaseName canonical-path))
     :expanded expanded?
     :on-expanded-changed (when is-dir {:event/type ::on-expanded-changed :id canonical-path :expanded expanded?})}))

(defn children-for-dir [file expanded?]
  (let [is-dir (.isDirectory file)]
    (cond
      (and is-dir (not expanded?)) (conj (map #(create-tree-item % false)
                                              (file-operations/subdirs-as-file-objects file))
                                         {:fx/type :tree-item})
      (and is-dir expanded?) (map #(create-tree-item % false)
                                  (file-operations/subdirs-as-file-objects file))
      :else nil)))

(defn create-file-tree-item [file expanded?]
  (assoc (create-tree-item file expanded?)
    :children (children-for-dir file expanded?)))


(take 10 (file-operations/list-file-names "/home/dave"))

;; Create file tree item for resources directory

(.getCanonicalPath (clojure.java.io/file "resources"))
(create-file-tree-item (clojure.java.io/file "resources") true)

;(create-file-tree-item  "/home/dave" false)
(defn file-tree-view [{:keys [current-directory expanded]}]
  (pprint/pprint (create-file-tree-item (clojure.java.io/file current-directory) true))
  {:fx/type :v-box
   :spacing 10
   :padding 10
   :children [{:fx/type :label
               :text    "Current directory:"}
              {:fx/type :label
               :text    current-directory}
              {:fx/type :tree-view
               :root (create-file-tree-item (clojure.java.io/file "resources") true)}]})


(defn slider-view [{:keys [min max value label event]}]
  {:fx/type  :v-box
   :children [{:fx/type :label
               :text    label}
              {:fx/type          :slider
               :min              min
               :max              max
               :value            value
               :on-value-changed {:event/type event}
               :major-tick-unit  max
               :show-tick-labels true}]})

(defn root-view [{{:keys [gravity friction current-directory expanded]} :state}]
  {:fx/type :stage
   :title "Aeonik's Excellent Adaptation Emporium"
   :showing true
   :scene   {:fx/type :scene
             :root {:fx/type  :v-box
                    :spacing  20
                    :children [{:fx/type  :h-box
                                :spacing  10
                                :children [{:fx/type file-tree-view
                                            :current-directory current-directory
                                            :expanded expanded}]}
                               {:fx/type  chart-view
                                :gravity  gravity
                                :friction friction}
                               {:fx/type   :h-box
                                :spacing   10
                                :alignment :center
                                :children  [{:fx/type slider-view
                                             :min     0
                                             :max     5
                                             :value   gravity
                                             :label   "Gravity"
                                             :event   ::set-gravity}
                                            {:fx/type slider-view
                                             :min     0
                                             :max     1
                                             :label   "Friction"
                                             :value   friction
                                             :event   ::set-friction}
                                            ]}
                               {:fx/type    state-text-area
                                :state-atom *state}
                               ]}
             }})


(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc (fn [state]
                                    {:fx/type root-view
                                     :state   state}))
    :opts {:fx.opt/map-event-handler event-handler}))

(fx/mount-renderer *state renderer)

(renderer)
(require 'cljfx.dev)
(cljfx.dev/help-ui)