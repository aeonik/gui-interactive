(ns gui-interactive
  (:require [cljfx.api :as fx]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [file-operations :as file-operations]
            )
  (:import (org.apache.commons.io FileUtils FilenameUtils)))


;; you will use custom logic here to determine if it's a prod or dev,
;; e.g. by using a system property: (Boolean/getBoolean "my-app.dev")
(def in-development? true)
(use 'clojure.tools.trace)
(trace-ns gui-interactive)

;;TODO remove current-directory from state
;;TODO Add the tree to the state
(def *state
  (atom {:gravity           10
         :friction          0.4
         ;:current-directory (System/getProperty "user.home")
         :current-directory (.getCanonicalPath (clojure.java.io/file "resources"))
         :expanded          #{}
         ::tree             {}}))

(println @*state)

;(defn update-tree-items [state id event]
;  (-> state
;      (update ::expanded (if event conj disj) id)
;      (cond-> (and event (not (get-in state [::tree id])))
;              (assoc-in [::tree id] (file-operations/fetch-subdirs id (clojure.string/join "/" id)))
;              (some #(= "dummy" (first %)) (get-in state [::tree id]))
;              (update-in [::tree id] #(filter (fn [item] (not= "dummy" (first item))) %)))))

(defmulti handle :event/type)

(defmethod handle ::on-expanded-changed [{:keys [id fx/event]}]
  (swap!
    *state
    #(-> %
         (update ::expanded (if event conj disj) id)
         (cond-> (and event (not (get-in % [::tree id])))
                 (assoc-in [::tree id]
                           ;; This is "lazy loading". Note: swap! fn might be retried,
                           ;; don't do side effects here in your app
                           (random-sample 0.5 (map conj (repeat id) (range 5))))))))
(defmulti event-handler :event/type)

(defmethod event-handler ::set-friction [e]
  (swap! *state assoc :friction (:fx/event e)))

(defmethod event-handler ::set-gravity [e]
  (println e)
  (swap! *state assoc :gravity (:fx/event e)))

;(defmethod event-handler ::on-expanded-changed [e]
;  (println "Entered on-expanded-changed")
;  (let [expanded-path (get e :id)
;        is-expanded?  (get e :fx/event)]
;    (println e)
;    (println expanded-path)
;    (println is-expanded?)
;    (if is-expanded?
;      (swap! *state update :expanded conj expanded-path)
;      (swap! *state update :expanded disj expanded-path)
;        )))


;; For deebugging purposes, print the state to a text box
(defn state-text-area [state-atom]
  (pprint/pprint state-atom)
  {:fx/type  :text-area
   ;:text "test"
   :text     (with-out-str (pprint/pprint @*state))
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

;;TODO - Need to make the tree expanded? stateful

(defn is-path-expanded? [file state-atom]
  (let [canonical-path (.getCanonicalPath file)
        expanded-paths (get @state-atom :expanded)]
    (contains? expanded-paths canonical-path)))

(defn create-tree-item
  "Creates a tree item for the given file. Takes the file and a boolean flag expanded? as arguments."
  [file]
  (let [canonical-path (.getCanonicalPath file)
        is-dir         (.isDirectory file)]
    {:fx/type             :tree-item
     :value               (str (FilenameUtils/getBaseName canonical-path))
     :expanded            (is-path-expanded? file *state)
     :on-expanded-changed (when is-dir {:event/type
                                        ::on-expanded-changed
                                        :id canonical-path})}))



(defn child-tree-items
  "Generates child tree items for the given file.
  Returns the child items as a sequence, or nil if the file is not a directory."
  [file]
  (let [is-dir (.isDirectory file)]
    (when is-dir
      (map #(create-tree-item %)
           (file-operations/subdirs-as-file-objects file)))))

(child-tree-items (clojure.java.io/file "resources"))

(defn create-file-tree-item
  "Creates a complete file tree item, including children, for the given file."
  [file]
  (assoc (create-tree-item file)
    :children (child-tree-items file)))



(take 10 (file-operations/list-file-names "/home/dave"))

;; Create file tree item for resources directory
(.getCanonicalPath (clojure.java.io/file "resources"))
(create-file-tree-item (clojure.java.io/file "resources"))

;(create-file-tree-item  "/home/dave" false)
(defn file-tree-view [{:keys [current-directory expanded]}]
  (pprint/pprint (create-file-tree-item (clojure.java.io/file current-directory)))
  {:fx/type  :v-box
   :spacing  10
   :padding  10
   :children [{:fx/type :label
               :text    "Current directory:"}
              {:fx/type :label
               :text    current-directory}
              {:fx/type :tree-view
               :root    (create-file-tree-item (clojure.java.io/file current-directory))}]})


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

(defn root-view [{{:keys [gravity friction current-directory expanded tree]} :state}]
  {:fx/type :stage
   :title   "Aeonik's Excellent Adaptation Emporium"
   :showing true
   :scene   {:fx/type :scene
             :root    {:fx/type  :v-box
                       :spacing  20
                       :children [{:fx/type  :h-box
                                   :spacing  10
                                   :children [{:fx/type           file-tree-view
                                               :current-directory current-directory
                                               :expanded          :expanded}]}
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

(defn root-view [{::keys [expanded tree]}]
  (let [->desc (fn ->desc [id]
                 {:fx/type :tree-item
                  :value id
                  :expanded (contains? expanded id)
                  :on-expanded-changed {:event/type ::on-expanded-changed :id id}
                  :children (if (or (contains? expanded id) (tree id))
                              (map ->desc (tree id))
                              ;; this is a dummy tree item that exists to make
                              ;; its parent show as expandable:
                              [{:fx/type :tree-item}])})]
    {:fx/type :stage
     :showing true
     :scene {:fx/type :scene
             :root {:fx/type :tree-view
                    :root (->desc [::root])}}}))

(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc (fn [state]
                                    {:fx/type root-view
                                     :state   state}))
    :opts {:fx.opt/map-event-handler event-handler}))

(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc #'root-view)
    :opts {:fx.opt/map-event-handler handle}))

(fx/mount-renderer *state renderer)

(renderer)
;(require 'cljfx.dev)
;(cljfx.dev/help-ui)
(defn- ->tree-item [x]
  (cond
    (string? x) {:fx/type :tree-item :value x}
    (seqable? x) {:fx/type :tree-item :value x :children (map ->tree-item x)}
    :else {:fx/type :tree-item :value x}))

(def tree-table-view
  {:fx/type :tree-table-view
   :row-factory {:fx/cell-type :tree-table-row
                 :describe (fn [x]
                             {:style {:-fx-background-color (cond
                                                              (number? x) "#99f"
                                                              (string? x) "#cfa"
                                                              (map? x) "fda"
                                                              (set? x) :pink
                                                              (coll? x) "#faa"
                                                              (keyword? x) "eaf"
                                                              :else "#adf")}})}
   :columns [{:fx/type :tree-table-column
              :text "pr-str"
              :max-width 960/2
              :cell-value-factory identity
              :cell-factory {:fx/cell-type :tree-table-cell
                             :describe (fn [x]
                                         {:text (pr-str x)})}}
             {:fx/type :tree-table-column
              :text "str"
              :max-width 960/2
              :cell-value-factory identity
              :cell-factory {:fx/cell-type :tree-table-cell
                             :describe (fn [x]
                                         {:text (str x)})}}]
   :root (->tree-item
           {:set #{:a :b :c}
            :scalars ["string" false 1 1M 1/2 1.0 'symbol :keyword]
            :map {:a 1}
            :vec [1 2 3]
            :list '(1 2 3)
            :range (range 4)})})

(clojure.tools.trace/trace-form (defn root-view [{{:keys [gravity friction current-directory expanded]} :state}]
                  {:fx/type :stage
                   :showing true
                   :title   "Cell factory examples"
                   :scene   {:fx/type :scene
                             :root    {:fx/type     :tab-pane
                                       :pref-width  960
                                       :pref-height 540
                                       :tabs        [
                                                     {:fx/type  :tab
                                                      :text     "Tree Table View"
                                                      :closable false
                                                      :content  tree-table-view}
                                                     ]}}}

                  ))

(renderer)