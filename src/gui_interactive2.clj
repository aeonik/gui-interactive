(ns gui-interactive2
  (:require [cljfx.api :as fx]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (org.apache.commons.io FileUtils FilenameUtils)))

(def *state
  (atom {:gravity  10
         :friction 0.4
         :current-directory (System/getProperty "user.home")
         ::expanded #{}
         ::tree {}}))

(defn list-files-and-dirs [path]
  (file-seq (clojure.java.io/file path)))

(defn list-files-and-dirs-up-to-depth [path depth]
  (letfn [(walk [p d]
            (when (<= d depth)
              (lazy-cat [(io/file p)]
                        (when (.isDirectory p)
                          (mapcat #(walk % (inc d))
                                  (file-seq (clojure.java.io/file p)))))))]
    (walk path 0)))


(defn list-files-and-dirs [path]
  (doall (file-seq (clojure.java.io/file path))))


(comment (list-files-and-dirs "/home/dave"))

(defmulti event-handler :event/type)

(defmethod event-handler ::set-friction [e]
  (swap! *state assoc :friction (:fx/event e)))

(defmethod event-handler ::set-gravity [e]
  (swap! *state assoc :gravity (:fx/event e)))

(defmethod event-handler ::on-expanded-changed [{:keys [id fx/event]}]
  (swap!
    *state
    #(-> %
         (update ::expanded (if event conj disj) id)
         (cond-> (and event (not (get-in % [::tree id])))
                 (assoc-in [::tree id]
                           (->> (io/file (str/join "/" id))
                                (file-seq)
                                (doall) ;; Force the evaluation of the lazy sequence
                                (mapv (fn [file] (conj id (.getName file))))))))))



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

(defn create-file-tree-item [id expanded?]
  {:fx/type :tree-item
   :value (str (FilenameUtils/getBaseName (last id)))
   :expanded expanded?
   :on-expanded-changed {:event/type ::on-expanded-changed :id id}
   :children (if expanded?
               (->> (get-in @*state [::tree id])
                    (map #(create-file-tree-item % (contains? (@*state ::expanded) %))))
               ;; Add a dummy tree item to show the parent as expandable
               [{:fx/type :tree-item}])})

(defn file-tree-view [{:keys [current-directory]}]
  {:fx/type :v-box
   :spacing 10
   :padding 10
   :children [{:fx/type :label
               :text    "Current directory:"}
              {:fx/type :label
               :text    current-directory}
              {:fx/type :tree-view
               :root    (create-file-tree-item (list current-directory) true)}]})

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

(defn root-view [{{:keys [gravity friction current-directory]} :state}]
  {:fx/type :stage
   :showing true
   :scene   {:fx/type :scene
             :root {:fx/type  :v-box
                    :spacing  20
                    :children [{:fx/type  :h-box
                                :spacing  10
                                :children [{:fx/type file-tree-view
                                            :current-directory current-directory}]}
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
                                             :event   ::set-friction}]}]}}})

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