(ns file-tree-view
  (:require [cljfx.api :as fx]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [file-operations :as file-operations]
            [clojure.tools.reader.edn :as edn])
  (:import (org.apache.commons.io FileUtils FilenameUtils)))

;; Load tree items lazily by triggering loading when item is expanded
(def default-directory (io/file "resources"))
(def default-directory-path (.getCanonicalPath default-directory))
(def *state
  (atom {::current-directory default-directory-path
         ::expanded          #{}
         ::tree              {default-directory-path default-directory}}))
(pp/pprint @*state)

(def in-development? true)

(defn list-files [dir]
  (->> dir
       .listFiles
       (filter #(.isDirectory %))
       (map (fn [file] {(.getCanonicalPath file) {}}))
       (apply merge)))

(comment
  (list-files (java.io.File. "resources"))
  )

(defn new-state-tree-with-children [state dir]
  (let [children (list-files (java.io.File. dir))]
    (assoc-in state [::tree dir] children)))

(comment
  (new-state-tree-with-children @*state default-directory-path)
  )

(defn new-state-expanded [state dir]
  (update state ::expanded conj dir))


(comment
  (new-state-expanded @*state default-directory-path)
  )

;; Update the state with the expanded directory and its children, this function has side effects
(defn update-expanded [state dir]
  (let [state-with-children (new-state-tree-with-children state dir)
        new-state (new-state-expanded state-with-children dir)]
    (reset! *state new-state)))


(comment
  (update-expanded @*state default-directory-path)
  )

(defmulti handle :event/type)

(defmethod handle ::on-expanded-changed [{:keys [id event]}]
  (when event
    (update-expanded @*state id)))


(defn root-view [{::keys [expanded tree current-directory]}]
  (let [->desc (fn ->desc [id]
                 {:fx/type             :tree-item
                  :value               (.getName (java.io.File. id))  ;; display just the directory name
                  :expanded            (contains? expanded id)
                  :on-expanded-changed {:event/type ::on-expanded-changed :id id}
                  :children (if (contains? expanded id)
                              (map ->desc (keys (list-files (get tree id))))
                              ;; this is a dummy tree item that exists to make
                              ;; its parent show as expandable:
                              [{:fx/type :tree-item}])})]
    {:fx/type :stage
     :showing true
     :scene   {:fx/type :scene
               :root    {:fx/type :tree-view
                         :root    (->desc current-directory)}}}))

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc #'root-view)
   :opts {:fx.opt/map-event-handler handle}))

(fx/mount-renderer *state renderer)
