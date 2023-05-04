(ns file_tree_view
  (:require [cljfx.api :as fx]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [file-operations :as file-operations]
            )
  (:import (org.apache.commons.io FileUtils FilenameUtils)))

;; Load tree items lazily by triggering loading when item is expanded

(def *state
  (atom {::current-directory (.getCanonicalPath (clojure.java.io/file "resources"))
         ::expanded #{} ;; expanded ids
         ::tree {}})) ;; id -> children

(def in-development? true)
(use 'clojure.tools.trace)

(add-watch *state :logger
           (fn [_key _atom old-state new-state]
             (println "old:" old-state)
             (println "new:" new-state)))


(defmulti handle :event/type)

(defmethod handle ::on-expanded-changed [{:keys [id fx/event]}]
  (trace (swap!
           *state
           #(-> %
                (update ::expanded (if event conj disj) id)
                (cond-> (and event (not (get-in % [::tree id])))
                        (assoc-in [::tree id]
                                  ;; This is "lazy loading". Note: swap! fn might be retried,
                                  ;; don't do side effects here in your app
                                  (file-operations/subdirs (get-in % [::current-directory]))))))))

(defn root-view [{::keys [expanded tree current-directory]}]
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
    :middleware (fx/wrap-map-desc #'root-view)
    :opts {:fx.opt/map-event-handler handle}))

(fx/mount-renderer *state renderer)