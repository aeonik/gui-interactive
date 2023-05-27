(ns tree-view-simple
  (:require [cljfx.api :as fx]
            [babashka.fs :as fs]))

;; Load tree items lazily by triggering loading when item is expanded


(def default-directory (fs/file "resources"))
(def *state
  (atom {::expanded #{}
         ::tree {default-directory (map fs/file (fs/list-dir default-directory))}}))

(defmulti handle :event/type)

(defmethod handle ::on-expanded-changed [{:keys [id fx/event]}]
  (swap!
    *state
    #(-> %
         (update ::expanded (if event conj disj) id)
         (cond-> (and event
                      (not (get-in % [::tree id]))
                      (.isDirectory id))  ;; only load children for directories
                 (assoc-in [::tree id]
                           (map fs/file (fs/list-dir id)))))))

(defn root-view [{::keys [expanded tree]}]
  (let [->desc (fn ->desc [id]
                 (if (.isDirectory id)
                   {:fx/type :tree-item
                    :value (fs/file-name id)
                    :expanded (contains? expanded id)
                    :on-expanded-changed {:event/type ::on-expanded-changed :id id}
                    :children (if (or (contains? expanded id)
                                      (tree id))
                                (map ->desc (tree id))
                                ;; this is a dummy tree item that exists to make
                                ;; its parent show as expandable:
                                [{:fx/type :tree-item}])}
                   {:fx/type :tree-item
                    :value (fs/file-name id)}))]
    {:fx/type :stage
     :showing true
     :scene {:fx/type :scene
             :root {:fx/type :tree-view
                    :root (->desc default-directory)}}}))

(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc #'root-view)
    :opts {:fx.opt/map-event-handler handle}))

(fx/mount-renderer *state renderer)