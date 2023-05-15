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
         ::tree              {default-directory-path {}}}))
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
  (let [children (list-files (io/file dir))]
    (assoc-in state [::tree dir] children)))

(comment
  (new-state-tree-with-children @*state default-directory-path)
  )

(defn add-dir-to-expanded [state dir]
  (update state ::expanded conj dir))


(comment
  (add-dir-to-expanded @*state default-directory-path)
  )

;; Update the state with the expanded directory and its children, this function has side effects
;; Chat GPT broke this, use the next one, which is the original
(comment (defn update-expanded [state dir]
           (let [children (list-files (java.io.File. dir))
                 state-with-children (assoc-in state [::tree dir] children)]
             (swap! state
                    (fn [s]
                      (-> s
                          (assoc-in [::tree dir] children)
                          (update ::expanded conj dir)))))))

;; Update the state with the expanded directory and its children, this function has side effects
;; TODO Probably want to change this to swap
(defn update-expanded! [state dir]
  (let [state-with-children (new-state-tree-with-children state dir)
        new-state (add-dir-to-expanded state-with-children dir)]
    (reset! *state new-state)))


(comment
  (update-expanded! @*state default-directory-path)
  (pp/pprint @*state)

  )

(defmulti handle :event/type)

(defmethod handle ::on-expanded-changed [{:keys [id fx/event]}]
  (println event)
  (println id)
  (when event
    (update-expanded! @*state id)))

(defn tree->cljfx-tree [node-id tree expanded]
  (let [children (get tree node-id)
        is-expanded (contains? expanded node-id)
        has-children (or (not (empty? children))
                         (seq (.list (io/file node-id))))]
    {:fx/type :tree-item
     :value (.getName (io/file node-id))
     :expanded is-expanded
     :on-expanded-changed {:event/type ::on-expanded-changed :id node-id}
     :children (if has-children
                 (concat
                   (map #(tree->cljfx-tree % tree expanded) (keys children))
                   ;; add a dummy tree item to make its parent show as expandable only if not already expanded
                   (if (not is-expanded) [{:fx/type :tree-item}] []))
                 [])}))

(defn build-cljfx-tree [state]
  (tree->cljfx-tree (first (keys (::tree state))) (::tree state) (::expanded state)))

(defn root-view [state]
  (let [cljfx-tree (build-cljfx-tree state)]
    {:fx/type :stage
     :showing true
     :scene   {:fx/type :scene
               :root    {:fx/type :tree-view
                         :root    cljfx-tree}}}))

(root-view @*state)

; """This generates a dummy tree to allow a node to be expandable"""
(def dummy-tree-item
  [{:fx/type :tree-item}])

;; Testing build-cljfx-tree
(build-cljfx-tree @*state)

;; Trying to figure out how this tree works:
(pp/pprint (keys (::tree @*state)))

(pp/pprint (vals (::tree @*state)))

;; TODO This function works with one layer, I need to navigate to the edges of the tree and call this function on each one
(defn aeonik-get-children-keys [{::keys [tree]}]
  (mapcat keys (vals tree)))
(aeonik-get-children-keys @*state)

;; TODO I can then call this gfunctioj
(comment
  (new-state-tree-with-children @*state default-directory-path)
  )

(map list-files (map io/file (aeonik-get-children-keys @*state)))

;; Trying to figure out how to traverse the whole tree. Didn't end up using this
;; This function kind of works, but loses parents. Would need to call it in another function
(let [children (mapcat keys (vals (::tree @*state)))]
  (->> children
       (map io/file)
       (map list-files)
       ))

;; Trying to build a function that can traverse the entire tree, and convert it to CLJFX
;; This is overkill, new-state-tree-with-children
(let [children (mapcat keys (vals (::tree @*state)))]
  (->> children
       (map #(new-state-tree-with-children @*state %))
       ))

(list-files (java.io.File. "resources"))

;; Need to see if every child is in the expanded state.
(map #(contains? (::expanded @*state) %) (aeonik-get-children-keys @*state))
(pp/pprint @*state)

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc #'root-view)
   :opts {:fx.opt/map-event-handler handle}))

(fx/mount-renderer *state renderer)
