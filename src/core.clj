(ns core
    (:require [membrane.skia :as skia]
              [membrane.ui :as ui
               :refer [vertical-layout
                       translate
                       horizontal-layout
                       button
                       label
                       with-color
                       bounds
                       spacer
                       on]]
              [membrane.component :as component
               :refer [defui defeffect]]
              [membrane.basic-components :as basic]
              [clojure.java.io :as io]
              [clojure.string :as str]
              [babashka.fs :as fs]))

;; Helper function to get relative path
(defn relative-path [base file]
  (str (fs/relativize base file)))

;; Helper function to safely read file content
(defn read-file-content [file]
  (try
    (if (fs/regular-file? file)
      (slurp (str file))
      "")
    (catch Exception e
      (str "Error reading file: " (.getMessage e)))))

(defui item-row [{:keys [item-name selected? is-file?]}]
  (on
   :mouse-down
   (fn [_]
     [[:update $selected? not]])
   (horizontal-layout
    (translate 5 5
               (ui/checkbox selected?))
    (spacer 5 0)
    (ui/label (str (if is-file? "📄 " "📁 ") item-name)))))

(defui file-browser
  [{:keys [base-path selected-file file-content str-filter]
    :or {str-filter ""
         selected-file nil
         file-content ""}}]
  (let [files (->> (fs/glob base-path "**" {:hidden false})
                   (filter #(not= (str base-path) (str %)))
                   (map (fn [f]
                          {:file f
                           :path (relative-path base-path f)
                           :is-file? (fs/regular-file? f)}))
                   (filter #(str/includes? (str/lower-case (:path %))
                                         (str/lower-case str-filter)))
                   (sort-by :path))]
    (horizontal-layout
     ;; Left panel: file browser
     (vertical-layout
      (ui/label "Filter:")
      (basic/textarea {:text str-filter})
      (spacer 0 10)
      (ui/label "Files and Folders (hidden excluded):")
      (spacer 0 5)
      (basic/scrollview
       {:scroll-bounds [300 400]
        :body (apply
               vertical-layout
               (for [{:keys [file path is-file?]} files]
                 (on :update
                     (fn [& args]
                       (when is-file?
                         [[:set $selected-file file]
                          [:set $file-content (read-file-content file)]]))
                     (item-row {:item-name path
                               :selected? (= selected-file file)
                               :is-file? is-file?}))))}))

     (spacer 20 0)

     ;; Right panel: file content viewer
     (vertical-layout
      (ui/label (if selected-file
                  (str "Content of: " (relative-path base-path selected-file))
                  "Select a file to view its content"))
      (spacer 0 10)
      (basic/scrollview
       {:scroll-bounds [500 500]
        :body (basic/textarea {:text file-content
                              :read-only? true})})))))

(defn -main
  ([]
   (-main "."))
  ([path]
   (let [state (atom {:base-path path})]
     (skia/run-sync (component/make-app #'file-browser state)))))
