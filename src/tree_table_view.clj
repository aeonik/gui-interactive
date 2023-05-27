(ns tree_table_view
  (:require [babashka.fs :as fs]
            [cljfx.api :as fx]
            [clj-commons.digest]
            [clojure.java.io :as io])
  (:import (java.awt Taskbar)
           (javafx.scene.image Image)
           (javax.imageio ImageIO)))

(def default-directory (fs/path "resources"))

(defn compute-file-hash [file-path]
  (clj-commons.digest/sha-256 (fs/file file-path)))

(defn file-info [file-path]
  (let [base-map {:name       (fs/file-name file-path)
                  :value     file-path
                  :size       (fs/size file-path)
                  :attributes (fs/read-attributes file-path "posix:*")
                  :hash       (when (not (fs/directory? file-path))
                                (compute-file-hash file-path))}
        children-map (when (fs/directory? file-path)
                       {:children (map file-info (fs/list-dir file-path))})]
    (merge base-map children-map)))

(file-info default-directory)

(defn- ->tree-item [x]
  (cond
    (string? x) {:fx/type :tree-item :name x}
    (seqable? x) {:fx/type :tree-item :name x :children (map ->tree-item x)}
    :else {:fx/type :tree-item :value x}))

(defn- ->tree-item2 [x]
  (if (contains? x :children)
    {:fx/type :tree-item :value (:name x) :children (map ->tree-item2 (:children x))}
    {:fx/type :tree-item :value (:name x)}))



;; Check if item if a file or a directory, check if the directory has children

(def root-dir (file-seq (io/file "resources")))

(->tree-item2 (file-info default-directory))

(->tree-item (map #(.getPath %)
                  (file-seq (clojure.java.io/file "resources"))))

(map fs/file-name root-dir)

(comment (defn- ->tree-item [x]
           (let [is-dir?  (.isDirectory x)
                 children (if is-dir? (seq (.listFiles x)) [])]
             {:fx/type  :tree-item
              :value    (.getPath x)
              :children (if (and is-dir? (seq children))
                          (map ->tree-item children)
                          ())})))

(->tree-item root-dir)

(defn- file->map [file]
  (when (instance? java.io.File file)
    (let [path (.getPath file)]
      (if (.isDirectory file)
        {path (map file->map (.listFiles file))}
        path))))

(map file->map (->tree-item root-dir))

(defn- map->tree-item [[value children]]
  (if (map? children)
    {:fx/type :tree-item :value value :children (map map->tree-item children)}
    {:fx/type :tree-item :value value}))

(defn- root-dir->tree-item [root-dir]
  (-> root-dir file->map map->tree-item))

(->tree-item root-dir)

;; TODO: Need to change the :root object to a function that has a tree structure with :children
(def tree-table-view
  {:fx/type     :tree-table-view
   :row-factory {:fx/cell-type :tree-table-row
                 :describe     (fn [x]
                                 {:style {:-fx-background-color (cond
                                                                  (number? x) "#99f"
                                                                  (string? x) :gray
                                                                  (map? x) "fda"
                                                                  (set? x) :pink
                                                                  (coll? x) "#faa"
                                                                  (keyword? x) "eaf"
                                                                  :else "#adf")}})}
   :columns     [{:fx/type            :tree-table-column
                  :text               "File Name"
                  :max-width          960/2
                  :cell-value-factory identity
                  :cell-factory       {:fx/cell-type :tree-table-cell
                                       :describe     (fn [x]
                                                       {:text x})}}
                 {:fx/type            :tree-table-column
                  :text               "str"
                  :max-width          960/2
                  :cell-value-factory identity
                  :cell-factory       {:fx/cell-type :tree-table-cell
                                       :describe     (fn [x]
                                                       {:text x})}}]
   :root        (->tree-item2 (file-info default-directory))})

(def image-url (-> "frog2.png" io/resource .toString))
(def image (Image. image-url))
(when (.startsWith (System/getProperty "os.name") "Mac")
  (try
    (let [awt-image (ImageIO/read (io/input-stream (io/resource "frog2.png")))]
      (.setIconImage (Taskbar/getTaskbar) awt-image))
    (catch Exception e
      (println "Failed to set dock icon"))))

(fx/on-fx-thread
 (fx/create-component
   {:fx/type :stage
    :showing true
    :title   "Cell factory examples"
    :icons   [image]
    :scene   {:fx/type :scene
              :root    {:fx/type     :tab-pane
                        :pref-width  960
                        :pref-height 540
                        :tabs        [{:fx/type  :tab
                                       :text     "Tree Table View"
                                       :closable false
                                       :content  tree-table-view}]}}}))
