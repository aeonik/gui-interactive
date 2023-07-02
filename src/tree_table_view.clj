(ns tree_table_view
  (:require [babashka.fs :as fs]
            [cljfx.api :as fx]
            [clj-commons.digest]
            [clojure.java.io :as io])
  (:import (java.awt Taskbar)
           (javafx.scene.image Image)
           (javafx.scene.input KeyCode)
           (javax.imageio ImageIO)))

(def default-directory (fs/path "resources"))

(defmulti handle :event/type)
(defmethod handle ::key-press [{:keys [fx/event]}]
  (let [code (.getCode event)
        control-down? (.isControlDown event)]
    (when (and control-down? (= code KeyCode/LEFT))
      {:fx/type :alert
       :alert-type :information
       :title "Info"
       :header-text nil
       :content-text "Ctrl+Left key combination pressed!"})))

(defn compute-file-hash [file-path]
  (clj-commons.digest/digest "sha-256" (fs/file file-path)))

(compute-file-hash (fs/path "resources/frog.png"))

(defn combine-hashes
  "Combine a list of hashes into a single hash."
  [hashes]
  (clj-commons.digest/digest "sha-256" (apply str hashes)))

(defn hex->bytes
  "Convert a hex string to a byte array."
  [s]
  (byte-array
   (map #(Integer/parseInt (apply str %) 16)
        (partition 2 (seq s)))))

(defn combine-hashes
  "Combine a list of hex-encoded hashes into a single hash."
  [hashes]
  (clj-commons.digest/digest "sha-256" (byte-array (mapcat hex->bytes hashes))))
(combine-hashes [(compute-file-hash (fs/path "resources/frog.png"))
                 (compute-file-hash (fs/path "resources/frog.png"))])

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

(defn file-info [file-path]
  (let [children (when (fs/directory? file-path)
                   (map file-info (fs/list-dir file-path)))
        file-size (if (fs/directory? file-path)
                    (apply + (map :size children))
                    (fs/size file-path))
        file-hash (if (fs/directory? file-path)
                    (combine-hashes (map :hash children))
                    (compute-file-hash file-path))
        base-map {:name       (fs/file-name file-path)
                  :value      file-path
                  :size       file-size
                  :attributes (fs/read-attributes file-path "posix:*")
                  :hash       file-hash}]
    (if children
      (assoc base-map :children children)
      base-map)))

(file-info default-directory)

;; Sum the sizes, and hash the children hashes to get the hash of the directory
;(def file-info (file-info default-directory))

(defn- ->tree-item [x]
  (if (contains? x :children)
    {:fx/type :tree-item :value x :children (map ->tree-item (:children x))}
    {:fx/type :tree-item :value x}))

(def root-dir (file-seq (io/file "resources")))

(->tree-item (file-info default-directory))

(map fs/file-name root-dir)

(comment (defn- ->tree-item [x]
           (let [is-dir?  (.isDirectory x)
                 children (if is-dir? (seq (.listFiles x)) [])]
             {:fx/type  :tree-item
              :value    (.getPath x)
              :children (if (and is-dir? (seq children))
                          (map ->tree-item children)
                          ())})))
(defn- file->map [file]
  (when (instance? java.io.File file)
    (let [path (.getPath file)]
      (if (.isDirectory file)
        {path (map file->map (.listFiles file))}
        path))))

(defn- map->tree-item [[value children]]
  (if (map? children)
    {:fx/type :tree-item :value value :children (map map->tree-item children)}
    {:fx/type :tree-item :value value}))

(defn- root-dir->tree-item [root-dir]
  (-> root-dir file->map map->tree-item))

(def tree-table-view
  {:fx/type     :tree-table-view
   :selection-mode :multiple
   :on-key-pressed {:event/type ::key-press}
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
                  :cell-value-factory identity
                  :cell-factory       {:fx/cell-type :tree-table-cell
                                       :describe     (fn [x]
                                                       {:text (:name x)})}}
                 {:fx/type            :tree-table-column
                  :text               "Size"
                  :cell-value-factory identity
                  :cell-factory       {:fx/cell-type :tree-table-cell
                                       :describe     (fn [x]
                                                       {:text (str (:size x))})}}
                 {:fx/type            :tree-table-column
                  :text               "Hash"
                  :cell-value-factory identity
                  :cell-factory       {:fx/cell-type :tree-table-cell
                                       :describe     (fn [x]
                                                       {:text (str (:hash x))})}}]
   :root        (->tree-item (file-info default-directory))})

(def image-url (-> "frog2.png" io/resource .toString))

(def image (Image. image-url))

(when (.startsWith (System/getProperty "os.name") "Mac")
  (try
    (let [awt-image (ImageIO/read (io/input-stream (io/resource "frog2.png")))]
      (.setIconImage (Taskbar/getTaskbar) awt-image))
    (catch Exception e
      (println "Failed to set dock icon"))))

;; TODO Add Renderer function that mounts handler
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

(require 'cljfx.dev)
(cljfx.dev/help-ui)
