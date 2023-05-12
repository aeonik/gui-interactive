(ns file-operations
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader :as reader]
            [clojure.tools.analyzer.jvm :as jvm]
            [clojure.pprint :as pp]
            [clojure.walk :refer [postwalk]])
  (:import (java.io File)))

(defn recursive-file-seq
  "Returns a lazy sequence of java.io.File objects representing all files and directories
  (recursively) within the given path."
  [^String path]
  (->> path
       clojure.java.io/file
       file-seq))
(do (take 10 (recursive-file-seq (System/getProperty "user.home"))))

(defn list-file-names
  "Returns a sequence of file and directory names within the given path."
  [^String path]
  (->> path
       clojure.java.io/file
       .list
       seq))

(do (list-file-names (System/getProperty "user.home")))

(defn absolute-paths-in-dir
  "Returns a vector of absolute paths of all files and directories within the given directory."
  [^File file]
  (->> file
       .listFiles
       (mapv (fn [f] (.getAbsolutePath f)))))

(absolute-paths-in-dir (clojure.java.io/file (System/getProperty "user.home")))

(defn subdirs-as-file-objects
  "Returns a sequence of java.io.File objects representing only the subdirectories within the given directory."
  [^File file]
  (->> file
       .listFiles
       (filter #(-> % .isDirectory))
       ;(mapv #(-> % .getAbsolutePath))
       ))

(subdirs-as-file-objects (clojure.java.io/file (System/getProperty "user.home")))

(defn subdirs
  "Returns a vector of vectors, each containing the directory names and the name of a subdirectory within the given input.
  Input can be a file, path, or a vector."
  [input]
  (let [file (cond
               (instance? java.io.File input) input
               (string? input) (clojure.java.io/file input)
               (vector? input) (clojure.java.io/file (str/join File/separator input)))]
    (->> file
         .listFiles
         (filter #(.isDirectory %))
         (mapv (fn [f] (conj [(-> file .getParentFile .getName)] (.getName f)))))))

(println (subdirs "/home/dave"))
(println (subdirs (clojure.java.io/file "/home/dave")))
(println (subdirs ["home" "dave"]))



(defn absolute-paths-in-dir
  "Returns a vector of absolute paths of all files and directories within the given directory."
  [^File file]
  (->> file
       .listFiles
       (mapv (fn [f] (.getAbsolutePath f)))))

(absolute-paths-in-dir (clojure.java.io/file (System/getProperty "user.home")))
