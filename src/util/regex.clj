(ns util.regex
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(defn replace-object-tags [filename]
  (let [content (slurp filename)
        new-content (str/replace content
                                 #"#object\[[^\]]*\"\s([^\"]*)\"\]"
                                 "$1")]
    (spit filename new-content)))

(defn replace-object-tags-new [input-filename output-filename]
  (let [content (slurp input-filename)
        new-content (str/replace content
                                 #"#object\[[^\]]*\"\s([^\"]*)\"\]"
                                 "$1")]
    (spit output-filename new-content)))


(replace-object-tags "resources/test_tree.edn")

(def file (slurp "resources/test_tree.edn"))

(def matches (re-seq #"#object\[[^\]]*\"\s([^\"]*)\"\]" file))

(println matches)