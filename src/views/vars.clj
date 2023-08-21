(ns views.vars
  (:require [clojure.tools.namespace.find]
            [clojure.tools.namespace.file])
  (:import (clojure.lang RT)))

(defn ns-publics-list
  "Provides list of public functions a namespace provides"
  [ns]
  (#(list (ns-name %) (map first (ns-publics %))) ns))

(defn source-clj
  [ns]
  (require ns)
  (some->> ns
           ns-publics
           vals
           first
           meta
           :file
           (.getResourceAsStream (RT/baseLoader)) .toString))

(source-clj 'views.vars)

*ns*