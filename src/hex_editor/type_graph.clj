(ns hex-editor.type-graph
  (:require [clojure.pprint :refer [pprint]]
            [ubergraph.core :as uber]
            [clojure.core.logic :as logic]
            [clojure.core.logic.pldb :as pldb]
            [ubergraph.alg :as algo]
            [typed.clojure :as t]
            [clojure.core.async :as async :refer [chan go <! >!]])
  (:import (com.sun.jdi ShortValue)
           (java.nio ByteBuffer ByteOrder)))

; Simple conversion functions
(defn byte->hex [b]
  (format "%02X" b))

(defn hex->byte [hex-str]
  (-> hex-str
      (Integer/parseInt 16)
      (mod 256)
      (byte)))

(def <-> :<->)

(defn ^{:convert [:str <-> :int]} int-str-involution [x]
  (cond
    (integer? x) (str x)
    (string? x) (Integer/parseInt x)
    :else (throw (IllegalArgumentException. "Unsupported type"))))

(def type-conversion-graph [[:byte :int {:fn :byte->int}]
                            [:byte :hex {:fn :byte->hex}]
                            [:int :byte {:fn :int->byte}]
                            [:int :str {:fn :int->str}]
                            [:str :int {:fn :str->int}]
                            [:str :hex {:fn :str->hex}]
                            [:hex :byte {:fn :hex->byte}]
                            [:hex :str {:fn :hex->str}]
                            [:int :str {:fn :int-str-involution}]
                            [:str :int {:fn :int-str-involution}]
                            [:str :test {:fn :str->test}]])

"Couldn't get logic variables to work with the graph, well, it works, but cycles cause infinite loops"
(comment (defn conversiono [graph from to fn-name]
           (logic/membero [from to fn-name] graph))

         (defn patho [graph from to path]
           (logic/fresh [a b next-path]
                        (logic/conde
                          [(conversiono graph from to b) (logic/== path [b])]
                          [(conversiono graph from a b)
                           (patho graph a to next-path)
                           (logic/conso b next-path path)])))

         (defn find-conversion-path [graph from to]
           (logic/run 20 [q]
                      (patho graph from to q)))
         (time (find-conversion-path type-conversion-graph :byte :test))
         (t/ann byte->hex [Byte -> String])
         (t/ann hex->byte [String -> Byte]))

(def type-graph (-> (uber/multigraph)
                    (uber/add-directed-edges* type-conversion-graph)))
(uber/pprint type-graph)

(uber/pprint (-> (uber/multigraph)
                 (uber/add-directed-edges* type-conversion-graph)))

(uber/viz-graph (-> (uber/multigraph)
                    (uber/add-directed-edges* type-conversion-graph))
                {:layout :neato :auto-label true})

(algo/pprint-path (algo/shortest-path type-graph :byte :test))