(ns views.editor
  (:require
    [cljfx.api :as fx]))

(require 'cljfx.dev)
(cljfx.dev/help-ui)

(defn buffer
  [{:keys [text]}]
  {:fx/type :text-area
   :text text})
