(ns core
  (:require [babashka.fs :as fs]
            [cljfx.api :as fx]
            [clj-commons.digest]
            [clojure.java.io :as io]
            [util.file :as file])
  (:import (java.awt Taskbar)
           (javafx.scene.image Image)
           (javafx.scene.input KeyCode)
           (javax.imageio ImageIO)))

(fx/on-fx-thread
  (fx/create-component
    {:fx/type :stage
     :showing true
     :title "Cell factory examples"
     :scene {:fx/type :scene
             :root {:fx/type :tab-pane
                    :pref-width 960
                    :pref-height 540
                    :tabs [{:fx/type :tab
                            :text "Hex Editor"
                            :closable false
                            :content table-view}
                           {:fx/type :tab
                            :text "List View"
                            :closable false
                            :content list-view}
                           {:fx/type :tab
                            :text "Combo Box"
                            :closable false
                            :content combo-box}
                           {:fx/type :tab
                            :text "Tree Table View"
                            :closable false
                            :content tree-table-view}
                           {:fx/type :tab
                            :text "Tree View"
                            :closable false
                            :content tree-view}
                           {:fx/type :tab
                            :text "Date Picker"
                            :closable false
                            :content date-picker}]}}}))