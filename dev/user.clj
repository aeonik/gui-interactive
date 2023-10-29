(ns user
  (:require [nextjournal.clerk :as clerk]))

(clerk/serve! {:host "localhost" :browse true :watch-paths ["notebooks" "src"]})