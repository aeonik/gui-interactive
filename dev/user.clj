(ns user
  (:require [nextjournal.clerk :as clerk]
            [clojure.tools.namespace.repl :refer [refresh]]
            ))

;; Serve Clerk UI
(clerk/serve! {:host "localhost" :browse true :watch-paths ["notebooks" "src"]})
