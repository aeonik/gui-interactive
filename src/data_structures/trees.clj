(ns data-structures.trees
  (:import (clojure.lang PersistentQueue)))


(defn bfs-eager [tree]
  (loop [ret [], queue (conj PersistentQueue/EMPTY tree)]
    (if (seq queue)
      (let [[node & children] (peek queue)]
        (recur (conj ret node) (into (pop queue) children)))
      ret)))

(defn bfs-lazy [tree]
  ((fn step [queue]
     (lazy-seq
       (when (seq queue)
         (let [[node & children] (peek queue)]
           (cons node
                 (step (into (pop queue) children)))))))
   (conj PersistentQueue/EMPTY tree)))


