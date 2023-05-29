(ns data-structures.trees
  (:require [clojure.zip :as z]
            [clojure.tools.trace] :as t)
  (:import (clojure.lang PersistentQueue)))

(def tree {:name "root"
           :children [{:name "child1"
                       :children [{:name "grandchild1"} {:name "grandchild2"}]}
                      {:name "child2"
                       :children [{:name "grandchild3"} {:name "grandchild4"}]}]})


;; BFS Traversal algorithms from stackoverflow
;; https://stackoverflow.com/questions/11409140/stumped-with-functional-breadth-first-tree-traversal-in-clojure
(defn bfs-eager-queue [tree]
  (loop [ret [], queue (conj PersistentQueue/EMPTY tree)]
    (if (seq queue)
      (let [[node & children] (peek queue)]
        (recur (conj ret node) (into (pop queue) children)))
      ret)))

(defn bfs-lazy-queue [tree]
  ((fn step [queue]
     (lazy-seq
       (when (seq queue)
         (let [[node & children] (peek queue)]
           (cons node
                 (step (into (pop queue) children)))))))
   (conj PersistentQueue/EMPTY tree)))

(def lazy-seq (bfs-lazy-queue tree))
(doseq [node lazy-seq]
  (println (:name node)))

(defn bfs-eager [tree]
  (loop [ret [], queue (conj PersistentQueue/EMPTY tree)]
    (if (seq queue)
      (let [node (peek queue)
            children (:children node)]
        (recur (conj ret node) (into (pop queue) children)))
      ret)))

(def eager-seq (bfs-eager tree))

;; Print out the names of the nodes in the order they were visited.
(doseq [node eager-seq]
  (println (:name node)))

;; Trying to use maps to traverse a nested structure.
(:name tree)
(map key tree)
(map keys (:children tree))

;; This doesn't work
(defn map-bfs [tree function]
  (map function tree)
  (map map-bfs (:children tree) function))
(map-bfs tree :name)

;; I have been told that flattening tree structure can make it easier.
;; This function allows calling map over a tree
;; Problem is it loses all structural information in the tree
(defn flatten-tree [tree]
  (tree-seq
    (comp seq :children) ;; Branch if :children is a non-empty sequence.
    :children             ;; Get children of a node.
    tree))                ;; The root of the tree.

(map :name (flatten-tree tree))

;; Trying to fix previous problem
(defn flatten-tree-with-depth
  ([tree] (flatten-tree-with-depth tree 0))
  ([tree depth]
   (lazy-seq
     (cons
       [tree depth]
       (mapcat #(flatten-tree-with-depth % (inc depth)) (:children tree))))))

(map #(assoc (first %) :depth (second %)) (flatten-tree-with-depth tree))

(defn flatten-tree-with-depth
  ([tree] (flatten-tree-with-depth tree 0))
  ([tree depth]
   (println (:children tree))
   (lazy-seq
     (cons
       [tree depth]
       (mapcat #(flatten-tree-with-depth % (inc depth)) (:children tree))))))

(use 'clojure.tools.trace)
(defn flatten-tree-with-depth
  ([tree]
   (println "First form:" tree)
   (flatten-tree-with-depth tree 0))
  ([tree depth]
   (println "Second form:" tree "Depth:" depth)
   (lazy-seq
     (concat
       [[tree depth]]
       (mapcat #(flatten-tree-with-depth % (inc depth)) (:children tree))))))

(def flattened-tree (t/trace (flatten-tree-with-depth tree)))

(map (fn [[node depth]] (assoc node :depth depth)) flattened-tree)

(defn flatten-tree-with-depth
  ([tree] (flatten-tree-with-depth tree 0))
  ([tree depth]
   (lazy-seq
     (cons
       [tree depth]
       (doall (mapcat #(flatten-tree-with-depth % (inc depth)) (:children tree)))))))

(defn node-values [nodes]
  (map first nodes))

(defn node-children [nodes]
  (mapcat next nodes))

(defn depth-traversal [nodes]
  (if (not (empty? nodes))
    (cons (node-values nodes) (depth-traversal (node-children nodes)))))

(depth-traversal tree)

;; From Brandon Bloom Dendrology Talk
;; https://www.youtube.com/watch?v=YgvJqWiyMRY
(defn annotate-depth [node]
  (letfn [(f [node depth]
            (let [d (inc depth)
                  annotate-child #(f % d)]
              (-> node
                  (assoc :depth depth)
                  (update-in [:children]
                             (mapv annotate-child %)))))]
    (f node 0)))

;; ChatGPT says this version is better:
(defn annotate-depth [node]
  (letfn [(f [node depth]
            (let [d (inc depth)]
              (assoc node
                :depth depth
                :children (mapv #(f % d) (:children node)))))]
    (f node 0)))

(defn annotate-max-depth [node]
  (let [{:keys [children]} node]
    (if (seq children)
      (let [children*
            (mapv annotate-max-depth children)]
        (assoc node
          :max-depth (apply max
                            (map :max-depth children*))
          :children children*))
      (assoc node :max-depth (:depth node)))))

(defn print-depth-first-recursive [node]
  (letfn [(f [node index]
            (println index (:label node))
            (loop [nodes (:children node)
                   i index]
              (if (seq nodes)
                (recur (next nodes)
                       (f (first nodes) (inc i)))
                i)))]
    (f node 0))
  nil)

(defn number-depth-first-recursive [node]
  (letfn [(f [node index]
            (let [[max-index children*]
                  (reduce (fn [[i children] child]
                            (let [child* (f child (inc i))
                                  i* (:max-index child*)]
                              [i* (conj children child*)]))
                          [index []]
                          (:children node))]
              (assoc node
                :index index
                :children children*
                :max-index max-index)))]
    (f node 0)))

(defn number-depth-first-stateful [node]
  (let [index (atom 0)]
    ((fn rec [n]
       (let [i @index]
         (swap! index inc)
         (-> n
             (assoc :index i)
             (update-in [:children] #(mapv rec %)))))
     node)))

(defn print-depth-first-iterative [node]
  (loop [index 0
         nodes (list node)]
    (when (seq node)
      (let [[node & nodes*] nodes]
        (println index (:label node))
        (recur (inc index)
               (concat (:children node)
                       nodes*))))))

(defn print-breadth-first-iterative [node]
  (loop [index 0
         nodes (list node)]
    (when (seq node)
      (let [[node & nodes*] nodes]
        (println index (:label node))
        (recur (inc index)
               (concat nodes*
                       (:children node)))))))

(defn make-zipper [root]
  (z/zipper (fn branch? [n]
              true)
            :children
            (fn make-node [n children]
              (assoc n :children (vec children)))
            root))

(defn number-depth-first-zipper [node]
  (loop [index 0
         loc (make-zipper node)]
    (if (z/end? loc)
      (z/root loc)
      (let [loc* (z/edit loc assoc :index index)]
        (recur (inc index) (z/next loc*))))))

;; Further Resources
;; Breadth First numbering by Chris Okasaki
;; Backtracking Iterators by Jean-Christoph Filliâtre