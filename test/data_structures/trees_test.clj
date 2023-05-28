(ns data-structures.trees-test
  (:require [clojure.test :refer :all]
            [data-structures.trees :as trees]))

(defn test-tree []
  {:name "root"
   :children [{:name "child1"
               :children [{:name "grandchild1"} {:name "grandchild2"}]}
              {:name "child2"
               :children [{:name "grandchild3"} {:name "grandchild4"}]}]})

(deftest bfs-eager-test
  (testing "BFS eager"
    (is (= (map :name (trees/bfs-eager (test-tree)))
           ["root" "child1" "child2" "grandchild1" "grandchild2" "grandchild3" "grandchild4"]))))

(deftest bfs-lazy-test
  (testing "BFS lazy"
    (is (= (map :name (take 7 (trees/bfs-lazy (test-tree))))
           ["root" "child1" "child2" "grandchild1" "grandchild2" "grandchild3" "grandchild4"]))))
