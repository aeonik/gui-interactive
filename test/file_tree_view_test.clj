(ns file-tree-view-test
  (:require [clojure.test :refer :all]
            [file-tree-view :refer [list-files get-children update-expanded! ::tree ::expanded]]
            [java.io :as io]))


(def test-directory (io/file "resources"))                  ;; Adjust the path based on your test directory.
(def *state
  (atom {::current-directory (.getCanonicalPath test-directory)
         ::expanded          #{}
         ::tree              {}}))

(deftest list-files-test
  (testing "List files in a directory"
    (let [results (list-files test-directory)]
      (is (map? results) "Should return a map.")
      (doseq [path (keys results)]
        (is (string? path) "Each key should be a string.")
        (is (map? (results path)) "Each value should be a map.")))))

(deftest get-children-test
  (testing "Get children of a directory"
    (let [state (get-children *state (.getCanonicalPath test-directory))]
      (is (contains? (::tree @state) (.getCanonicalPath test-directory)) "Should contain the directory in the tree.")
      (is (map? (get (::tree @state) (.getCanonicalPath test-directory))) "The value should be a map."))))

(deftest update-expanded-test
  (testing "Update expanded directories"
    (let [state (update-expanded! *state (.getCanonicalPath test-directory))]
      (is (contains? (::expanded @state) (.getCanonicalPath test-directory)) "Should contain the directory in the expanded set.")
      (is (map? (get (::tree @state) (.getCanonicalPath test-directory))) "The value should be a map."))))

; TODO Finish this test
(def test-tree
  {:fx/type             :tree-item
   :value               "resources"                         ;; display just the directory name
   :expanded            false
   :on-expanded-changed {:event/type ::on-expanded-changed :id (.getCanonicalPath
                                                                 (io/file "resources"))}
   :children            [{:fx/type :tree-item}]})
(deftest tree->cljfx-tree-test
  (let [top-tree-key (keys (::tree @*state))
        state *state]
    (is (= test-tree
         (tree->cljfx-tree-test top-tree-key state)))))
