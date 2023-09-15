(ns hex-editor.binary
  (:require [clojure.java.io :as io]
            [buddy.core.codecs :as codecs]
            [rmap.core :as rmap :refer [rmap!]])
  (:import (java.nio ByteBuffer)
           [org.apache.commons.codec.binary Hex]
           [java.io ByteArrayOutputStream
                    RandomAccessFile]
           [java.util HexFormat]))

(defn slurp-bytes
  "Slurp the bytes from a slurpable thing, this is an eager load"
  [x]
  (with-open [out (ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (.toByteArray out)))

(defrecord LazyFile [path raf length])

(defn open-lazy-file [path]
  "Open a file in read-only mode and return a LazyFile record"
  (let [raf (RandomAccessFile. path "r")
        length (.length raf)]
    (->LazyFile path raf length)))

(defn close-lazy-file [lazy-file]
  (.close (:raf lazy-file)))

(defn read-byte [lazy-file pos]
  (let [raf (:raf lazy-file)]
    (.seek raf pos)
    (byte (.read raf))))

(defn read-range [lazy-file start end]
  (lazy-seq
    (when (< start end)
      (cons (read-byte lazy-file start) (read-range lazy-file (inc start) end)))))

(def file (io/file "/home/dave/Projects/mods/bg3/Shared.pak"))

(def binary-file (slurp-bytes file))

(def binary-file-lazy (open-lazy-file "/home/dave/Projects/mods/bg3/Shared.pak"))

(comment (read-byte binary-file-lazy 2))
(comment
  (time (read-range binary-file-lazy 150 153))
  (time (read-range binary-file-lazy 15000 15003)))
(comment (take 1 binary-file))
(comment (rmap! {:foo 1
                 :bar (rmap/ref :foo)}))

(def file-map
  (rmap!
    {:header           4
     :version          4
     :file-list-offset 8
     :file-list-size   4
     :flags            1
     :priority         1
     :MD5              16
     :file-data        (rmap/ref :file-list-size)}))

(read-range binary-file-lazy (:file-list-size file-map) (:file-list-offset file-map))



(defn process-spec
  [spec byte-seq]
  (loop [remaining-spec  spec
         remaining-bytes byte-seq
         acc             {}]
    (if (empty? remaining-spec)
      acc
      (let [[label value] (first remaining-spec)]
        (cond
          (number? value)
          (let [segment (take value remaining-bytes)]
            (recur (rest remaining-spec) (drop value remaining-bytes)
                   (assoc acc label segment)))

          (map? value)
          (let [nested (process-spec value remaining-bytes)]
            (recur (rest remaining-spec) (drop (apply + (map count (vals nested))) remaining-bytes)
                   (assoc acc label nested)))

          :else
          (recur (rest remaining-spec) remaining-bytes acc))))))


(def file-spec
  {:header           4
   :version          4
   :file-list-offset 8
   :file-list-size   4
   :nested           {:flags 1, :priority 1}})

(def self-map {:header           4
               :version          4
               :file-list-offset 8
               :file-list-size   4
               :nested-map           {:flags 1, :priority 1}
               :data             :file-list-size
               :test   :nested-map
               :test2 {:nested-map :nested-map}})                ; Self-reference to :file-list-size

(defn build-self-ref-map [spec]
  (let [acc (atom {})]
    (doseq [[k v] spec]
      (if (keyword? v)
        (reset! acc (assoc @acc k (get @acc v)))            ; Self-reference
        (reset! acc (assoc @acc k v))))                     ; Normal assignment
    @acc))

(def my-self-ref-map (build-self-ref-map self-map))

(comment (process-spec file-spec file-bytes))

(comment {:header           {:address 0 :size 4 :data (76 83 80 75)},
          :version          {:address 4 :size 4 :data (18 0 0 0)},
          :file-list-offset {:address 8 :size 8 :data (40 62 196 27 0 0 0 0)},
          :file-list-size   {:address 16 :size 4 :data (205 148 20 0)},
          :nested           {:address 20 :size 2 :data {:flags    {:address 20 :size 1 :data (0)},
                                                        :priority {:address 21 :size 1 :data (0)}}}}
         (defn nested-fn [acc _]
           (let [file-list-size        (:size (get acc :file-list-size))
                 file-list-offset-data (:data (get acc :file-list-offset))
                 dynamic-labels        (mapv (fn [n] (keyword (str "dynamic-label-" n))) (range 1 (inc file-list-size)))]
             (zipmap dynamic-labels (repeat {:size 1 :data file-list-offset-data}))))

         (def file-spec
           {:header           4
            :version          4
            :file-list-offset 8
            :file-list-size   4
            :nested           nested-fn})

         (defn process-label [acc label size-fn byte-seq]
           (let [current-address (count (apply concat (map :data (vals acc))))
                 size            (if (fn? size-fn) (size-fn acc) size-fn)
                 segment         (take size byte-seq)]
             (assoc acc label {:address current-address :size size :data segment})))

         (defn run-spec [spec byte-seq acc]
           (reduce
             (fn [acc [label size-fn]]
               (let [remaining-bytes (drop (count (apply concat (map :data (vals acc)))) byte-seq)]
                 (cond
                   (number? size-fn) (process-label acc label size-fn remaining-bytes)
                   (fn? size-fn) (let [new-spec (size-fn acc remaining-bytes)]
                                   (assoc acc label (run-spec new-spec remaining-bytes {})))
                   :else acc)))
             acc
             (seq spec)))
         (defn process-spec [spec byte-seq]
           (run-spec spec byte-seq {}))

         (def byte-seq [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 4 0 0 0 1 2])

         (run-spec file-spec byte-seq {}))