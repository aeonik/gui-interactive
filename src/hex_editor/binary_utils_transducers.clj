(ns hex-editor.binary-utils-transducers
  (:require [clojure.pprint :refer [pprint]])
  (:import [java.nio ByteBuffer ByteOrder]))

(defn partition-all-sizes [sizes coll]
  (for [size sizes]
    {:size size, :partitioned (partition-all size coll)}))

(defn reversing-txf [rf]
  (fn
    ([] (rf))
    ([result] (rf result))
    ([result input] (rf result (map reverse input)))))

(def test-bytes (byte-array [(byte 32) (byte -84) (byte 32) (byte -84) (byte 32) (byte -84) (byte 32) (byte -84)]))
(take 1 test-bytes)

(defn partition-all-sizes-lazy
  "Partition a collection into every size given the parameter sizes
   This is a lazy version of partition-all-sizes"
  [sizes coll]
  (lazy-seq
    (when-let [s (first sizes)]
      (cons {:size s, :partitioned (partition-all s coll)}
            (partition-all-sizes (rest sizes) coll)))))

(partition-all-sizes-lazy [1 2 4 8] test-bytes)

(time (transduce (map :partitioned) conj () (partition-all-sizes-lazy [1 2 4 8] test-bytes)))
(time (transduce (map :partitioned) conj [] (partition-all-sizes-lazy [1 2 4 8] test-bytes)))
(transduce
  (comp
    (map :partitioned)
    reversing-txf)
  conj []
  (partition-all-sizes-lazy [1 2 4 8] test-bytes))

(map reverse (partition-all-sizes [1 2 4 8] test-bytes))

(get (first (partition-all-sizes [1 2 4 8] test-bytes)) :partitioned)

(partition-all-sizes [1 2 4 8] test-bytes)

(defn make-to-hex-transducer [f]
  (fn
    ([] identity)
    ([result] result)
    ([result x]
     (if (sequential? x)
       (do
         (println "Debug to-hex: " x)
         (conj result (map f x)))
       (do
         (println "Debug to-hex: " x)
         (conj result (f x)))))))

(defn to-hex
  "More robust to-hex conversion."
  ([x]
   (cond
     (number? x) (format "%02x" x)
     (vector? x) (map #(format "%02x" %) x)
     :else (throw (ex-info "Unsupported type" {:type (type x)}))))
  ([xs]
   (map #(to-hex %) xs)))


(defn demo-hex []
  (let [ints [15 16 31 32 63 64]
        xf-hex (map (make-to-hex-transducer to-hex))]
    (println (transduce xf-hex conj [] ints))))

(defn transduce-ints [xf input]
  (transduce xf conj [] input))

(demo-hex)

(defn hex-to-int [x] (Integer/parseInt x 16))
(defn byte->signed [b] (if (>= b 128) (- b 256) b))

(defn make-byte->int-transducer [f byte-length debug-message]
  (fn
    ([] identity)
    ([result] result)
    ([result bs]
     (println "Debug BS Data Type:" (type bs))
     (when debug-message
       (println "Debug" debug-message ":" bs))
     (if (= byte-length (count bs))
       (conj result (f bs))
       (throw (IllegalArgumentException. (str "Expected a byte sequence of length " byte-length ", got: " bs)))))))
(defn byte->uint8 [bs]
  (byte bs))

(defn byte->uint16 [bs]
  (-> (.getShort (ByteBuffer/wrap (byte-array bs))) int))

(defn byte->sint16 [bs]
  (-> (.getShort (ByteBuffer/wrap (byte-array bs))) int))

(defn byte->uint32 [bs]
  (.getInt (ByteBuffer/wrap (byte-array bs))))

(defn byte->sint32 [bs]
  (println "Debug byte->sint32: " bs)
  (int (.getInt (ByteBuffer/wrap (byte-array bs)))))

(defn byte->uint64 [bs]
  (println "Debug byte->uint64: " bs)
  (.getLong (ByteBuffer/wrap (byte-array bs))))

(defn byte->sint64 [bs]
  (println "Debug byte->sint64: " bs)
  (long (.getLong (ByteBuffer/wrap (byte-array bs)))))


(defn transduce-bytes [xf input]
  (transduce xf conj [] input))

(defn demo []
  (let [bytes (mapv byte-array [[0] [2] [4]])
        xf-uint8 (map (make-byte->int-transducer byte->uint8 1 "byte->uint8"))]
    (println (transduce xf-uint8 conj [] bytes))))

(demo)
(comment :uint8 {:xform (map #(map (fn [x] (bit-and x 0xFF)) %)), :size 1})

(def type-data
  {:hex     {:xform (map to-hex), :size 1}
   :utf-8   {:xform (map #(try (String. (byte-array [%]) "UTF-8") (catch Exception e :invalid))), :size 1}
   :uint8 {:xform (map byte->uint8), :size 1}
   :sint8 {:xform (map identity), :size 1}
   :uint16 {:xform (map byte->uint16), :size 2}
   :sint16 {:xform (map byte->sint16), :size 2}
   :uint32 {:xform (map byte->uint32), :size 4}
   :sint32 {:xform (map byte->sint32), :size 4}
   :uint64 {:xform (map byte->uint64), :size 8}
   :sint64 {:xform (map byte->sint64), :size 8}})

(defn test-transform [bytes xform]
  (vec (sequence xform bytes)))

(def hex-test (test-transform test-bytes (get-in type-data [:hex :xform])))
(def utf8-test     (test-transform test-bytes (get-in type-data [:utf-8 :xform])))
(def uint8-test    (test-transform test-bytes (get-in type-data [:uint8 :xform])))
(def sint8-test    (test-transform test-bytes (get-in type-data [:sint8 :xform])))
(def uint16-test   (test-transform (partition-all 2 test-bytes) (get-in type-data [:uint16 :xform])))
(def sint16-test   (test-transform (partition-all 2 test-bytes) (get-in type-data [:sint16 :xform])))
(def uint32-test   (test-transform (partition-all 4 test-bytes) (get-in type-data [:uint32 :xform])))
(def sint32-test   (test-transform (partition-all 4 test-bytes) (get-in type-data [:sint32 :xform])))
(def uint64-test   (test-transform (partition-all 8 test-bytes) (get-in type-data [:uint64 :xform])))
(def sint64-test   (test-transform (partition-all 8 test-bytes) (get-in type-data [:sint64 :xform])))

(defn build-xform-pipeline
  "Builds a composite transducer from a list of types."
  [types]
  (apply comp (map #(get-in type-data [% :xform]) types)))

(def size 4)
(defn get-applicable-types [size]
  (filter #(= (:size (get type-data %)) size) (keys type-data)))

(defn partition-all-sizes-lazy [sizes coll]
  (map (fn [size]
          {:size size,
           :partitioned (partition-all size coll)}) sizes))

(defn partition-all-sizes-lazy [sizes coll]
  (mapv (fn [size]
          {:size size,
           :partitioned (mapv vec (partition-all size coll))})
        sizes))


(def partitioned-bytes (partition-all-sizes-lazy [1 2 4 8] test-bytes))

(defn dynamic-xform-for-size [size]
  (let [applicable-types (filter #(= (:size (get type-data %)) size) (keys type-data))]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (rf result
             (into {} (map (fn [atype]
                             [atype (transduce (get-in type-data [atype :xform]) conj [] input)])
                           applicable-types))))))))

(defn dynamic-xform []
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result {:keys [size partitioned]}]
       (let [applicable-types (get-applicable-types size)]
         (->> applicable-types
              (map (fn [atype]
                     [atype (transduce (get-in type-data [atype :xform]) conj [] partitioned)]))
              (into {})
              (rf result)))))))

(defn partitioned-xforms [sizes coll]
  (mapv (fn [size]
          {:size size,
           :partitioned (mapv vec (partition-all size coll))})
        sizes))

(defn main-pipeline [sizes coll]
  (transduce (comp
               (map #(assoc % :xforms (get-applicable-types (:size %))))
               (dynamic-xform))
             conj
             (partitioned-xforms sizes coll)))


(def result (main-pipeline [1 2 4 8] test-bytes))


(get-applicable-types 1)
(defn get-applicable-types [size]
  (filter #(= (:size (get type-data %)) size) (keys type-data)))

(defn byte-array->vec [byte-arr]
  (vec (map byte byte-arr)))

(transduce
  (comp
    ;; Step 1: Add the applicable types (xforms) to the map
    (map (fn [{:keys [size partitioned]}]
           {:size size, :partitioned partitioned, :xforms (get-applicable-types size)}))

    ;; Step 2: Map the type symbols to actual transformation functions
    (map (fn [{:keys [size partitioned xforms]}]
           {:size size,
            :partitioned partitioned,
            :xforms (map #(get-in type-data [% :xform]) xforms)}))


    ;; Step 3: Apply the xforms to the partitioned data
    #_(map (fn [{:keys [size partitioned xforms]}]
           {:size size,
            :transformed (map (fn [xf]
                                (transduce xf conj partitioned))
                              xforms)}))
    )
  conj
  (partition-all-sizes-lazy [1 2 4 8] test-bytes))

(defn check-types [elem]
  (println "Type is: " (type elem))
  elem) ; return the element back


(defn transduce-seq
  [xf partitioned]
  (let [result (transduce xf conj [] partitioned)
        numeric-result (filter number? result)] ; Remove non-numeric results
    (println "Debug result: " result)
    {:result (map byte-array->vec numeric-result)}))


(map (fn [{:keys [size partitioned xforms]}]
       (let [transformed-results (map (fn [xf]
                                        (transduce-seq xf partitioned))
                                      xforms)]
         {:size size, :transformed transformed-results}))
     (map (fn [{:keys [size partitioned]}]
            {:size size,
             :partitioned (vec partitioned), ; make sure partitioned is a vector
             :xforms (map #(get-in type-data [% :xform]) (get-applicable-types size))})
          (partition-all-sizes-lazy [1 2 4 8] test-bytes)))




(transduce
  (comp
    (map (fn [{:keys [size partitioned]}]
           {:size size, :partitioned (transduce (dynamic-xform-for-size size) conj [] partitioned)}))
    ;; ... other xforms
    )
  conj []
  (partition-all-sizes-lazy [1 2 4 8] test-bytes))

(byte->sint16 (byte-array [(byte 32) (byte -84)]))
(byte->sint16 [(byte 32) (byte -84)])

(comment (defn parse-bytes-into-types [bytes types]
  (let [byte-vec (vec bytes)
        type-fn (fn [type vec] (into [] (:xform (get type-data type)) vec))
        init-results (into {} (map (fn [t] [t []]) types))]
    (loop [idx 0
           result init-results]
      (if (>= idx (count byte-vec))
        (into {} (map (fn [[k v]] [k (flatten v)]) result)) ;; Flatten here
        (let [updated-result (reduce (fn [acc type]
                                       (let [size (:size (get type-data type 1))
                                             to-convert (subvec byte-vec idx (+ idx size))
                                             transformed (type-fn type to-convert)]
                                         (update acc type conj transformed))) result types)
              next-idx (+ idx (apply min (map (fn [t] (:size (get type-data t))) types)))]
          (recur next-idx updated-result)))))))

(def test-bytes (byte-array [(byte 32) (byte -84) (byte 32) (byte -84) (byte 32) (byte -84) (byte 32) (byte -84)]))

(comment (def results (parse-bytes-into-types test-bytes [:hex :utf-8 :uint8-be :sint8-be])))

(pprint results)

(def my-transducer (comp (map to-hex) (map hex-to-int)))

(transduce my-transducer conj [] (vec test-bytes))
