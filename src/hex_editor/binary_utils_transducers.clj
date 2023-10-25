(ns hex-editor.binary-utils-transducers
  (:require [clojure.pprint :refer [pprint]])
  (:import [java.nio ByteBuffer ByteOrder]))

;; I found this library that already does the binary conversions via graph path: https://github.com/clj-commons/byte-streams

;; It doesn't look like it does transducers though

(defn logging-xform [rf]
  " Transducer for logging the input to the reduce function. "
  (fn
    ([] (rf))
    ([result] (rf result))
    ([result input]
     (do
       (println " Debug xform: " input)
       (rf result input)))))

(defn reversing-txf [rf]
  " Transducer for reversing the input to the reduce function. Used for endian swapping "
  (fn
    ([] (rf))
    ([result] (rf result))
    ([result input] (rf result (map reverse input)))))

(def test-bytes
  " Test bytes it is the € symbol repeated 4 times. "
  (byte-array [(byte 32) (byte -84) (byte 32) (byte -84) (byte 32) (byte -84) (byte 32) (byte -84)]))
(take 1 test-bytes)

(defn partition-all-sizes-lazy [sizes coll]
  (mapv (fn [size]
          {:size size,
           :partitioned (mapv vec (partition-all size coll))})
        sizes))

(partition-all-sizes-lazy [1 2 4 8] test-bytes)

(time (transduce (map :partitioned) conj () (partition-all-sizes-lazy [1 2 4 8] test-bytes)))
(time (transduce (map :partitioned) conj [] (partition-all-sizes-lazy [1 2 4 8] test-bytes)))

;; This is a simple transducer example
(transduce
  (comp
    (map :partitioned)
    reversing-txf)
  conj []
  (partition-all-sizes-lazy [1 2 4 8] test-bytes))

;; Simple example showing endian change
(map reverse (partition-all-sizes-lazy [1 2 4 8] test-bytes))

;; Getting values from the stream
(get (first (partition-all-sizes-lazy [1 2 4 8] test-bytes)) :partitioned)

(defn valid-utf8-seq? [buffer]
  (let [b0 (first buffer)]
    (cond
      (< b0 0x80) (= (count buffer) 1)
      (< b0 0xE0) (= (count buffer) 2)
      (< b0 0xF0) (= (count buffer) 3)
      :else (= (count buffer) 4))))

(defn decode-utf8 [buffer]
  " Currently working in demo, but broken in pipeline because this is a higher order transducer "
  (let [b0 (first buffer)]
    (cond
      (< b0 0x80) (str (char b0))
      (< b0 0xE0) (str (char (+ (bit-and b0 0x1F)
                                (bit-shift-left (bit-and (nth buffer 1) 0x3F) 6))))
      (< b0 0xF0) (str (char (+ (bit-and b0 0x0F)
                                (bit-shift-left (bit-and (nth buffer 1) 0x3F) 6)
                                (bit-shift-left (bit-and (nth buffer 2) 0x3F) 0))))
      :else (let [code-point (+ (bit-shift-left (bit-and b0 0x07) 18)
                                (bit-shift-left (bit-and (nth buffer 1) 0x3F) 12)
                                (bit-shift-left (bit-and (nth buffer 2) 0x3F) 6)
                                (bit-and (nth buffer 3) 0x3F))]
              (if (< code-point 0x10000)
                (str (char code-point))
                (String/valueOf (Character/toChars code-point)))))))

(defn utf8-transducer
  ([] identity)
  ([rf]
    (let [buffer (volatile! [])]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result byte]
         (let [b (byte byte)]
           (if (< b 0x80)
             (rf result (char b))
             (do
               (vswap! buffer conj b)
               (when (valid-utf8-seq? @buffer)
                 (rf result (decode-utf8 @buffer))
                 (vreset! buffer []))
               result))))))))

(let [buffer (volatile! [0xF0 0x9F 0x92 0x83])]
  (when (valid-utf8-seq? @buffer)
    (println (decode-utf8 @buffer))))



(defn make-to-hex-transducer [f]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result x]
       (let [hex-val (if (sequential? x)
                       (map f x)
                       (f x))]
         (rf result hex-val))))))

(defn to-hex
  " More robust to-hex conversion. "
  ([x]
   (cond
     (number? x) (format " %02X " x)
     (vector? x) (map #(format " %02X " %) x)
     (sequential? x) (map #(to-hex %) x)
     :else (throw (ex-info " Unsupported type " {:type (type x)})))))

(defn demo-hex []
  (let [ints [15 16 31 32 63 64]
        xf-hex (make-to-hex-transducer to-hex)]
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
     (println " Debug BS Data Type: " (type bs))
     (when debug-message
       (println " Debug " debug-message " : " bs))
     (if (= byte-length (count bs))
       (conj result (f bs))
       (throw (IllegalArgumentException. (str " Expected a byte sequence of length " byte-length ", got: " bs)))))))

(defn make-utf8-transducer []
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result byte]
       (let [utf8-val (try (String. (byte-array [byte]) " UTF-8 ")
                           (catch Exception _ :invalid))]
         (rf result utf8-val))))))

(defn byte->uint8 [bs]
  (byte bs))

(defn byte->utf8 [bs]
  (try (String. (byte-array bs) " UTF-8 ")
       (catch Exception _ :invalid)))

(defn byte->uint16 [bs]
  (-> (.getShort (ByteBuffer/wrap (byte-array bs))) int))

(defn byte->sint16 [bs]
  (-> (.getShort (ByteBuffer/wrap (byte-array bs))) int))

(defn byte->uint32 [bs]
  (.getInt (ByteBuffer/wrap (byte-array bs))))

(defn byte->sint32 [bs]
  (println " Debug byte->sint32: " bs)
  (int (.getInt (ByteBuffer/wrap (byte-array bs)))))

(defn byte->uint64 [bs]
  (println " Debug byte->uint64: " bs)
  (.getLong (ByteBuffer/wrap (byte-array bs))))

(defn byte->sint64 [bs]
  (println " Debug byte->sint64: " bs)
  (long (.getLong (ByteBuffer/wrap (byte-array bs)))))


(defn transduce-bytes [xf input]
  (transduce xf conj [] input))

(defn demo []
  (let [bytes (mapv byte-array [[0] [2] [4]])
        xf-uint8 (map (make-byte->int-transducer byte->uint8 1 " byte->uint8 "))]
    (println (transduce xf-uint8 conj [] bytes))))

(demo)
(comment :uint8 {:xform (map #(map (fn [x] (bit-and x 0xFF)) %)), :size 1})

(def type-data
  {:hex     {:xform (map to-hex), :size 1}
   :utf-8   {:xform (map byte->utf8), :size 1}
   :uint8   {:xform (map (make-byte->int-transducer 1 byte->uint8 " uint8 ")), :size 1}
   :sint8   {:xform (map identity), :size 1}
   :uint16  {:xform (map byte->uint16), :size 2}
   :sint16  {:xform (map byte->sint16), :size 2}
   :uint32  {:xform (map byte->uint32), :size 4}
   :sint32  {:xform (map byte->sint32), :size 4}
   :uint64  {:xform (map byte->uint64), :size 8}
   :sint64  {:xform (map byte->sint64), :size 8}})


(defn to-uint [bits byte]
  (bit-and (bit-shift-left (bit-and byte 0xFF) bits)))

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

(def size 4)
(defn get-applicable-types [size]
  (filter #(= (:size (get type-data %)) size) (keys type-data)))

(def partitioned-bytes (partition-all-sizes-lazy [1 2 4 8] test-bytes))

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
               logging-xform
               (map #(assoc % :xforms (get-applicable-types (:size %))))
               logging-xform
               (dynamic-xform)
               logging-xform)
             merge
             (partitioned-xforms sizes coll)))

(use 'clojure.tools.trace)

(def result (main-pipeline [1 2 4 8] test-bytes))
(pprint result)