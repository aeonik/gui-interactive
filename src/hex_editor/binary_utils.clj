(ns hex-editor.binary-utils
  (:require [clojure.pprint :refer [pprint]])
  (:import (com.sun.jdi ShortValue)
           (java.nio ByteBuffer ByteOrder)))

(defn uint8 [x]
  (bit-and x 0xFF))

(defn int8 [x]
  (byte x))

(defn uint [x]
  (Byte/toUnsignedInt x))

(defn ulong [x]
  (.toUnsignedLong x))

(defn value-of [x & [radix]]
  (.valueOf x radix))

 (defn big-endian->int [bytes]
   (reduce (fn [acc x] (+ (bit-shift-left acc 8) (uint8 x))) 0 bytes))

(defn little-endian->int [bytes]
  (big-endian->int (reverse bytes)))

(defn to-bool [x]
  (not (zero? x)))

(defn to-ascii [x]
  (let [ch (char (uint8 x))]
    (if (<= 0x20 ch 0x7E)  ; ASCII printable characters
      ch
      :invalid)))

(defn to-utf-8 [bytes]
  (String. bytes "UTF-8"))

(defn to-date [timestamp]
  (java.util.Date. (* 1000 timestamp)))

(defn to-guid [bytes]
  ;; GUID conversion logic here
  )

"This needs to be fixed to handle sequences of bytes."
(def type-conversions
  {:utf-16 ^{:bytes 1} #(str (char %))
   :uint8  ^{:bytes 1} #(bit-and % 0xFF)
   :int8   ^{:bytes 1} #(byte %)
   :short  ^{:bytes 2} #(short %)
   :char   ^{:bytes 2} #(char (bit-and % 0xFF))
   :int    ^{:bytes 4} #(int %)
   :uint   ^{:bytes 4} #(Byte/toUnsignedInt %)
   :float  ^{:bytes 4} #(float %)
   :double ^{:bytes 8} #(double %)
   :long   ^{:bytes 8} #(long %)
   :ulong  ^{:bytes 8} #(Byte/toUnsignedLong %)
   :hex    ^{:bytes 1} #()


(map (:utf-16 type-conversions) [65 100])
(map (:int type-conversions) [(byte 65) (byte 0) (byte 0) (byte 0)])
(map (:int type-conversions) (byte-array [(byte 65) (byte 0) (byte 0) (byte 0)]))

((:utf-16 type-conversions) (byte 65))
((:int type-conversions) (byte 65))
((:short type-conversions) (byte 65))
((:long type-conversions) (byte 65))
((:int type-conversions) (byte-array [65 0 0 0]))

(defn swap-endianness [bs]
  (reverse bs))

(defn endian-wrapper [f endian]
  (fn [x]
    (let [bytes (if (= endian :little) (swap-endianness x) x)]
      (f bytes))))

(defn add-endianess [conv-map endian]
  (into {} (for [[k v] conv-map]
             [k (endian-wrapper v endian)])))

(def little-endian-conversions (add-endianess type-conversions :little))

(defn apply-fn [f bytes]
  (do
    (println "Inside apply-fn: Bytes received:" bytes)
    (let [partitioned-bytes (partition (-> f meta :bytes) bytes)]
      (println "Partitioned bytes:" partitioned-bytes)
      (map f partitioned-bytes))))

(defn apply-conversions [conv-map type bytes]
  (let [f (get conv-map type)]
    (if f
      (do
        (println "Bytes before apply-fn:" bytes)
        (let [applied-bytes (apply-fn f bytes)]
          (println "Function to apply:" f)
          (println "Applied bytes:" applied-bytes)
          applied-bytes))
      (throw (Exception. (str "Unknown type: " type))))))

(defn single-or-seq [f x]
  (if (sequential? x)
    (if (= 1 (count x))
      (f (first x))
      (map f x))
    (f x)))

(def wrapped-conversions
  (into {} (map (fn [[k f]]
                  [k (fn [x] (single-or-seq f x))])
                type-conversions)))

(defn parse-bytes [bytes]
  (reduce (fn [acc [k _]]
            (assoc acc k ((get wrapped-conversions k) bytes)))
          {}
          type-conversions))

(parse-bytes [(byte 65) (byte 66) (byte 67)])

(apply-conversions little-endian-conversions :int [(byte 65) (byte 0) (byte 0) (byte 0)])
