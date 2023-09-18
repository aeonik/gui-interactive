(ns hex-editor.binary-utils
  (:require [clojure.pprint :refer [pprint]]
            [ubergraph.core :as uber]
            [tupelo.core :as t])
  (:import (com.sun.jdi ShortValue)
           (java.nio ByteBuffer ByteOrder)))



(defn byte-seq [^InputStream is size]
  (let [ib (byte-array size)]
    ((fn step []
       (lazy-seq
         (let [n (.read is ib)]
           (when (not= -1 n)
             (let [cb (chunk-buffer size)]
               (dotimes [i size] (chunk-append cb (aget ib i)))
               (chunk-cons (chunk cb) (step))))))))))

(defn byte->hex [b]
  (format "%02X" b))

(defn hex->byte [hex-str]
  (-> hex-str
      (Integer/parseInt 16)
      (mod 256)
      (byte)))


(defn hex->uint [hex-str]
  (Integer/parseInt hex-str 16))


(defn hex->num [#^String s]
  (Integer/parseUnsignedInt s 16))
(defn hex->num [#^String s]
  (Integer/parseInt s 16))

(let [byte-value (byte 45)]
  (byte->hex byte-value))

(defn uint8 [x]
  (bit-and x 0xFF))

(defn uint [x]
  (Byte/toUnsignedInt x))

(defn value-of [x & [radix]]
  (.valueOf x radix))

 (defn big-endian->int [bytes]
   (reduce (fn [acc x] (+ (bit-shift-left acc 8) (uint8 x))) 0 bytes))

(defn little-endian->int [bytes]
  (big-endian->int (reverse bytes)))

(defn to-guid [bytes]
  ;; GUID conversion logic here
  )


(def type-conversions
  {:utf-8 ^{:bytes 1} #(str (char %))
   :uint8  ^{:bytes 1} #(bit-and % 0xFF)
   :int8   ^{:bytes 1} #(byte %)
   :hex    ^{:bytes 1} #(byte->hex %)
   :utf-16 ^{:bytes 2} #(str (char %))
   :short  ^{:bytes 2} #(short %)
   :char   ^{:bytes 2} #(char (bit-and % 0xFF))
   :int    ^{:bytes 4} #(int %)
   :uint   ^{:bytes 4} #(Byte/toUnsignedInt %)
   :float  ^{:bytes 4} #(float %)
   :double ^{:bytes 8} #(double %)
   :long   ^{:bytes 8} #(long %)
   :ulong  ^{:bytes 8} #(Byte/toUnsignedLong %) })


(char (long (byte-array [(byte 65) (byte 66)])))
"Execution error (ClassCastException) at hex-editor.binary-utils/eval27026 (binary_utils.clj:1).
class [B cannot be cast to class java.lang.Number ([B and java.lang.Number are in module java.base of loader 'bootstrap'"
(byte->hex (byte 114))
(byte->hex (byte 190))
(hex->byte "20")
(hex->uint "")

(def test-bytes (byte-array [(byte 32) (byte -84) (byte 32) (byte -84) (byte 32) (byte -84) (byte 32) (byte -84)]))

;; Example Output after parsing test-bytes
=> :hex ["20" "AC" "20" "AC" "20" "AC" "20" "AC"]
=> :utf-8 [" " :invalid " " :invalid " " :invalid " " :invalid]
=> :utf-16 [€ € € €]
=> :uint8-be [32 172 32 172 32 172 32 172]
=> :sint8-be [32 -84 32 -84 32 -84 32 -84]
=> :uint16-be [8364 8364 8364 8364]
=> :sint16-be [8364 8364 8364 8364]
=> :uint16-le [44064 44064 44064 44064]
=> :sint16-le [-21472 -21472 -21472 -21472]
=> :uint32-be [548151468 548151468]
=> :sint32-be [548151468 548151468]
=> :uint32-le [2887822368 2887822368]
=> :sint32-le [-1407144928 -1407144928]
=> :uint64-be [2354292628862541996]
=> :sint64-be [2354292628862541996]
=> :uint64-le [12403102630105099296]
=> :sint64-le [-6043641443604452320]

(hex->uint "20AC20AC20AC20AC")
(hex->byte "20AC")

(byte->hex (byte 32))
(byte->hex (byte 114))
(map (:utf-16 type-conversions) [(byte 20)])
(map (:utf-16 type-conversions) [(byte 20) (byte 30)])
(map (:int type-conversions) [(byte 65) (byte 0) (byte 0) (byte 0)])
(map (:int type-conversions) (byte-array [(byte 65) (byte 0) (byte 0) (byte 0)]))

(map (:hex type-conversions) (byte-array [(byte 65) (byte 0) (byte 0) (byte 0)]))

((:utf-16 type-conversions) (byte 65))

((:int type-conversions) (byte 65))

((:short type-conversions) (byte 65))


((:long type-conversions) (byte 65))
((:int type-conversions) (byte-array [65 0 0 0]))
"Execution error (ClassCastException) at hex-editor.binary-utils/fn (form-init363557813137182152.clj:42)
class [B cannot be cast to class java.lang.Character ([B and java.lang.Character are in module java.base of loader 'bootstrap'"

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
    (let [partitioned-bytes (partition (-> f meta :bytes) bytes)]
      (map f partitioned-bytes))))

(defn apply-conversions [conv-map type bytes]
  (let [f (get conv-map type)]
    (if f
      (do
        (let [applied-bytes (apply-fn f bytes)]
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

(defn parse-bytes-into-types [bytes types]
  (loop [bytes bytes
         types types
         result []]
    (if (empty? bytes)
      result
      (let [type (first types)
            type-info (get type-conversions type)
            size (:bytes (meta type-info))
            to-convert (byte-array (take size bytes))]
        (recur (drop size bytes)
               (rest types)
               (conj result (type-info to-convert)))))))


; Usage
(parse-bytes-into-types (byte-array [(byte 65) (byte 66) (byte 67) (byte 0) (byte 0) (byte 0)]) [:utf-8 :utf-8 :utf-8 :int])
