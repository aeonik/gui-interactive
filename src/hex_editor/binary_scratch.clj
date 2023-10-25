(ns hex-editor.binary-scratch
  (:require [criterium.core :as crit]
            [injest.path :refer [+> +>> x>> =>>]]))

(def test-bytes (byte-array [(byte 32) (byte 127) (byte 64) (byte -82) (byte 2) (byte 40) (byte 32) (byte -84)]))

(partition-all 1 test-bytes)

(partition-all 2 test-bytes)
(Integer/toBinaryString (first test-bytes))
(Integer/toBinaryString (Byte/toUnsignedInt (first test-bytes)))
(Integer/toBinaryString (bit-shift-left (Byte/toUnsignedInt (first test-bytes)) 1))

(defn bit-wise-representation
  [b]
  (Integer/toBinaryString (Byte/toUnsignedInt b)))

(defn slide-and-tag
  [bytes]
  (loop [bs bytes
         result []]
    (if (< (count bs) 2)
      result
      (let [byte1 (first bs)
            byte2 (second bs)
            bits1 (bit-wise-representation byte1)
            bits2 (bit-wise-representation byte2)
            combined-bits (bit-or (bit-shift-left (Byte/toUnsignedInt byte1) 8)
                                  (Byte/toUnsignedInt byte2))]
        (recur (rest bs)
               (conj result {:byte1 bits1 :byte2 bits2 :combined combined-bits}))))))
(slide-and-tag test-bytes)

(defn tag-byte
  [b]
  {:byte b :bit-wise (bit-wise-representation b)})

(defn slide-and-tag-lazy
  [byte-stream]
  (when-let [s (seq byte-stream)]
    (lazy-seq
      (cons (tag-byte (first s))
            (slide-and-tag-lazy (rest s))))))
(take 4 (slide-and-tag-lazy test-bytes))

(defn slide-and-chunk
  [n byte-stream]
  (let [bit-queue (java.util.LinkedList.)
        step (fn step [s]
               (lazy-seq
                 (when-let [seq-s (seq s)]
                   (doseq [b seq-s]
                     (loop [bits-left 8
                            bit-mask 128]
                       (when (> bits-left 0)
                         (.add bit-queue (if (zero? (bit-and b bit-mask)) 0 1))
                         (recur (dec bits-left) (bit-shift-right bit-mask 1)))))
                   (when (>= (.size bit-queue) n)
                     (let [chunk (doall (take n bit-queue))]
                       (.poll bit-queue)
                       (cons chunk (step seq-s)))))))]
    (step byte-stream)))

(take 5 (slide-and-chunk 16 test-bytes))

(def random-bytes (byte-array (repeatedly 10000 #(byte (- (rand-int 256) 128)))))

^{:nextjournal.clerk/viewer (assoc nextjournal.clerk.viewer/string-viewer :page-size 500) :nextjournal.clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench (doall (take 500 (slide-and-chunk 16 random-bytes)))))

(defn byte-to-bits [b]
  (mapv #(bit-and 1 (bit-shift-right b %)) (range 7 -1 -1)))

(defn slide-and-chunk-xform [n]
  (let [q (volatile! [])]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [bits (byte-to-bits input)]
           (vswap! q into bits)
           (loop []
             (when (>= (count @q) n)
               (let [chunk (subvec @q 0 n)]
                 (vswap! q subvec n)
                 (rf result chunk)
                 (recur))))))))))

(defn slide-and-chunk2 [n bytes]
  (sequence (comp (slide-and-chunk-xform n)) bytes))

^{:nextjournal.clerk/viewer (assoc nextjournal.clerk.viewer/string-viewer :page-size 500) :nextjournal.clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench (doall (take 500 (slide-and-chunk2 16 random-bytes)))))

(defn slide-and-chunk-xform2 [n]
  (let [q (atom [])]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [bits (byte-to-bits input)]
           (swap! q into bits)
           (loop [acc result]
             (let [qq @q]
               (if (>= (count qq) n)
                 (let [chunk (take n qq)]
                   (swap! q #(drop n %))
                   (recur (rf acc chunk)))
                 acc)))))))))

(defn slide-and-chunk2 [n bytes]
  (eduction (slide-and-chunk-xform2 n) bytes))

^{:nextjournal.clerk/viewer (assoc nextjournal.clerk.viewer/string-viewer :page-size 500) :nextjournal.clerk/auto-expand-results? true}
(let [random-bytes (take 1000 (repeatedly #(rand-int 256)))]
  (with-out-str (crit/quick-bench (doall (take 500 (slide-and-chunk2 16 random-bytes))))))

(defn byte-to-bits-xform []
  (mapcat byte-to-bits))

(defn consume-bits [n q rf acc]
  (let [qq @q]
    (if (>= (count qq) n)
      (let [chunk (take n qq)]
        (swap! q #(drop n %))
        (recur n q rf (rf acc chunk)))
      acc)))

(defn transduce-bits [n q rf]
  (fn
    ([] (rf))
    ([result] (rf result))
    ([result input]
     (swap! q into (byte-to-bits input))
     (consume-bits n q rf result))))

(defn slide-and-chunk-xform3 [n]
  (let [q (atom [])]
    (fn [rf]
      (transduce-bits n q rf))))

(defn slide-and-chunk3 [n bytes]
  (eduction (slide-and-chunk-xform3 n) bytes))

^{:nextjournal.clerk/viewer (assoc nextjournal.clerk.viewer/string-viewer :page-size 500) :nextjournal.clerk/auto-expand-results? true}
(let [random-bytes (take 1000 (repeatedly #(rand-int 256)))]
  (with-out-str (crit/quick-bench (doall (take 500 (slide-and-chunk3 16 random-bytes))))))

(defn byte-to-bits2 [b]
  (let [bits (map #(bit-and 1 (bit-shift-right b %)) (range 7 -1 -1))]
    bits))

(defn consume-bits [n q rf acc]
  (let [qq @q]
    (if (>= (count qq) n)
      (let [chunk (take n qq)]
        (swap! q #(drop n %))
        (recur n q rf (rf acc chunk)))
      acc)))

(defn transduce-bits2 [n q rf]
  (fn
    ([] (rf))
    ([result] (rf result))
    ([result input]
     (swap! q into (byte-to-bits2 input))
     (consume-bits n q rf result))))

(defn slide-and-chunk-xform3 [n]
  (let [q (atom [])]
    (fn [rf]
      (transduce-bits2 n q rf))))

(defn slide-and-chunk4 [n bytes]
  (eduction (slide-and-chunk-xform3 n) bytes))

(let [random-bytes (take 1000 (repeatedly #(rand-int 256)))]
  (doall (take 500 (slide-and-chunk3 16 random-bytes))))

(x>> (repeatedly #(rand-int 256))
     (mapcat byte-to-bits)
     (take 2))


