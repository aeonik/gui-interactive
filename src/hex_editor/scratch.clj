(ns hex-editor.scratch)

(defn byte-seq [^bytes arr size]
  (let [arr-len (alength arr)]
    ((fn step [start]
       (lazy-seq
         (when (< start arr-len)
           (let [end (min (+ start size) arr-len)
                 sub-arr (subvec (vec arr) start end)]
             (cons sub-arr (step end)))))) 0)))

(defn partition-all-sizes-lazy [sizes coll]
  (let [byte-coll (byte-array coll)]
    (mapv (fn [size]
            {:size size,
             :partitioned (byte-seq byte-coll size)})
          sizes)))

(defn demo []
  (let [sizes [2 3]
        coll (byte-array [0 1 2 3 4 5 6 7 8 9])]
    (partition-all-sizes-lazy sizes coll)))

(demo)

(first (:partitioned (first (take 1 (demo)))))
(type (first (:partitioned (first (take 1 (demo))))))

(defn valid-utf8-seq? [buffer]
  (let [b0 (first buffer)]
    (cond
      (< b0 0x80) (= (count buffer) 1)
      (< b0 0xE0) (= (count buffer) 2)
      (< b0 0xF0) (= (count buffer) 3)
      :else (= (count buffer) 4))))

(defn decode-utf8 [buffer]
  "Currently working in demo, but broken in pipeline because this is a higher order transducer"
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

(defn utf8-transducer-stateless [rf]
  (let [step (fn [result byte]
               (let [{:keys [buffer str-acc]} result
                     new-buffer (conj buffer byte)
                     [utf-char new-buffer*] (if (valid-utf8-seq? new-buffer)
                                              [(decode-utf8 new-buffer) []]
                                              [nil new-buffer])]
                 (rf result {:buffer new-buffer* :str-acc (if utf-char (conj str-acc utf-char) str-acc)})))]
    (fn
      ([] (rf {} {}))
      ([result] (apply str (:str-acc result)))
      ([result input] (step result input)))))

(defn transduce-utf8-stateless [input]
  (->> input
       (transduce (utf8-transducer-stateless merge) (fn [] {:buffer [] :str-acc []}) {:buffer [] :str-acc []})
       :str-acc
       (apply str)))

(defn example-usage []
  (let [test-input [0xC3 0xA9 0xC3 0xA9]
        res (transduce-utf8-stateless test-input)]
    (println "Result:" res)))

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(comment (example-usage))
;; ```none
;; Execution error (ClassCastException) at hex-editor.scratch/transduce-utf8-stateless (form-init14082250683920690850.clj:71).
;; class java.lang.String cannot be cast to class clojure.lang.IFn (java.lang.String is in module java.base of loader 'bootstrap';
;; clojure.lang.IFn is in unnamed module of loader 'app')
;;```