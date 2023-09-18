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