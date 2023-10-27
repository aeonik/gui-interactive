(ns notebooks.binary-transducer
  (:require [clojure.string :as str]
            [injest.path :refer [+> +>> x> x>> => =>> |> |>>]]
            [injest.state :as i.s]
            [net.cgrand.xforms :as x]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [criterium.core :as crit]
            [zprint.core :as z]
            [tesser.core :as t]
            [clojure.core.async :as async :refer [chan >!! <! go-loop]]))

(i.s/regxf! 'net.cgrand.xforms/transjuxt)
(i.s/regxf! 'net.cgrand.xforms/some)
(i.s/regxf! 'net.cgrand.xforms/into)
(i.s/regxf! 'net.cgrand.xforms/multiplex)
(i.s/regxf! 'net.cgrand.xforms/reduce)
(i.s/regxf! 'net.cgrand.xforms/window)
(i.s/regxf! 'net.cgrand.xforms/partition)
(i.s/regpxf! 'tesser.core/tesser)
(i.s/regpxf! 'tesser.core/fold)


^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(clerk/serve! {:host "aeonik.connett.io" :browse true :watch-paths ["notebooks" "src"]})

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(comment (do
           (swap! i.s/par-transducables (fn [current-set] (disj current-set 'clojure.core/flatten)))
           (swap! i.s/transducables (fn [current-set] (disj current-set 'clojure.core/flatten)))
           (swap! i.s/par-transducables (fn [current-set] (disj current-set 'net.cgrand.xforms/reduce)))
           (swap! i.s/par-transducables (fn [current-set] (disj current-set 'clojure.core/reduce)))))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(def zprint-code-viewer
  {:name         `zprint-code-viewer
   :render-fn    'nextjournal.clerk.render/render-code
   :transform-fn (comp v/mark-presented
                       #(update-in % [:nextjournal/render-opts :language] (fn [lang] (or lang "clojure")))
                       (clerk/update-val (fn [v] (str/trim (with-out-str (z/zprint v {:map {:comma? true :indent 0 :justify? true}}))))))})


;; # ⚧️ Binary Transducer
;; Take binary data, and fan it out into multiple representations.
;;
;; For example, let's say you have a transducer that converts a `Byte` -> `Hex`.
;;
;; You could combine that with another transducer that converts `Hex` -> `Integer` all without creating intermediate collections.
;;
;; Such a pipeline could then be agnostic of source and sink, data could come from:
;; * Database
;; * Channels
;; * Streams
;; * Files
;;
;; Each transducer can be tested individually, and they can be combined in different orders to achieve different effects.
;;
;; Bigger idea: register all conversion in paths into a multigraph and then use that to generate a UI for exploring the data.
;;
;; Different paths through the graph can also be benchmarked and the graph's edge weights can be updated to reflect the performance of each conversion.
;;
;; ## Convert a byte to a binary representation
;; E.g. (byte 0) -> [0 0 0 0 0 0 0 0]
^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn byte-to-bits [b]
  (map #(bit-and 1 (bit-shift-right b %)) (range 7 -1 -1)))

;; ## Setup test bytes
;; ### A sequence of bytes with a mix of interesting values:
;; Java, and thus Clojure, use `signed integers` by default.

;; Therefore the range of a `signed integer` in `byte` form is `-128` to `127`.

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(def interesting-bytes (byte-array [(byte 0)
                                    (byte -128)
                                    (byte 127)
                                    (byte 2)
                                    (byte 32)
                                    (byte 40)
                                    (byte 32)
                                    (byte -84)]))

;; ### A sequence of 10k random bytes:
;; This will come in handy for performance testing.

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(def random-bytes (byte-array (repeatedly 10000 #(byte (- (rand-int 256) 128)))))

;; ## Pipe bytes into bit transducer pipeline.
;; This expression uses injest macro to automatically parralelize and transducify the computation.
^{::clerk/viewer v/code-viewer ::clerk/auto-expand-results? true}
(x>> interesting-bytes
     (mapcat byte-to-bits)
     (partition 8))

;; You can see the mapping more clearly if the original `integers` are `zipmapped` with the binary representation.
;;
;; First we need a way to parsing the Bytes back into Integers for display.
(x>> interesting-bytes
     (map byte))

;; Now we can zipmap the original bytes (converted to integers) with the binary representation.
^{::clerk/viewer zprint-code-viewer ::clerk/auto-expand-results? true}
(x>> interesting-bytes
     (mapcat byte-to-bits)
     (partition 8)
     (zipmap (map byte interesting-bytes)))


;; #### Performance Test of `partition` against 8 `interesting-bytes`:
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (dorun
                  (x>> interesting-bytes
                       (mapcat byte-to-bits)
                       (partition 8)))))

;; #### Performance Test of `partition` against 10k `random-bytes` with `x>>`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (dorun
                  (x>> random-bytes
                       (mapcat byte-to-bits)
                       (partition 8)))))

;; #### Performance Test of `partition` against 10k `random-bytes` with `=>>`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (dorun
                  (=>> random-bytes
                       (mapcat byte-to-bits)
                       (partition 8)))))

;; The following expression will concatenate the binary representation of each byte into a single sequence.

;; Aka, a stream of 1s and 0s.

;; Then it will partition that sequence of sliding binary windows over the data.
(x>> interesting-bytes
     (mapcat byte-to-bits)
     (fn [coll]
       (map (fn [index]
              (partition index 1 coll))
            (range 2 16))))

;; Making a function for reuse.
^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn multi-partition [coll]
  (map (fn [index]
         (partition index 1 coll))
       (range 2 16)))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(i.s/regxf! 'notebooks.binary-transducer/multi-partition)
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(i.s/regpxf! 'notebooks.binary-transducer/multi-partition)
(x>> interesting-bytes
     (mapcat byte-to-bits)
     multi-partition)

;; Same idea as before, but using `cgrand.xforms/partition` instead of Clojure's `partition`.
^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn multi-partition-cgrand [coll]
  (map (fn [index]
         (into [] (x/partition index 1) coll))
       (range 2 16)))

(x>> interesting-bytes
     (mapcat byte-to-bits)
     (multi-partition-cgrand))

;; ### Transducers with channels

;; #### Transducer that converts Java Bytes to bits, and groups the bits into chucks of 8
^{::clerk/viewer v/code-viewer ::clerk/auto-expand-results? true}
^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(def my-byte-transducer
  (comp (mapcat byte-to-bits)
        (x/partition 8)))

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(def src-channel (chan 100))

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(def dest-channel (chan 100))

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(def captured-output (atom []))                             ;; Atom to hold channel data for Clerk

;; ##### Populate src-channel with 10 random bytes
^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(let [n 10]
  (go-loop [i 0]
    (when (< i n)
      (>!! src-channel (byte (- (rand-int 256) 128)))
      (recur (inc i)))))

;; ##### Setup pipeline with core.async
^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(async/pipeline 2
                dest-channel
                my-byte-transducer
                src-channel)

;; ### Read 10 times from some-dest-channel and capture it
^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(let [n 10]
  (go-loop [i 0]
    (when (< i n)
      (when-let [v (<! dest-channel)]
        (println "Transformed Data: " v)
        (swap! captured-output conj v))                     ;; Side-effect to capture output
      (recur (inc i)))))

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
^{::clerk/viewer zprint-code-viewer ::clerk/auto-expand-results? true}
@captured-output

;; #### Performance Test of `multi-partition` against 10k `random-bytes` with `x>>`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str
  (crit/quick-bench
    (dorun
      (x>> random-bytes
           (mapcat byte-to-bits)
           multi-partition))))

;; #### Performance Test of `multi-partition` against 10k `random-bytes` with `=>>`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str
  (crit/quick-bench
    (dorun
      (=>> random-bytes
           (mapcat byte-to-bits)
           multi-partition))))


(require '[clj-async-profiler.core :as prof])
;; ## The Sum of All Bits ☢️
;; This technique just concatenates all the binary representations into a single sequence.
(=>> interesting-bytes
     (mapcat byte-to-bits)
     multi-partition
     (mapcat concat)
     (mapcat concat)
     (x/reduce +))

;; This technique sums up the bits in each byte, then sums up the sums.
(=>> interesting-bytes
     (mapcat byte-to-bits)
     multi-partition
     (map (fn [x] (map #(reduce + %) x)))                   ;; sum up each inner list
     (map #(reduce + %))                                    ;; sum up each outer list
     (x/reduce +))                                          ;; sum up all the sums

;; Using partial application rather than anonymous functions
(=>> interesting-bytes
     (mapcat byte-to-bits)
     multi-partition
     (map (partial map (partial reduce +)))                 ;; sum up each inner list
     (map (partial reduce +))                               ;; sum up each outer list
     (x/reduce +))                                          ;; sum up all the sums


(prof/profile (dotimes [i 10] (=>> interesting-bytes
                                   (mapcat byte-to-bits)
                                   multi-partition
                                   (map (partial map (partial reduce +)))
                                   (map (partial reduce +))
                                   (x/reduce +))))

(prof/serve-ui 9090)


^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
;; Trying to get more cgrand functions in the code.
;; I am currently having a lot of issues getting some of these to work.
(comment (=>> interesting-bytes
              (mapcat byte-to-bits)
              (fn [coll]
                (map (fn [index]
                       (sequence (x/partition index 1 coll)))
                     (range 2 16)))
              (map (fn [x] (map #(reduce + %) x)))          ;; sum up each inner list
              (map #(reduce + %))                           ;; sum up each outer list
              (x/reduce +)))                                ;; sum up all the sums


;; ## Performance tests:

;; ###
;; These performance tests all use the 10k `random-bytes` sequence.
;; It is summing up 9,518,880 million individual 1s and 0s (due to the sliding window technique).
;; The sum is 4,778,384, but will be omitted from the performance tests.
(=>> random-bytes
     (mapcat byte-to-bits)
     multi-partition
     flatten
     count)

(=>> random-bytes
     (mapcat byte-to-bits)
     multi-partition
     (map (partial map (partial reduce +)))                 ;; sum up each inner list
     (map (partial reduce +))                               ;; sum up each outer list
     (x/reduce +))                                          ;; sum up all the sums

;; ### `xforms/reduce` + nested `reduce`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     (map (fn [x] (map #(reduce + %) x)))   ;; sum up each inner list
                     (map #(reduce + %))                    ;; sum up each outer list
                     (x/reduce +))))                        ;; sum up all the sums

;; ### Trying `(apply +)` instead of `(reduce +)`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     (map (fn [x] (map #(apply + %) x)))    ;; sum up each inner list
                     (map #(apply + %))                     ;; sum up each outer list
                     (x/reduce +))))                        ;; sum up all the sums

;; ### Trying `(apply +)` instead of `(x/reduce +)`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     (map (fn [x] (map #(apply + %) x)))    ;; sum up each inner list
                     (map #(apply + %))                     ;; sum up each outer list
                     (apply +))))                           ;; sum up all the sums

;; ### Trying `(apply +)` instead of `(x/reduce +)` with `partial`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     (map (partial map (partial apply +)))  ;; sum up each inner list
                     (map (partial apply +))                ;; sum up each outer list
                     (apply +))))                           ;; sum up all the sums

;; ### `xforms/reduce` + `flatten`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     (mapcat concat)
                     (mapcat concat)
                     (x/reduce +))))                        ;; sum up all the sums

;; ### `clojure.core/reduce` + nested `reduce`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     (map (fn [x] (map #(reduce + %) x)))   ;; sum up each inner list
                     (map #(reduce + %))                    ;; sum up each outer list
                     (x/reduce +))))                        ;; sum up all the sums

;; ### `clojure.core/reduce` + nested `reduce` with `partial`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     (map (partial map (partial reduce +))) ;; sum up each inner list
                     (map (partial reduce +))               ;; sum up each outer list
                     (reduce +))))                          ;; sum up all the sums

;; ### `clojure.core/reduce` + `mapcat`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     (mapcat concat)
                     (mapcat concat)
                     (reduce +))))                          ;; sum up all the sums

;; ### `clojure.core/reduce` + `flatten`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     flatten
                     (x/reduce +))))                        ;; sum up all the sums

;; ### `(apply +)` instead of `(x/reduce +)` with `partial`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     (map (partial map (partial apply +)))  ;; sum up each inner list
                     (map (partial apply +))                ;; sum up each outer list
                     (x/reduce +))))                        ;; sum up all the sums


;; ### `(pmap)` with `(x/reduce +)` with `partial`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     (pmap (partial pmap (partial reduce +))) ;; sum up each inner list
                     (pmap (partial reduce +))              ;; sum up each outer list
                     (x/reduce +))))                        ;; sum up all the sums


;;; ### `|>>` Parallel pipeline
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (|>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     (pmap (partial pmap (partial reduce +))) ;; sum up each inner list
                     (pmap (partial reduce +))              ;; sum up each outer list
                     (x/reduce +))))                        ;; sum up all the sums

;; ### Comparing to tresser:
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     #(t/tesser % (t/fold + (t/mapcat concat))))))

;; #### Testing tressor with varying threading macros

;; `->>`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     #(->> (t/mapcat concat)
                           (t/fold +)
                           (t/tesser %)))))

;; `+>>` Enhanced path threading (shouldn't make a difference)
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     #(+>> (t/mapcat concat)
                           (t/fold +)
                           (t/tesser %)))))

;; `x>>` Auto compose transudcers, not sure if this makes any difference
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     #(x>> (t/mapcat concat)
                           (t/fold +)
                           (t/tesser %)))))

;; `|>>` Testing parallel pipeline rather than parallel fold, theoretically should be slower
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (|>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     #(x>> (t/mapcat concat)
                           (t/fold +)
                           (t/tesser %)))))


;; Note: This is 43.6752443 nanoseconds of total processing per bit.

;; ### More complicated version using tesser custom fold logic
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition                        ;; Tesser isn't good at partitioning so leaving this out
                     #(t/tesser % (t/fold {:reducer-identity  (constantly [0 0]) ; [inner-sum, outer-sum]
                                           :reducer           (fn [[inner-sum outer-sum] inner-list]
                                                                [(+ inner-sum (reduce + inner-list))
                                                                 outer-sum])
                                           :post-reducer      (fn [[inner-sum outer-sum]]
                                                                (+ inner-sum outer-sum))
                                           :combiner-identity (constantly 0)
                                           :combiner          (fn [outer-sum post-reducer-result]
                                                                (+ outer-sum post-reducer-result))
                                           :post-combiner     identity})))))

(comment (with-out-str (crit/quick-bench
                         (->> (t/mapcat byte-to-bits)
                              (t/tesser random-bytes)))))

;; Simple tesser usage example
;; The data to be processed is passed at the end of the pipeline.
(->> (t/mapcat seq)                                         ; explode strings into seqs of chars
     (t/set)
     (t/tesser [["meow" "mix"]]))

;; ### Testing `transjuxt` with `=>>`
(=>> [12 15]
     (mapcat byte-to-bits)
     multi-partition
     (mapcat concat))

(=>> [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]
     multi-partition)

(=>> [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]
     multi-partition
     (x/transjuxt [(comp (mapcat concat) (mapcat concat) (x/into []))])
     first
     first)

;; TODO: Figure out how to get `transjuxt` working with `injest` macros.
;; `transjuxt` may be the same as `=>>`
^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> random-bytes
                     (mapcat byte-to-bits)
                     multi-partition
                     (x/transjuxt [(comp (mapcat concat) (mapcat concat) (x/into []))])
                     first
                     first
                     (x/reduce +))))


;; ### Testing `multiplex` vs `=>>`
(=>> (range 3)
     (mapcat #(do [[:up (inc %)] [:down (dec %)]])))
(into [] (x/multiplex {:up (map inc) :down (map dec)}) (range 3))


;; ### Performance Testing `multiplex`

^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (=>> (range 10000)
                     (mapcat #(do [[:up (inc %)] [:down (dec %)]])))))

^{::clerk/viewer (assoc v/string-viewer :page-size 500) ::clerk/auto-expand-results? true}
(with-out-str (crit/quick-bench
                (into [] (x/multiplex {:up (map inc) :down (map dec)}) (range 10000))))

;; Turns out `multiplex` is probably not what I want. Each value gets each function applied to it in parallel...
(=>> [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]
     multi-partition
     (x/multiplex [(mapcat concat)]))


;; Here I am trying to clean up the nested reduce logic.

;;  Trying to write my own sliding window function:
(defn sliding-windows
  [sizes coll]
  (let [init-windows (mapv (fn [size] (vec (repeat size nil))) sizes)
        state        (atom {:windows init-windows :input coll})]
    (fn step []
      (lazy-seq
        (when-let [s (seq @(:input state))]
          (swap! state (fn [{windows :windows input :input}]
                         {:windows (mapv (fn [w]
                                           (conj (subvec w 1) (first input)))
                                         windows)
                          :input   (rest input)}))
          (cons (mapv vec @(:windows state)) (step)))))))

(defn sliding-windows-tx [sizes]
  (fn [rf]
    (let [init-windows (mapv (fn [size] (vec (repeat size nil))) sizes)
          state        (atom {:windows init-windows})]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (swap! state (fn [{windows :windows}]
                        {:windows (mapv (fn [w]
                                          (conj (subvec w 1) input))
                                        windows)}))
         (rf result (mapv vec @(:windows state))))))))

(defn custom-window
  [n f invf]
  (fn [rf]
    (let [ring  (object-array n)
          vi    (volatile! (- n))
          vwacc (volatile! (f))]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc x]
         (let [i    @vi
               wacc @vwacc]                                 ; window accumulator
           (aset ring (if (neg? i) (+ n i) i) x)
           (if (neg? i)
             (do
               (vreset! vi (inc i))
               (rf acc (f (vreset! vwacc (f wacc x)))))
             (let [x' (aget ring i)]
               (aset ring i x)
               (vreset! vi (let [i (inc i)] (if (= n i) 0 i)))
               (rf acc (f (vreset! vwacc (f (invf wacc x') x))))))))))))
(defn windows
  "like partition-all with step of one but outputs window on each item"
  ([n]
   (custom-window n conj (fn [acc _] (subvec acc 1 n)))))

(i.s/regxf! 'notebooks.binary-transducer/windows)
(i.s/regpxf! 'notebooks.binary-transducer/windows)

(x>> interesting-bytes
     (map byte-to-bits)
     flatten
     ;; Need to drop the last item
     #_(x/window 8 conj concat)
     (windows 8))
(comment map-indexed
         (x))

(comment #(fn [coll] (let [index (range 2 3)]
                       (partition index 1 coll))))


^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn consume-bits [n q rf acc]
  (let [qq @q]
    (if (>= (count qq) n)
      (let [chunk (take n qq)]
        (swap! q #(drop n %))
        (recur n q rf (rf acc chunk)))
      acc)))

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn transduce-bits [n q rf]
  (fn
    ([] (rf))
    ([result] (rf result))
    ([result input]
     (swap! q into (byte-to-bits input))
     (consume-bits n q rf result))))

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn slide-and-chunk-xform [n]
  (let [q (atom [])]
    (fn [rf]
      (transduce-bits n q rf))))

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn slide-and-chunk [n bytes]
  (eduction (slide-and-chunk-xform n) bytes))

(x>> (repeatedly #(rand-int 256))
     (mapcat byte-to-bits)
     (take 16))

