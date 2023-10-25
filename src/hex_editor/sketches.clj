(ns hex-editor.sketches)

"Polyglot binary stream processor that can handle multiple types of data on-the-fly"

(comment (defn meta-transducer []
           "I don't like that I have to dispatch on size and type in my transducers. Might need a meta transducer"
           (fn [rf]
             (fn
               ([] (rf))
               ([result] (rf result))
               ([result bytes]
                (let [type-id (peek-type bytes)             ;; some logic to determine type from bytes
                      xform   (get-in type-data [type-id :xform])
                      size    (get-in type-data [type-id :size])]
                  ;; use only 'size' number of bytes for this transformation
                  (let [chunk             (take size bytes)
                        rest-bytes        (drop size bytes)
                        transformed-chunk (transduce xform conj [] chunk)]
                    (rf result {:transformed transformed-chunk :remaining rest-bytes}))))))))


"Eventually I want to I envision a gui that basically tries to decode a byte stream
as video, as integers, as audio, as a file, etc...
I want to be able to pipe these transforms to different sinks, to graph them,
or if it's compatible with audio, to play it."

(comment (defn tag-with-type []
           (fn [rf]
             (fn
               ([] (rf))
               ([result] (rf result))
               ([result chunk]
                ;; Your logic here to tag
                (let [tagged-chunk (tag-type chunk)]
                  (rf result tagged-chunk)))))))

(defn dynamic-xform []
  ;; ... same as before but with type dispatch logic
  )

(comment (def registered-sinks
           {:audio audio-sink-fn
            :graph graph-sink-fn
            :file  file-sink-fn
            ;;...
            }))

(comment (defn main-pipeline [sizes coll]
           (transduce (comp
                        (tag-with-type)
                        (dynamic-xform)
                        ;; ...
                        )
                      (fn [result chunk]
                        ;; Dynamic sink selection based on type
                        (let [sink (get registered-sinks (:type chunk))]
                          (sink result chunk)))
                      {}
                      (partitioned-xforms sizes coll))))



