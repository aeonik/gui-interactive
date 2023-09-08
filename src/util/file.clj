(ns util.file
  (:require [babashka.fs :as fs]))
(defn compute-file-hash [file-path]
  (clj-commons.digest/digest "sha-256" (fs/file file-path)))

(compute-file-hash (fs/path "resources/frog.png"))

(defn combine-hashes
  "Combine a list of hashes into a single hash."
  [hashes]
  (clj-commons.digest/digest "sha-256" (apply str hashes)))

(defn hex->bytes
  "Convert a hex string to a byte array."
  [s]
  (byte-array
    (map #(Integer/parseInt (apply str %) 16)
         (partition 2 (seq s)))))

(defn combine-hashes
  "Combine a list of hex-encoded hashes into a single hash."
  [hashes]
  (clj-commons.digest/digest "sha-256" (byte-array (mapcat hex->bytes hashes))))
(combine-hashes [(compute-file-hash (fs/path "resources/frog.png"))
                 (compute-file-hash (fs/path "resources/frog.png"))])