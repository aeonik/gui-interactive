(ns graph_view
  (:require
   [babashka.fs :as fs]
   [cljfx.api :as fx]
   [clojure.java.io :as io]
   [clojure.tools.reader :as reader]
   [clojure.tools.reader.edn :as edn]
   [clojure.tools.analyzer.jvm :as analyzer]
   [clojure.tools.deps.alpha :as deps]
   [clojure.tools.deps.graph :as graph]
   [clojure.tools.namespace.parse :as ns-parse]
   [clojure.tools.namespace.find :as ns-find]
   [babashka.fs :as fs]
   [flow-storm.api :as fs-api]
   [clojure.tools.deps.alpha :as deps]
   [clojure.tools.deps.alpha.extensions :as ext]
   [clojure.tools.deps.alpha.util.session :as session]
   [clojure.tools.deps.alpha.script.parse :as parse]
   [clojure.tools.cli :as cli]
   [clojure.string :as str]
   [clojure.pprint :as pprint]
   [clojure.data :as data]
   )
  (:import
    [java.io IOException]
    [clojure.lang IExceptionInfo]))


;; https://gist.github.com/cgrand/b5bf4851b0e5e3aeb438eba2298dacb9
;; Might also want to use https://github.com/cgrand/xforms
(defn tree-cat [branch? children]
  (fn [rf]
    (letfn [(nested [acc x]
              (let [acc (rf acc x)]
                (cond
                  (reduced? acc) (reduced acc)
                  (branch? x) (let [acc (reduce nested acc (children x))]
                                (cond-> acc (reduced? acc) reduced))
                  :else acc)))]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc x] (unreduced (nested acc x)))))))

(defn foliage-cat [branch? children]
  (fn [rf]
    (letfn [(nested [acc x]
              (let [acc (if (branch? x)
                          (reduce nested acc (children x))
                          (rf acc x))]
                (cond-> acc (reduced? acc) reduced)))]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc x] (unreduced (nested acc x)))))))

(defn nodes-cat [branch? children]
  (fn [rf]
    (let [rf (x/ensure-kvrf rf)
          nested (fn nested [acc x]
                   (let [is-branch (branch? x)
                         acc (rf acc is-branch x)]
                     (cond
                       (reduced? acc) (reduced acc)
                       is-branch (let [acc (reduce nested acc (children x))]
                                   (cond-> acc (reduced? acc) reduced))
                       :else acc)))]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc x] (unreduced (nested acc x)))))))


(defn project-map
  [{:keys [deps trace trace-file output aliases trace-omit size] :as opts}]
  (try
    (if trace-file
      (do
        (when-not output (throw (ex-info "Must specify output file name in trace mode" {})))
        (let [tf (io/file trace-file)]
          (if (.exists tf)
            ;; The output-trace function has been removed
            (throw (ex-info (str "Trace file does not exist: " trace-file) {})))))
      (let [{:keys [root-edn user-edn project-edn]} (deps/find-edn-maps (or deps "deps.edn"))
            master-edn (deps/merge-edns [root-edn user-edn project-edn])
            combined-aliases (deps/combine-aliases master-edn aliases)
            basis (session/with-session
                    (deps/calc-basis master-edn {:resolve-args (merge combined-aliases {:trace true})
                                                 :classpath-args combined-aliases}))
            lib-map (:libs basis)]
        ;; Return the relevant variables as a map
        {:root-edn root-edn
         :user-edn user-edn
         :project-edn project-edn
         :master-edn master-edn
         :combined-aliases combined-aliases
         :basis basis
         :lib-map lib-map
         :output output
         :size size}))
    (catch IOException e
      (if (str/starts-with? (.getMessage e) "Cannot run program")
        (throw (ex-info "tools.deps.graph requires Graphviz (https://graphviz.gitlab.io/download) to be installed to generate graphs." { } e))))))

(defn tree-diff [t1 t2]
  (cond
    (and (coll? t1) (coll? t2)) 
      (let [keys1 (set (keys t1))
            keys2 (set (keys t2))
            common (clojure.set/intersection keys1 keys2)
            only-in-1 (clojure.set/difference keys1 keys2)
            only-in-2 (clojure.set/difference keys2 keys1)]
        (concat 
          (for [k only-in-1] [:removed k (get t1 k)])
          (for [k only-in-2] [:added k (get t2 k)])
          (for [k common] 
            (let [diff (tree-diff (get t1 k) (get t2 k))]
              (when (seq diff) [:changed k diff])))))
    
    (= t1 t2) nil
    
    :else [[:changed t1 t2]]))



(def project-data (project-map {:deps "deps.edn"}))
(keys project-data)
(:root-edn project-data)
(:user-edn project-data)
(:project-edn project-data)
(:master-edn project-data)
(:combined-aliases project-data)
(:basis project-data)
(:lib-map project-data)
(:output project-data)
(:size project-data)

(def root-edn (:root-edn project-data))
(take 1 (mapcat root-edn))

(transduce (comp (tree-cat map? vals)
                 (filter map?))
           (:root-edn project-data))
      

(map (comp count (juxt (keys project-data)) project-data))
(count (tree-seq map? vals (:root-edn project-data)))
(count (:user-edn project-data))
(count (:project-edn project-data))
(count (:master-edn project-data))

(count (filter map? (tree-seq map? vals (:root-edn project-data))))
(count (filter map? (tree-seq map? vals (:user-edn project-data))))
(count (filter map? (tree-seq map? vals (:project-edn project-data))))
(count (filter map? (tree-seq map? vals (:master-edn project-data))))
(count (filter map? (tree-seq map? vals (:combined-aliases project-data))))
(count (filter map? (tree-seq map? vals (:basis project-data))))
(count (filter map? (tree-seq map? vals (:lib-map project-data))))

(pprint/pprint (tree-seq map? vals (:root-edn project-data)))

(transduce
 (filter vector?)
 (fn
   ([acc] acc)
   ([[l-acc r-acc] [l r]]
    [(+ l-acc l) (+ r-acc r)]))
 [0 0]
 (tree-seq (complement vector?) identity (root-edn project-data)))

(defn count-map-elements [m]
  (transduce
   (comp (filter map?) (map count))
   +
   (tree-seq map? vals m)))

(defn return-map-elements [m]
  (transduce
   (comp (filter map?) (map count))
   +
   (tree-seq map? vals m)))

(count-map-elements (:root-edn project-data))
(return-map-elements (:root-edn project-data))


(defn global-totals
  "the first transducer pulls out the `:changes` entry for each map and
  concatenates them all together. The second one, `cat`, concatenates all the
  subsequences together. This will feed only the vectors into the reducing
  function."
  [data]
  (let [xform (comp (mapcat :changes) cat)
        f     (completing
               (fn [acc item]
                 (mapv + acc item)))]
    (transduce xform f [0 0] data)))

sicmutils.env> (time (global-totals (take 100000 (cycle inputs))))
"Elapsed time: 2386.474 msecs"
[6440000 14540000]

(map (fn [key] 
       {key (count 
             (filter map? 
                     (tree-seq map? vals (get project-data key))))}) 
     (keys project-data))

(pprint/pprint (into {} 
                     (map (fn [key] 
                            {key (count 
                                  (filter map? 
                                          (tree-seq map? vals (get project-data key))))}) 
                          (keys project-data))))

;; Does not work
(tree-diff (:root-edn project-data) (:user-edn project-data)) 

(frequencies root-edn)

(def root-edn-user-edn-project-edn (deps/find-edn-maps (or deps "deps.edn")))
(def master-edn (deps/merge-edns [(:root-edn root-edn-user-edn-project-edn)
                                  (:user-edn root-edn-user-edn-project-edn)
                                  (:project-edn root-edn-user-edn-project-edn)]))
(def combined-aliases (deps/combine-aliases master-edn aliases))  ;; aliases should be defined somewhere
(def basis (session/with-session
             (deps/calc-basis master-edn {:resolve-args (merge combined-aliases {:trace true})
                                          :classpath-args combined-aliases})))
(def lib-map (:libs basis))

(defn- ->tree-item [x]
  (cond
    (string? x) {:fx/type :tree-item :value x}
    (seqable? x) {:fx/type :tree-item :value x :children (map ->tree-item x)}
    :else {:fx/type :tree-item :value x}))

(defn- graph
  [ns-name key]
  (let [ns (ns-parse/parse-ns ns-name)
        deps (->> ns
                  :requires
                  (map :name)
                  (mapcat (fn [x] (deps/resolve-deps '{:deps {x {}}}))))]
    (into {} (map (fn [x] [x deps]) (get ns key)))))

(defn var-graph
  "Returns a map of vars to their dependencies."
  [ns-name]
  (graph ns-name :vars))

(defn ns-graph
  "Returns a map of namespaces to their dependencies."
  [ns-name]
  (graph ns-name :ns))

;; Load the deps.edn file of the current project
(def deps-edn (slurp "deps.edn"))
(def project (edn/read-string deps-edn))

(def paths (:paths project))
(def deps (:deps project))


(defn build-ns-map []
  (into {} (mapv #(vector (ns-name %)
                          {:mappings (ns-map %)
                           :aliases  (reduce-kv (fn [a k v] (assoc a k (ns-name v)))
                                                {} (ns-aliases %))
                           :ns       (ns-name %)})
                 (all-ns))))

(defn global-env []
  (atom {:namespaces (build-ns-map)}))

(global-env)

;; Find all namespaces in src
(def clojure-files (ns-find/find-sources-in-dir (io/file "src")))

(ns-find/find-ns-decls-in-dir (io/file "src"))

;; This has side effects apparently
(map analyzer/analyze-ns (ns-find/find-namespaces-in-dir (io/file "src")))

(def gui (nth (ns-find/find-namespaces-in-dir (io/file "src")) 9))
(analyzer/analyze+eval gui)

(def analyzed-files (mapv analyze-file clojure-files))

;; For each clojure file, read the file
(def clojure-file-contents
  (->> (map slurp clojure-files)
       (map #(with-open [rdr (io/reader %)]
               (doall (read-all rdr))))
       (apply concat)))

;; For each clojure file, read the file
(def clojure-file-contents
  (->> (map slurp clojure-files)
       (line-seq)
       (map edn/read-string )))
(->> clojure-files
     (first)
     slurp
     edn/read-string)
(->> clojure-files
     (map #(slurp %))
     (-> line-seq
         (map #(edn/read-string %))))
(graph/graph {})

(let [{:keys [root-edn user-edn project-edn]} (deps/find-edn-maps (or deps "deps.edn"))
      master-edn (deps/merge-edns [root-edn user-edn project-edn])
      combined-aliases (deps/combine-aliases master-edn aliases)
      basis (session/with-session
              (deps/calc-basis master-edn {:resolve-args (merge combined-aliases {:trace true})
                                           :classpath-args combined-aliases}))
      lib-map (:libs basis)]


