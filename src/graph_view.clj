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
   )
  (:import
    [java.io IOException]
    [clojure.lang IExceptionInfo]))


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

(project-map {:deps "deps.edn"})


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


