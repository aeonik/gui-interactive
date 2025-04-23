(ns core
  (:require [membrane.skia :as skia]
            [membrane.ui :as ui
             :refer [vertical-layout
                     translate
                     horizontal-layout
                     button
                     label
                     with-color
                     bounds
                     origin
                     spacer
                     on]]
            [membrane.component :as component :refer [defui defeffect]]
            [membrane.basic-components :as basic]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [liq.buffer :as buffer]
            [components.code-editor :as code-editor]))

(add-tap (bound-fn* clojure.pprint/pprint))

;; --- Core state and dynamic component registry ---

(defonce app-state
  (atom {:base-path "."
         :window-width 1200
         :window-height 800
         :selected-file nil
         :file-content ""
         :str-filter ""
         :buffer (buffer/buffer "" {:rows 40 :cols 5 :mode :insert})
         :hello-text "Hello from composed component!"
         :current-view :main}))

(defonce view-registry (atom {}))

(defn register-view! [k view-var]
  (swap! view-registry assoc k view-var))

(defn get-view [k] (get @view-registry k))

;; --- Effects ---

(defeffect ::select-file [$selected-file $file-content $buffer path]
  (tap> {:event :select-file :path path :state @app-state})
  (when (and path (fs/exists? path))
    (let [content (slurp (str path))
          new-buffer (buffer/buffer content)]
      (tap> {:event :creating-buffer :content-length (count content) :buffer new-buffer})
      (dispatch! :update $selected-file (constantly path))
      (dispatch! :update $file-content (constantly content))
      (dispatch! :update $buffer (constantly new-buffer)))))

(defeffect ::update-filter [$str-filter text]
  (tap> {:event :update-filter :text text})
  (dispatch! :update $str-filter (constantly text)))

(defeffect ::update-hello [$hello-text text]
  (tap> {:event :update-hello :text text})
  (dispatch! :update $hello-text (constantly text)))

;; --- Components ---

(defui file-item [{:keys [path is-file?]}]
  (on :mouse-down
      (fn [_]
        (tap> {:event :file-clicked :path path :is-file? is-file?})
        (when is-file?
          [[::select-file :selected-file :file-content :buffer path]]))
      (horizontal-layout (label (str (if is-file? "📄 " "📁 ") path)))))

(defui file-browser [{:keys [base-path selected-file file-content str-filter buffer] :as props}]
  (tap> {:event :file-browser-render 
         :props props
         :buffer buffer
         :has-buffer? (some? buffer)})
  (let [files (when (and base-path (fs/exists? base-path))
                (->> (fs/glob base-path "**" {:hidden false})
                     (map (fn [f] {:path f :is-file? (fs/regular-file? f)}))
                     (filter #(str/includes? (str/lower-case (str (:path %)))
                                             (str/lower-case (or str-filter ""))))
                     (sort-by :path)))]
    (horizontal-layout
     (vertical-layout
      (label "Filter:")
      (basic/textarea {:text str-filter
                       :on-change (fn [txt] [[::update-filter txt]])})
      (spacer 0 10)
      (basic/scrollview {:scroll-bounds [400 600]
                         :body (apply vertical-layout (map file-item files))}))
     (spacer 20 0)
     (vertical-layout
      (label (if selected-file
               (str "Content of: " selected-file)
               "Select a file to view its content"))
      (spacer 0 10)
      (try
        (if buffer
          (code-editor/text-editor {:buf buffer})
          (label "Buffer is nil."))
        (catch Exception e
          (tap> {:event :text-editor-error :error (.getMessage e)})
          (label (str "Editor error: " (.getMessage e)))))))))

(defui hello-box [{:keys [hello-text]}]
  (vertical-layout
   (label "Greeting:")
   (basic/textarea {:text hello-text :on-change (fn [txt] [[::update-hello txt]])})
   (spacer 0 10)))

(defui main-view [{:keys [hello-text base-path selected-file file-content str-filter buffer] :as state}]
  (tap> {:event :main-view-render 
         :state state
         :buffer buffer
         :has-buffer? (some? buffer)})
  (vertical-layout
   (hello-box {:hello-text hello-text})
   (file-browser {:base-path base-path
                  :selected-file selected-file
                  :file-content file-content
                  :str-filter str-filter
                  :buffer buffer})))

(defui root-view [{:keys [hello-text
                          base-path
                          selected-file
                          file-content
                          str-filter
                          buffer] :as state}]
  (vertical-layout
   (hello-box {:hello-text hello-text})
   (file-browser {:base-path base-path
                  :selected-file selected-file
                  :file-content file-content
                  :str-filter str-filter
                  :buffer buffer})))


;; --- Entrypoint ---

(defn -main [& [path]]
  (tap> {:event :main-start :path path})
  (let [initial-buffer (buffer/buffer "" {:rows 40 :cols 5 :mode :insert})]
    (tap> {:event :creating-initial-buffer :buffer initial-buffer})
    (let [new-state (merge @app-state
                          {:base-path (or path ".")
                           :buffer initial-buffer})]
      (tap> {:event :setting-initial-state :state new-state})
      (reset! app-state new-state)
      (tap> {:event :app-state-initialized :state @app-state})
      (register-view! :main #'main-view)
      (skia/run (component/make-app #'root-view app-state)))))

(comment
  (-main))
