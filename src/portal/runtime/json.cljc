(ns ^:no-doc portal.runtime.json
  (:refer-clojure :exclude [read])
  #?(:bb   (:require [cheshire.core :as json]
                     [clojure.walk :as walk])
     :clj  (:require [clojure.data.json :as json]
                     [clojure.walk :as walk])
     :cljr (:require [portal.runtime.clr.assembly]
                     [clojure.data.json :as json]
                     [clojure.walk :as walk])
     :cljs (:require [clojure.walk :as walk])))

(def meta-key :portal.runtime.json/meta__)
(def data-key :portal.runtime.json/data__)
(defn preserve-ns-key-fn [key]
  (subs  (str key) 1))

(defn encode-metadata
  "Recursively encodes metadata in a Clojure object.
   Converts objects with metadata to {meta-key metadata data-key object_itself} form."
  [x]
  (walk/postwalk
   (fn [node]
     (cond
       ;; Verify a map doesn't already contain our special keys
       (and (map? node) (contains? node meta-key))
       (throw (ex-info (str "Object already contains " meta-key " key")
                       {:object node}))

       ;; If the node has metadata, encode it
       (meta node)
       {meta-key (encode-metadata (meta node))
        data-key node}

       :else
       node))
   x))

(defn decode-metadata
  "Recursively decodes metadata from a Clojure object.
   Converts {meta-key metadata data-key object_itself} back to objects with metadata."
  [x]
  (walk/postwalk
   (fn [node]
     (if (and (map? node)
              (contains? node meta-key)
              (contains? node data-key))
       (with-meta (get node data-key) (get node meta-key))
       node))
   x))

(defn write-raw [value]
  #?(:bb   (json/generate-string value {:key-fn preserve-ns-key-fn})
     :clj  (json/write-str value {:key-fn preserve-ns-key-fn})
     :cljr (json/write-str value {:key-fn preserve-ns-key-fn})
     :cljs (.stringify js/JSON (clj->js value {:keyword-fn preserve-ns-key-fn}))))

(defn read-raw
  ([string]
   (read-raw string {:key-fn keyword}))
  ([string opts]
   #?(:bb   (json/parse-string string (:key-fn opts))
      :clj  (json/read-str string :key-fn (:key-fn opts))
      :cljr (json/read-str string :key-fn (:key-fn opts))
      :cljs (js->clj (.parse js/JSON string)
                     :keywordize-keys
                     (= keyword (:key-fn opts))))))

(defn read-stream-raw [stream]
  #?(:bb   (json/parse-stream stream keyword)
     :clj  (json/read stream :key-fn keyword)
     :cljs (throw (ex-info "Unsupported in cljs" {:stream stream}))))

(defn write [value]
  (write-raw (encode-metadata value)))

(defn read
  ([string]
   (read string {:key-fn keyword}))
  ([string opts]
   (decode-metadata (read-raw string opts))))

(defn read-stream [stream]
  (decode-metadata (read-stream-raw stream)))
