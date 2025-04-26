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
(def keyword-key "__portal__runtime__json__keyword__")

(defn preserve-ns-key-fn [key]
  (subs  (str key) 1))

(defn starts-with? [s prefix]
  (let [prefix-len (count prefix)]
    (and (>= (count s) prefix-len)
         (= (subs s 0 prefix-len) prefix))))

(defn remove-keyword-key [x]
  (subs (str x) (count keyword-key)))

(defn encode-keywords [x]
  (walk/postwalk
   (fn [node]
     (if (keyword? node)
       (str keyword-key (preserve-ns-key-fn node))
       node))
   x))

(defn decode-keywords [x]
  (walk/postwalk
   (fn [node]
     (if (and
          (string? node)
          (starts-with? node keyword-key))

       (keyword (remove-keyword-key node))
       node))
   x))

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
  #?(:bb   (json/generate-string value)
     :clj  (json/write-str value)
     :cljr (json/write-str value)
     :cljs (.stringify js/JSON (clj->js value))))

(defn read-raw
  ([string]
   (read-raw string {}))
  ([string opts]
   #?(:bb   (json/parse-string string)
      :clj  (json/read-str string)
      :cljr (json/read-str string)
      :cljs (js->clj (.parse js/JSON string)))))

(defn read-stream-raw [stream]
  #?(:bb   (json/parse-stream stream)
     :clj  (json/read stream)
     :cljs (throw (ex-info "Unsupported in cljs" {:stream stream}))))

(defn write [value]
  (-> value
      encode-metadata
      encode-keywords
      write-raw))

(defn read
  ([string]
   (read string {}))
  ([string opts]
   (-> string
       (read-raw opts)
       decode-keywords
       decode-metadata)))

(defn read-stream [stream]
  (-> stream
      read-stream-raw
      decode-keywords
      decode-metadata))
