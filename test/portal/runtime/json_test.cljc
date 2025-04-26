(ns portal.runtime.json-test
  (:require
   [clojure.test :refer [deftest is]]
   [portal.runtime.json :as json]
   [portal.viewer :as v]))

(defn- sin [x]
  #?(:cljr (Math/Sin x) :default (Math/sin x)))

(def line-chart
  (v/vega-lite
   {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
    :data {:values
           (map #(-> {:time % :value (sin %)})
                (range 0 (* 2 3.14) 0.25))}
    :encoding {:x {:field "time" :type "quantitative"}
               :y {:field "value" :type "quantitative"}}
    :mark "line"}))

(deftest test-preserve-metadata
  (is
   (=
    (meta line-chart)
    (-> line-chart
        json/encode-metadata
        json/decode-metadata
        meta))
   "Metadata on vega-lite is lost when converting to JSON and back"))
