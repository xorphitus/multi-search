(ns multi-search.core
  (:require [clojure.xml     :as xml]
            [clojure.java.io :as io]))

(def words ["apple" "google" "microsoft" "test" "baseball" "basketball"])

(defn google [word]
  (->> (xml/parse (str "https://news.google.com/news/feeds?q=" "apple" "&output=rss"))
       xml-seq
       (filter #(= (:tag %) :link))
       (map :content)
       (map first)
       (take 5)))

(defn grep [word]
  (->> (file-seq (io/file "/var/log"))
       (filter #(.isFile %))
       (filter
        #(with-open [rdr (io/reader %)]
           (some (fn [l] (re-find (re-pattern word) l)) (line-seq rdr))))
       (map #(str (.getPath %) (.getName %)))))

(defprotocol Source
  (search [this]))

(defrecord Web [word]
  Source
  (search [this] (google (:word this))))

(defrecord Local [word]
  Source
  (search [this] (grep (:word this))))

(def typed-words
  (concat
   (map #(Web. %) words)
   (map #(Local. %) words)))

(defn compaction [coll]
  (distinct (filter (comp not empty?) coll)))

(defn -main []
  (time (println (compaction (pmap search typed-words))))
  (shutdown-agents))
