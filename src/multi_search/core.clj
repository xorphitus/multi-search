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
       flatten
       (map #(last (clojure.string/split % #"url=")))))

(defn grep [word]
  (->> (file-seq (io/file (str (System/getenv "HOME") "/junk")))
       (filter #(.isFile %))
       (filter
        #(with-open [rdr (io/reader %)]
           (some (fn [l]
                   (re-find (re-pattern word) l))
                 (line-seq rdr))))
       (map #(str (.getPath %) (.getName %)))))

(defprotocol Source
  (search [this]))

(defmacro defsource [type finder]
  `(defrecord ~type [~'word]
    Source
    (search [this#] (~finder (:word this#)))))

(defsource Web   google)
(defsource Local grep)

(defmacro gen-searchables [sources targets]
  (let [typed-words
        (map
         (fn [src]
           (let [rec (symbol (str `~src "."))]
             `(map #(~rec %) ~targets)))
         sources)]
    `(concat ~@typed-words)))

(defn compaction [coll]
  (distinct (filter (comp not empty?) coll)))

(defn mprint [coll] (println (interleave coll (repeat "\n"))))

(defn -main []
  (time (mprint (compaction (flatten (pmap search (gen-searchables [Web Local] words))))))
  (shutdown-agents))
