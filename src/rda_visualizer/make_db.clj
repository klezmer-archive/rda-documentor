(ns rda-visualizer.make-db
  (:require [babashka.fs :as fs]
            [babashka.pods :as pods]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [honey.sql :as sql]
            [honey.sql.helpers :as h]
            [rda-visualizer.db :refer [db table]]))

(pods/load-pod 'org.babashka/go-sqlite3 "0.1.0")
(require '[pod.babashka.go-sqlite3 :as sqlite])

(defmacro with-read-files [[rdr-binding file-binding] & forms]
  `(->> (fs/glob "RDARegistry RDA-Vocabularies master csv-Elements" "*.csv")
        (map fs/file)
        (map (fn [~file-binding]
               (with-open [~rdr-binding (io/reader ~file-binding)]
                 ~@forms)))))

(defn csv-headers []
  (with-read-files [rdr f]
    (->> (csv/read-csv rdr)
         first
         (vector (fs/file-name f)))))

(comment
  (->> (csv-headers)
       (map second)
       set
       count)
  ;; => 15

  ;; Headers grouped by file
  (->> (csv-headers)
       (mapcat (fn [[fname headers]] (map vector (repeat fname) headers)))
       (reduce (fn [m [fname header]]
                 (update m header #(conj % fname)))
               {}))

  ;; All headers
  (->> (csv-headers)
       (mapcat second)
       set)
  )

(defn make-schema []
  (->> (csv-headers)
       (mapcat second)
       set  ;All csv columns
       (map #(vector % :text))
       (into [[:id :text]])
       (apply h/with-columns (h/create-table table))
       sql/format
       (sqlite/execute! db)))

(defn insert [cols rows]
  (-> (h/insert-into table cols)
      (h/values rows)
      sql/format))

(defn empty-str->nil [row]
  (map #(if (str/blank? %) nil %) row))

(defn load-db []
  (dorun
   (with-read-files [rdr f]
     (let [[[cols] rows] (split-at 1 (csv/read-csv rdr))]
       (doseq [batch (partition-all 300 (map #(into [(str (fs/file-name f) ":" %1)] %2) (range 1 9999999) rows))]
         (sqlite/execute! db (insert (into [:id] cols)
                                     (map empty-str->nil batch))))))))

(defn make-db []
  (fs/delete-if-exists db)
  (make-schema)
  (load-db))

(comment
  (make-db)
  (sqlite/execute! db "insert into triples (\"*uri\") values (\"hi\");")
  (sqlite/query db (-> (h/from table)
                       (h/select-distinct :*type)
                       sql/format))
  ;; => [{:*type "property"} {:*type "class"}]

  (sqlite/query db (-> (h/select :%count.*)
                       (h/from table)
                       #_(h/where [:= :*type "class"])
                       sql/format))
  ;; total
  ;; => [{:COUNT(*) 10656}]
  ;; classes
  ;; => [{:COUNT(*) 22}]
  ;; properties
  ;; => [{:COUNT(*) 10634}]
  ;; Something's not right, there's 10657 lines in all the files, including headers. I'd expect
  ;; 10627 rows
  ;; Aaaah, wc -l counts NEWLINES, not lines. Using grep -c '', I get a total of 10686. -30 header
  ;; lines (1 per file) I get the expected result: 10656 

  )