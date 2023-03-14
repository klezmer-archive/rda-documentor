(ns rda-visualizer.db 
  (:require [babashka.pods :as pods]
            [honey.sql :as sql]
            [honey.sql.helpers :as h]))

(pods/load-pod 'org.babashka/go-sqlite3 "0.1.0")
(require '[pod.babashka.go-sqlite3 :as sqlite])

(def db "triples.sqlite3")
(def table :elements)

(defn query [q]
  (-> q
      (h/from table)
      (as-> $ (if (:select $) $ (h/select $ :*)))
      sql/format
      (as-> $ (sqlite/query db $))))