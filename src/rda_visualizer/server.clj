(ns rda-visualizer.server
  (:require [babashka.pods :as pods]
            [clojure.set :as set]
            [clojure.string :as str]
            [hiccup2.core :refer [html]]
            [honey.sql :as sql]
            [honey.sql.helpers :as h]
            [org.httpkit.server :as httpkit]
            [rda-visualizer.db :refer [query]])) 

(pods/load-pod 'org.babashka/go-sqlite3 "0.1.0")
(require '[pod.babashka.go-sqlite3 :as sqlite])

;; -- Columns --
;; On all rows
;; - :*uri
;; - :*type
;; - :id
;; - :*label_en
;; 
;; On no rows
;; - :sameAs[0]
;; 
;; On some rows
;; - :description[0]_en
;; - :ToolkitLabel_en
;; - :instructionNumber
;; - :lexicalAlias_en
;; - :note[0]_en
;; - :altLabel[0]_en
;; - :ToolkitDefinition_en
;; - :*status
;; 
;; On some classes
;; - :subClassOf[1]
;; - :subClassOf[0]
;; 
;; On some properties
;; - :subPropertyOf[0]
;; - :subPropertyOf[1]
;; - :owl:propertyChainAxiom
;; - :altLabel[5]_en
;; - :See Also[0]
;; - :has element type
;; - :altLabel[8]_en
;; - :altLabel[4]_en
;; - :altLabel[7]_en
;; - :altLabel[6]_en
;; - :inverseOf
;; - :altLabel[1]_en
;; - :altLabel[3]_en
;; - :domain
;; - :range
;; - :altLabel[2]_en
;; - :subPropertyOf[2]

(comment
  (def columns
    (-> (sqlite/query db (str "select * from " (name table) " limit 1"))
        first
        keys))

  (defn iterate-db [query batch-size]
    (->> (iteration (fn [offset]
                      [(+ batch-size offset)
                       (sqlite/query db (-> query
                                            (h/limit batch-size)
                                            (h/offset offset)
                                            sql/format))])
                    :somef (comp seq second)
                    :initk 0
                    :kf first
                    :vf second)
         (sequence cat)))

  (def base-query (-> (h/select :*) (h/from table) (h/order-by :rowid)))

  @(def both-all-columns
     (reduce #(set/difference %1 (->> %2
                                      (filter (comp nil? val))
                                      (map key)
                                      set))
             (set columns)
             (iterate-db base-query 300)))
  ;; => #{:*uri :*type :id :*label_en}

  @(def both-no-columns
     (reduce #(set/difference %1 (->> %2
                                      (filter (comp some? val))
                                      (map key)
                                      set))
             (set columns)
             (iterate-db base-query 300)))
  ;; => #{:sameAs[0]}

  (def non-empty-columns (set/difference (set columns) both-no-columns))

  @(def class-only-some-columns
     (reduce #(set/difference %1 (->> %2
                                      (filter (comp some? val))
                                      (map key)
                                      set))
             non-empty-columns
             (iterate-db (h/where base-query :not= :*type "class") 300)))
  ;; => #{:subClassOf[1] :subClassOf[0]}

  @(def class-only-all-columns
     (reduce #(set/difference %1 (->> %2
                                      (filter (comp some? val))
                                      (map key)
                                      set))
             class-only-some-columns
             (iterate-db (h/where base-query := :*type "class") 300)))
  ;; => #{}

  @(def property-only-some-columns
     (reduce #(set/difference %1 (->> %2
                                      (filter (comp some? val))
                                      (map key)
                                      set))
             non-empty-columns
             (iterate-db (h/where base-query :not= :*type "property") 300)))
  ;; => #{:subPropertyOf[0]
  ;;      :subPropertyOf[1]
  ;;      :owl:propertyChainAxiom
  ;;      :altLabel[5]_en
  ;;      :See Also[0]
  ;;      :has element type
  ;;      :altLabel[8]_en
  ;;      :altLabel[4]_en
  ;;      :altLabel[7]_en
  ;;      :altLabel[6]_en
  ;;      :inverseOf
  ;;      :altLabel[1]_en
  ;;      :altLabel[3]_en
  ;;      :domain
  ;;      :range
  ;;      :altLabel[2]_en
  ;;      :subPropertyOf[2]}
  
  @(def property-only-all-columns
     (reduce #(set/difference %1 (->> %2
                                      (filter (comp some? val))
                                      (map key)
                                      set))
             property-only-some-columns
             (iterate-db (h/where base-query := :*type "property") 300)))
  ;; => #{}

  (set/difference (set columns) (set/union both-all-columns property-only-some-columns class-only-some-columns))
  ;; => #{:description[0]_en
  ;;      :ToolkitLabel_en
  ;;      :instructionNumber
  ;;      :lexicalAlias_en
  ;;      :note[0]_en
  ;;      :altLabel[0]_en
  ;;      :ToolkitDefinition_en
  ;;      :*status}

  (->> (set/difference (set columns) (set/union both-all-columns property-only-some-columns class-only-some-columns))
       (map #(vector % (sqlite/query db (-> (h/select-distinct :*type) (h/from table) (h/where [:is-not % nil]) sql/format))))
       (every? #(= (set (second %)) (set [{:*type "property"} {:*type "class"}]))))
  ;; => true 

  @(def both-some-columns
     (set/difference (set columns)
                     (set/union both-all-columns
                                property-only-some-columns
                                class-only-some-columns
                                both-no-columns)))
  ;; => #{:description[0]_en
  ;;      :ToolkitLabel_en
  ;;      :instructionNumber
  ;;      :lexicalAlias_en
  ;;      :note[0]_en
  ;;      :altLabel[0]_en
  ;;      :ToolkitDefinition_en
  ;;      :*status}


  (sqlite/query db (str "select * from " (name table) " where \"*type\" = 'class' limit 1"))
  (sqlite/query db (str "select \"*uri\", count(\"*uri\") from " (name table) " group by \"*uri\" having count(\"*uri\") > 1"))
  (apply clojure.data/diff (sqlite/query db (str "select * from " (name table) " where \"*uri\" = 'rdau:P60952'"))) 

  (sqlite/query db (str "select id from " (name table) " where \"*uri\" = 'rdac:C10001' limit 1"))
  )

(defn traverse [row col-prefix]
  (let [cols (->> row keys (map name)
                  (filter #(str/starts-with? % col-prefix))
                  (map keyword))
        go (fn [explore seen])] ;TODO finish. Maybe a CTE? Nah, DFS
    go (set ((apply juxt cols) row))))

(defn same-uri-as [{id :id, uri :*uri}]
  (when-let [others (-> (h/select :id)
                        (h/where [:= :*uri uri]
                                 [:not= :id id])
                        query
                        seq)]
    (list [:h4 "Same URI as"]
          (for [o others]
            [:a {:href (str "/" (:id o))} (:id o)]))))

(defn general-attributes [row]
  (list [:dt "Description"]
        [:dd ((keyword "description[0]_en") row)]
        [:dt "Note"]
        [:dd ((keyword "note[0]_en") row)]))

(def styles
  "
   .subtitle {
     font-style: italic;
   }
   dl {
     display: grid;
     grid-template-columns: auto auto;
   }
   dt {
     grid-column: 1;
     font-weight: bold;
   }
   dd {
     grid-column: 2;
   }
   ")
  ;;  long descriptions are leaking underneath the definition

(defn by-id [{[id] :params}]
  (if-let [row (-> (h/select :*)
                   (h/where := :id id)
                   (h/limit 1)
                   query
                   first)]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body
     (str
      (html
       [:html {:lang "en"}
        [:head
         [:title "RDA Visualizer"]
         [:link {:rel "stylesheet" :href "https://unpkg.com/sakura.css/css/sakura.css" :type "text/css"}]
         [:style styles]]
        [:body
         [:main
          [:h1 (:*type row) ": " (or (:ToolkitLabel_en row) (:*label_en row))]
          [:div.subtitle (:*uri row)]
          (same-uri-as row)
          [:dl
           (general-attributes row)]]]]))}
    {:status 404}))

(def routes {[:get #"/([a-zA-Z0-9:.]+)"] #'by-id})

(defn route
  "Given routes and a request, returns a response generated by a matching
   route, or a 404 if no match is found"
  [routes req]
  (if-let [[handler req']
           (->> routes
                (keep (fn [[[method pattern] handler]]
                        [method pattern handler]
                        (when (and (= method (:request-method req))
                                   (some? (re-matches pattern (:uri req))))
                                         ;; `rest` b/c the first group is always the whole match
                          [handler (assoc req :params (rest (re-matches pattern (:uri req))))])))
                first)]
    (handler req')
    {:status 404}))

(defn app [req]
  (route @#'routes req))

(defonce server (atom nil))

(defn start-server! []
  (when (nil? @server)
    (reset! server (httpkit/run-server #'app {:port 8000}))))

(defn stop-server! []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(comment
  (defonce log (atom []))
  (add-tap #(swap! log conj %))
  (reset! log [])
  (start-server!)
  (stop-server!)
  )