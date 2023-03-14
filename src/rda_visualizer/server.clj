(ns rda-visualizer.server
  (:require [babashka.pods :as pods]
            [clojure.set :as set]
            [clojure.string :as str]
            [hiccup2.core :refer [html raw]]
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
;; - :domain
;; - :range
;; - :subPropertyOf[0]
;; - :subPropertyOf[1]
;; - :subPropertyOf[2]
;; - :altLabel[1]_en
;; - :altLabel[2]_en
;; - :altLabel[3]_en
;; - :altLabel[4]_en
;; - :altLabel[5]_en
;; - :altLabel[6]_en
;; - :altLabel[7]_en
;; - :altLabel[8]_en
;; - :owl:propertyChainAxiom
;; - :See Also[0]
;; - :has element type
;; - :inverseOf

(comment
  (sqlite/query db (str "select * from " (name table) " where \"subClassOf[0]\" is not null limit 1"))
  (sqlite/query db (str "select \"*uri\", count(\"*uri\") from " (name table) " group by \"*uri\" having count(\"*uri\") > 1"))
  (apply clojure.data/diff (sqlite/query db (str "select * from " (name table) " where \"*uri\" = 'rdau:P60952'")))

  (sqlite/query db (str "select id from " (name table) " where \"*uri\" = 'rdac:C10001' limit 1")))

(def styles
  "
   a {
     color: #0001ee;
   }
   a:visited {
     color: #551a8b;
   }
   .subtitle {
     font-style: italic;
   }
   dl {
     display: grid;
     grid-template-columns: max-content auto;
   }
   dt {
     grid-column: 1;
     font-weight: bold;
     padding-bottom: 0.75em;
     text-align: right;
   }
   dt:after {
     content: \":\";
   }
   dd {
     grid-column: 2;
     margin-left: 1em;
     padding-bottom: 0.75em;
   }
   .label:not(:last-child):after {
     content: \", \";
   }
   .primary-label {
     font-weight: 500;
   }
   ul.inheritance {
     padding-left: 0;
     margin-bottom: 0;
     list-style: disc;
   }
   ul.inheritance li {
     margin-bottom: 0;
     margin-left: 1em;
   }
   dd > ul.inheritance {
     list-style-type: none;
   }
   dd > ul.inheritance > li {
     padding-left: 0;
     margin-left: 0;
   }
   .tooltip .tooltip-text {
     visibility: hidden;
     position: absolute;
     transform: translate(10px, -10px);
     max-width: 40%;
     border: solid;
     border-radius: 15px;
     padding: 0.25em;
     color: initial;
     background-color: white;
     box-shadow: 2px 2px 3px grey;
     font-size: 0.9em;
   }
   .tooltip:hover .tooltip-text {
     visibility: visible;
   }
   /* The arrow */
   .tooltip .tooltip-text::before {
     content: \" \";
     border: 10px solid #000;
     border-color: transparent black transparent transparent;
     position: absolute;
     top: 15px;
     left: -20px;
   }
   ")

;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-or-none [row k]
  (-> k keyword row (as-> $ (if-not $ "-" $))))

(defn label [row]
  (or (:ToolkitLabel_en row) (:*label_en row)))

(defn entity-link [row]
  [:a.tooltip {:href (str "/" (:id row))}
   (label row)
   [:span.tooltip-text (get-or-none row "description[0]_en")]])

(defn get-by-uri [uri]
  (-> (h/select :*)
      (h/where := :*uri uri)
      (h/limit 1)
      query first))

;; Sub/super class/properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: the code for superclasses and subclasses is nearly, but not exactly, identical. Find some
;; way to factor it out? Maybe after switching to a triplestore?
(defn traverse-down [row col-prefix]
  (let [cols (->> row keys (map name)
                  (filter #(str/starts-with? % col-prefix))
                  (map keyword))
        col-vals (comp set (apply juxt cols))
        dfs (fn dfs [explore seen]
              (into
               {}
               (for [uri explore
                     :when (not (seen uri))
                     ;; Some rows share uris, we just pick the first arbitrarily since we show the
                     ;; correspondence in the ui so users can always navigate from whichever one we
                     ;; picked to the other
                     :let [row (get-by-uri uri)]]
                 ;; This is not tail-recursive, but the max depths are likely to be small and it's
                 ;; way easier to build a tree this way
                 [row (dfs (filter some? (col-vals row))
                           (conj seen uri))])))]
    (dfs (filter some? (col-vals row)) #{(:*uri row)})))
(comment
  (traverse-down (-> (h/select :*) (h/where [:is-not (keyword "subClassOf[0]") nil]) (h/limit 1) query first)
                 "subClassOf")
  ;; => {"rdac:C10013" {}}
  ;; ish, keys are whole rows
  )

(defn traverse-up [row col-prefix]
  (let [cols (->> row keys (map name)
                  (filter #(str/starts-with? % col-prefix))
                  (map keyword))
        ;; select-distinct because uris aren't unique and we don't want to show dups in the tree. If
        ;; there are dups, this picks one arbitrarily. The UI shows a link to the other, so users
        ;; can navigate to it if needed.
        get-parents #(-> (h/select :* [[:min :id]])
                         (h/where (into [:or] (for [col cols] [:= col (:*uri %)])))
                         (h/group-by :*uri)
                         query)
        dfs (fn dfs [explore seen]
              (into {}
                    (for [parent explore
                          :when (not (seen (:*uri parent)))]
                      [parent (dfs (get-parents parent)
                                   (conj seen (:*uri parent)))])))]
    (dfs (get-parents row) #{(:*uri row)})))
(comment
  (traverse-up (-> (h/select :*) (h/where [:= :id "rof.csv:6"]) query first)
               "subClassOf")
  ;; {"rof.csv:3" {"rof.csv:8" {}, "rof.csv:9" {}},
  ;;  "rof.csv:4" {"rof.csv:2" {}, "rof.csv:9" {}},
  ;;  "rof.csv:5" {"rof.csv:1" {}, "rof.csv:8" {}},
  ;;  "rof.csv:7" {"rof.csv:1" {}, "rof.csv:2" {}}}
  ;; Kinda, except keys are whole rows
  )

(defn nested-list [parents]
  (when (some seq parents)
    [:ul.inheritance
     (for [[row parents] parents]
       [:li (entity-link row)
        (when (seq parents)
          (nested-list parents))])]))

(defn subclass-of [row]
  (let [parents (traverse-down row "subClassOf")]
    (list [:dt "Subclass of"]
          [:dd (or (nested-list parents) "-")])))

(defn subclassed-by
  [row]
  (let [parents (traverse-up row "subClassOf")]
    (list [:dt "Subclassed by"]
          [:dd (or (nested-list parents) "-")])))

(defn subproperty-of [row]
  (let [parents (traverse-down row "subPropertyOf")]
    (list [:dt "Subproperty of"]
          [:dd (or (nested-list parents) "-")])))

(defn superproperty-of [row]
  (let [parents (traverse-up row "subPropertyOf")]
    (list [:dt "Superproperty of"]
          [:dd (or (nested-list parents) "-")])))

;; Attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (list [:dt "Description"] [:dd (get-or-none row "description[0]_en")]
        [:dt "Note"] [:dd (get-or-none row "note[0]_en")]
        [:dt "Labels"] [:dd [:span.label.primary-label (get-or-none row "*label_en")]
                        (->> (range 9) (map #(str "altLabel[" % "]_en"))
                             (map (comp row keyword)) (filter some?)
                             (map #(vector :span.label.alt-label %)))]
        [:dt "Instruction number"] [:dd (get-or-none row "instructionNumber")]
        [:dt "Lexical alias"] [:dd (get-or-none row "lexicalAlias_en")]
        [:dt "Status"] [:dd (get-or-none row "*status")]))

(defn property-attributes [row]
  (list [:dt "Domain"] [:dd (or (some-> row :domain get-by-uri entity-link) "-")]
        [:dt "Range"] [:dd (or (some-> row :range get-by-uri entity-link) "-")]))
(comment
  (-> (h/where [:is-not :owl:propertyChainAxiom nil]) (h/select :owl:propertyChainAxiom) query)
  (get-by-uri "rdat:P70001")
  )

;; Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn by-id [{[id] :params}]
  (if-let [row (->  (h/where := :id id) (h/limit 1)
                    query first)]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body
     (str
      (html
       [:html {:lang "en"}
        [:head
         [:title "RDA Visualizer"]
         [:link {:rel "stylesheet" :href "https://unpkg.com/sakura.css/css/sakura.css" :type "text/css"}]
         [:style (raw styles)]]
        [:body
         [:main
          [:h1 (:*type row) ": " (label row)]
          [:div.subtitle (:*uri row)]
          (same-uri-as row)
          [:dl
           (general-attributes row)
           (case (:*type row)
             "class" (list (subclass-of row)
                           (subclassed-by row))
             "property" (list (property-attributes row)
                              (subproperty-of row)
                              (superproperty-of row)))]]]]))}
    {:status 404}))

;; Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def routes {[:get #"/([a-zA-Z0-9:.]+)"] #'by-id})

(defn route
  "Given routes and a request, returns a response generated by a matching
   route, or a 404 if no match is found"
  [routes req]
  (if-let [[handler req']
           (->> routes
                (keep (fn [[[method pattern] handler]]
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
  (stop-server!))