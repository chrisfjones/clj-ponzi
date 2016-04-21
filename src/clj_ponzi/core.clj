(ns clj-ponzi.core
  (:require [datomic.api :as d])
  (:import [datomic.db.Db]))

(defn attr
  "Sugar for attribute creation, looks like:
  (attr :person/name :string :one \"The person's name\")
  (attr :person/address :ref :one \"(optional) Reference to their address\")
  (attr :person/pizzas :string :many \"All the pizzas\")"
  [ident type cardinality doc]
  {:db/id (d/tempid :db.part/db)
   :db/ident ident
   :db/valueType (keyword "db.type" (name type))
   :db/cardinality (keyword "db.cardinality" (name cardinality))
   :db/doc doc
   :db.install/_attribute :db.part/db})
(def key-attr (comp #(assoc % :db/unique :db.unique/identity) attr))
(def unique-attr (comp #(assoc % :db/unique :db.unique/value) attr))
(def component-attr (comp #(assoc % :db/isComponent true) attr))

(defn hydrate
  "Like touch, only this goes deep and strips namespaces from the keys (good for json export)"
  ([db e] (hydrate db 3 e))
  ([db max-depth e] (hydrate db max-depth 0 e))
  ([db max-depth depth e]
   (if (or (>= depth max-depth) (nil? e))
     e
     (let [attrs (->> (cond
                       (instance? datomic.query.EntityMap e) e
                       (or (coll? e) (set? e) (instance? java.util.HashSet e)) (->> e seq flatten first (d/entity db))
                       (number? e) (d/entity db e)
                       :else (throw (Exception. (str "hydrate: Don't know how to deal with " (class e)))))
                      d/touch
                      seq)]
       (loop [attrs attrs
              result {}]
         (let [[k v] (first attrs)
               v (cond
                  (set? v) (map (partial hydrate db max-depth (inc depth)) (seq v))
                  (instance? datomic.query.EntityMap v) (hydrate db max-depth (inc depth) v)
                  :else v)
               result (assoc result (keyword (name k)) v)]
           (if (empty? (rest attrs))
             result
             (recur (rest attrs) result))))))))

(defn prefix-keys [m prefix]
  (->> (seq m)
       (map (fn [[k v]] [(keyword (name prefix) (name k)) v]))
       flatten
       (apply hash-map)))

(defn resolve-dbid
  "Grabs the first dbid that meets the criteria, looks like:
  (resolve-dbid db :person/name \"Steve\" :person/age 26) => 16297461"
  [db & attr-pairs]
  (if (not (instance? datomic.db.Db db))
    (throw (Exception. "first param needs to be a db silly"))
    (let [id (when (even? (count attr-pairs))
               (-> {:find ['?e]
                    :in ['$]
                    :where (->> (partition 2 attr-pairs)
                                (map #(vector '?e (first %) (second %))))}
                   (d/q db)
                   ffirst))]
      (when-not id (throw (Exception. (str "unable to resolve dbid with " (apply str (interleave attr-pairs (cycle ["=" ", "])))))))
      id)))

(defn resolve-dbid-quietly [db & attr-pairs]
  (try
    (apply (partial resolve-dbid db) attr-pairs)
    (catch Exception e nil)))

;;;;;;;;;;;;;;
;; auto-inc ;;
;;;;;;;;;;;;;;

(defn- auto-inc-ident [attr]
  (keyword
   (if (namespace attr)
     (str (namespace attr) "/" (name attr) "-auto-inc")
     (str (name attr) "-auto-inc"))))

(defn current-id-for-attr
  ([db attr] (current-id-for-attr ::current-id db attr))
  ([ident db attr]
   (get (d/entity db (auto-inc-ident attr)) ident)))

(defn set-current-id-for-attr
  ([conn attr v] (set-current-id-for-attr ::current-id conn attr v))
  ([ident conn attr v]
   (d/transact conn [{:db/id [:db/ident (auto-inc-ident attr)]
                      ident v}])))

(defn auto-inc-attr
  ([] (auto-inc-attr ::current-id))
  ([current-id-ident] (auto-inc-attr current-id-ident "Used for auto-incrementing ids via the db.fn/auto-inc function"))
  ([current-id-ident doc]
   {:db/id (d/tempid :db.part/db)
    :db/ident current-id-ident
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/noHistory true
    :db/doc doc
    :db.install/_attribute :db.part/db}))

(defn auto-inc-fn
  ([] (auto-inc-fn ::current-id))
  ([auto-inc-attr-ident] (auto-inc-fn auto-inc-attr-ident 0))
  ([auto-inc-attr-ident starting-value]
   {:db/id (d/tempid :db.part/db)
    :db/ident :db.fn/auto-inc
    :db/fn (d/function {:lang "clojure"
                        :params ['db 'eid 'attr]
                        :code (str "(let [auto-inc-ident (keyword (if (namespace attr)
                                   (str (namespace attr) \"/\" (name attr) \"-auto-inc\")
                                   (str (name attr) \"-auto-inc\")))
                                   [auto-inc-tx next-id] (if-let [existing-auto-inc (datomic.api/entity db auto-inc-ident)]
                                   (let [v (inc (get existing-auto-inc " auto-inc-attr-ident "))]
                                   [[:db/add auto-inc-ident " auto-inc-attr-ident " v] v])
                                   (let [a :a]
                                   [{:db/id (datomic.api/tempid :db.part/db)
                                   :db/ident auto-inc-ident
                                   " auto-inc-attr-ident " " starting-value "} " starting-value "]))
                                   tx [auto-inc-tx
                                   [:db/add eid attr next-id]]]
                                   tx)")
                        })}))

