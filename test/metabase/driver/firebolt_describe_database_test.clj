(ns metabase.driver.firebolt-describe-database-test
  "The schema filter in `describe-database*` must work on the reduce-only `:tables` that
  `sql-jdbc.sync/describe-database` returns since Metabase 0.63.22. Runs without a Firebolt connection."
  (:require
   [clojure.test :refer :all]
   [metabase.driver :as driver]
   [metabase.driver.firebolt]
   [metabase.driver.sql-jdbc.sync :as sql-jdbc.sync]))

(def ^:private tables
  [{:name "orders"   :schema "public"}
   {:name "events"   :schema "analytics_raw"}
   {:name "sessions" :schema "analytics_agg"}
   {:name "secrets"  :schema "internal"}])

(defn- reduce-only
  "Like the `:tables` Metabase returns: reducible, but `seq` throws."
  [coll]
  (reify clojure.lang.IReduceInit
    (reduce [_ rf init]
      (reduce rf init coll))))

(defn- describe [details]
  (with-redefs [sql-jdbc.sync/describe-database (fn [_driver _database]
                                                  {:tables (reduce-only tables)})]
    (driver/describe-database* :firebolt {:engine :firebolt, :details details})))

(defn- table-names [result]
  (into #{} (map :name) (:tables result)))

(deftest ^:parallel uses-describe-database*-test
  (testing "Firebolt implements describe-database*, so Metabase's resilient-connection wrapper still applies"
    (is (some? (get-method driver/describe-database* :firebolt)))
    (is (not= (get-method driver/describe-database* :firebolt)
              (get-method driver/describe-database* :sql-jdbc)))
    (is (nil? (get (methods driver/describe-database) :firebolt))
        "no direct describe-database override")))

(deftest no-schema-filter-test
  (testing "no filter configured: the result is returned untouched and never realized"
    (doseq [details [{} {:schemas nil} {:schemas {:filter-type "only" :schemas []}}
                     {:schemas {:filter-type "all" :schemas "public"}}]]
      (testing (pr-str details)
        (let [result (describe details)]
          (is (thrown? IllegalArgumentException (seq (:tables result)))
              "still the original reduce-only value")
          (is (= #{"orders" "events" "sessions" "secrets"} (table-names result))))))))

(deftest schema-filter-on-reducible-tables-test
  (testing "only"
    (is (= #{"events" "sessions"}
           (table-names (describe {:schemas {:filter-type "only" :schemas "analytics_*"}}))))
    (is (= #{"orders" "secrets"}
           (table-names (describe {:schemas {:filter-type "only" :schemas ["public" "internal"]}})))))
  (testing "exclude"
    (is (= #{"orders" "events" "sessions"}
           (table-names (describe {:schemas {:filter-type "exclude" :schemas "internal"}})))))
  (testing "plain string means only these"
    (is (= #{"orders"}
           (table-names (describe {:schemas "public"})))))
  (testing "the filtered :tables can be reduced more than once, as sync does"
    (let [result (describe {:schemas {:filter-type "exclude" :schemas "internal"}})]
      (is (= (table-names result) (table-names result))))))
