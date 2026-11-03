(ns metabase.driver.firebolt-mbql5-test
  "Compiles MBQL 5 queries with the Firebolt driver against mock metadata, so no Firebolt connection is needed.
  Guards the Metabase 64+ changes: `:sql` compiles MBQL 5 directly (options map is the second element of a clause)
  and `sql.qp/honey-sql-version` no longer exists."
  (:require
   [clojure.string :as str]
   [clojure.test :refer :all]
   [metabase.driver :as driver]
   [metabase.driver.firebolt]
   [metabase.driver.sql.query-processor :as sql.qp]
   [metabase.lib.core :as lib]
   [metabase.lib.options :as lib.options]
   [metabase.lib.test-metadata :as meta]
   [metabase.lib.test-util :as lib.tu]
   [metabase.query-processor.compile :as qp.compile]))

(defn- mp [engine]
  (lib.tu/merged-mock-metadata-provider meta/metadata-provider {:database {:engine engine}}))

(defn- compile-filter [engine filter-fn]
  (let [mp    (mp engine)
        query (lib/query mp (meta/table-metadata :venues))
        query (lib/filter query (filter-fn query))]
    (driver/with-driver engine
      (:query (qp.compile/compile-with-inline-parameters query)))))

(defn- case-insensitive [clause]
  (lib.options/update-options clause assoc :case-sensitive false))

(deftest ^:parallel driver-loads-test
  (testing "the namespace loads and registers every method (it used to stop at `honey-sql-version` on 64+)"
    (is (isa? driver/hierarchy :firebolt :sql-jdbc))
    (is (some? (get-method sql.qp/->honeysql [:firebolt :between])))
    (is (not= (get-method sql.qp/->honeysql [:firebolt :between])
              (get-method sql.qp/->honeysql [:sql :between]))
        "Firebolt must not override the shared [:sql :between] method")))

(deftest ^:parallel string-filters-test
  (let [name-col #(meta/field-metadata :venues :name)]
    (testing ":contains"
      (let [sql (compile-filter :firebolt (fn [_] (lib/contains (name-col) "BBQ")))]
        (is (re-find #"STRPOS\(\"PUBLIC\"\.\"VENUES\"\.\"NAME\", 'BBQ'\) > 0" sql) sql))
      (let [sql (compile-filter :firebolt (fn [_] (case-insensitive (lib/contains (name-col) "bbq"))))]
        (is (re-find #"STRPOS\(LOWER\(\"PUBLIC\"\.\"VENUES\"\.\"NAME\"\), LOWER\('bbq'\)\) > 0" sql) sql)))
    (testing ":starts-with"
      (let [sql (compile-filter :firebolt (fn [_] (lib/starts-with (name-col) "Red")))]
        (is (re-find #"REGEXP_EXTRACT\(\"PUBLIC\"\.\"VENUES\"\.\"NAME\", '\^' \|\| REGEXP_REPLACE_ALL\('Red'" sql) sql)
        (is (re-find #", ''\) IS NOT NULL" sql) sql))
      (let [sql (compile-filter :firebolt (fn [_] (case-insensitive (lib/starts-with (name-col) "red"))))]
        (is (re-find #", 'i'\) IS NOT NULL" sql) sql)))
    (testing ":ends-with"
      (let [sql (compile-filter :firebolt (fn [_] (lib/ends-with (name-col) "Grill")))]
        (is (re-find #"REGEXP_EXTRACT\(\"PUBLIC\"\.\"VENUES\"\.\"NAME\", REGEXP_REPLACE_ALL\('Grill'.* \|\| '\$'" sql) sql)))))

(deftest ^:parallel between-test
  (testing "Firebolt wraps BETWEEN in parentheses"
    (let [sql (compile-filter :firebolt (fn [_] (lib/between (meta/field-metadata :venues :price) 1 3)))]
      (is (str/includes? sql "WHERE (\"PUBLIC\".\"VENUES\".\"PRICE\" BETWEEN 1 AND 3)") sql)))
  (testing "other SQL drivers are not affected"
    (let [sql (compile-filter :h2 (fn [_] (lib/between (meta/field-metadata :venues :price) 1 3)))]
      (is (str/includes? sql "WHERE \"PUBLIC\".\"VENUES\".\"PRICE\" BETWEEN 1 AND 3") sql))))

(deftest ^:parallel regex-match-first-test
  (let [mp    (mp :firebolt)
        query (-> (lib/query mp (meta/table-metadata :venues))
                  (lib/expression "first_word" (lib/regex-match-first (meta/field-metadata :venues :name) "^\\w+")))
        sql   (driver/with-driver :firebolt (:query (qp.compile/compile-with-inline-parameters query)))]
    (is (re-find #"REGEXP_EXTRACT\(\"PUBLIC\"\.\"VENUES\"\.\"NAME\", '\^\\w\+'\) AS \"first_word\"" sql) sql)))

(deftest ^:parallel temporal-bucketing-test
  (testing "date functions still compile (64 changed metabase.util.date-2 signatures)"
    (let [mp    (mp :firebolt)
          date  (meta/field-metadata :checkins :date)
          base  (lib/query mp (meta/table-metadata :checkins))]
      (doseq [unit [:day :week :month :quarter :year :day-of-week :month-of-year]]
        (testing unit
          (let [query (-> base
                          (lib/aggregate (lib/count))
                          (lib/breakout (lib/with-temporal-bucket date unit)))
                sql   (driver/with-driver :firebolt (:query (qp.compile/compile-with-inline-parameters query)))]
            (is (string? sql))
            (is (re-find #"(?i)date_trunc|extract" sql) sql))))
      (testing "relative date filter (add-interval)"
        (let [query (lib/filter base (lib/time-interval date -30 :day))
              sql   (driver/with-driver :firebolt (:query (qp.compile/compile-with-inline-parameters query)))]
          (is (re-find #"(?i)date_add" sql) sql))))))
