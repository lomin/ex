(ns me.lomin.ex-test
  (:require [clojure.test :refer :all]
            [clojure.xml :as xml]
            [clojure.java.io :as io]
            [com.rpl.specter :as sp]
            [me.lomin.ex :as ex]
            [malli.core :as m]
            [malli.generator :as mg]
            [clojure.edn :as edn]
            [me.lomin.sinho.matcher :refer [=*]])
  (:import (javax.xml.parsers SAXParserFactory)
           (java.util Calendar Date)))

(defn- non-validating [s ch]
  (-> (doto
        (SAXParserFactory/newInstance)
        (.setFeature
          "http://apache.org/xml/features/nonvalidating/load-external-dtd" false))
      (.newSAXParser)
      (.parse s ch)))

(def currencies [:enum
                 :EUR
                 :NZD
                 :SGD
                 :HUF
                 :MYR
                 :ZAR
                 :PHP
                 :RON
                 :TRY
                 :MXN
                 :AUD
                 :ISK
                 :KRW
                 :JPY
                 :BRL
                 :IDR
                 :GBP
                 :SEK
                 :ILS
                 :DKK
                 :HRK
                 :HKD
                 :CNY
                 :BGN
                 :CZK
                 :CAD
                 :PLN
                 :THB
                 :USD
                 :NOK
                 :INR
                 :CHF])

(defn retrieve-currencies [xml-input-stream]
  (reduce
    (fn [m {:keys [rate currency]}] (assoc m (keyword currency) rate))
    {}
    (sequence
      (comp (map :attrs)
            (map #(update % :rate edn/read-string)))
      (get-in (xml/parse xml-input-stream non-validating) [:content 2 :content 0 :content]))))

(defn ->xml-file-input-stream [path]
  (-> (slurp path) .getBytes io/input-stream))

(defn retrieve-currencies!
  "simulating retrieval from https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/index.en.html"
  []
  (retrieve-currencies (->xml-file-input-stream "test/resources/exchange.xml")))

(def request-schema
  [:map
   [:from currencies]
   [:to currencies]
   [:amount double?]])

(defn- add-1d [^Date d]
  (.getTime (let [cal (Calendar/getInstance)]
              (doto cal
                (.setTime d)
                (.add Calendar/DATE 1)))))

(defn calculate-offer [exchange-request now currencies]
  (merge (select-keys exchange-request [:from :to])
         {:valid-until (add-1d now)
          :rate        (get currencies (:to exchange-request))}))

(defn valid-request? [exchange-request] (m/validate request-schema exchange-request))

(def inst<= (comp not pos? compare))

(defn valid-offer? [offer now]
  (inst<= now (:valid-until offer)))

(defn save-order!
  [& [_exchange-request]]
  #uuid"767df20e-93dd-4312-9258-c0f06216fffa")

(defn ok [msg & {:as data}]
  (merge data
         {:status :ok
          :msg    msg}))

(defn error [msg & {:as data}]
  (merge data
         {:status :error
          :msg    msg}))

(defn now! []
  #inst"2022-05-15T09:18:05.099-00:00")

(defn retrieve-offer! [exchange-request]
  exchange-request)

(defn save-offer! [offer]
  offer)

(defn offer-exchange [exchange-request]
  (ex/with-ex exchange-request
              (ex/with-examples [exchange-request (mg/generate request-schema)]
                (when (not (valid-request? exchange-request))
                  (ex/exit :request/invalid))
                (let [offer (ex/exchange :offer (retrieve-offer! exchange-request)
                                         :valid-offer (assoc exchange-request :valid-until #inst"2030-05-15T09:18:05.099-00:00")
                                         :invalid-offer (mg/generate request-schema)
                                         :currencies.retrieve/failed (ex/exit :currencies.retrieve/failed "test error"))]
                  (if-let [order-id (and (valid-offer? offer (now!)) (save-order! exchange-request))]
                    (ok (str "Your order with the id " order-id " has been saved!")
                        :order-id order-id)
                    (ok "Based on your request, we would like to make you the following offer:"
                        :offer (save-offer! (calculate-offer exchange-request (now!) (retrieve-currencies!)))))))
              (catch :request/invalid _e
                (error "Request is not valid."))
              (catch :currencies.retrieve/failed e
                (error "We are currently unable to make an offer. Try later." :fail e))
              (catch :offer.save/failed e
                (error "Your offer could not be saved. Try later." :fail e))
              (catch :order.save/failed e
                (error "Your order could not be saved. Try later." :fail e))))


(deftest catch-parsing-test
  (is (=* :malli.core/invalid
          (ex/parse-try '((catch RuntimeException e e)))))

  (is (=* {:catch-clauses [{:token            'catch
                            :body             '[body]
                            :exits+exceptions '[[:exception RuntimeException]],
                            :binding          'data}]}
          (ex/parse-try '(do (catch RuntimeException data body)))))

  (is (=* {:catch-clauses [{:token            'catch
                            :exits+exceptions [[:exception 'IllegalArgumentException]
                                               [:exit :test/failure]]
                            :binding          'data
                            :body             '[(prn "test") data]}]}
          (ex/parse-try '(do (catch IllegalArgumentException :test/failure data (prn "test") data)))))

  (is (=* {:catch-clauses [{:token            'catch
                            :body             '[body]
                            :exits+exceptions '[[:ex-info-navigators [:data :var]]],
                            :binding          'data}]}
          (ex/parse-try '(do (catch [:data :var] data body)))))

  (is (=* {:body          '[(do this) (then that)]
           :catch-clauses [{:token            'catch,
                            :exits+exceptions [[:exit ::something-else]],
                            :binding          '{:as my-ex-data, :keys [foo bar]},
                            :body             '[(do-something foo bar)]}]}
          (ex/parse-try '((do this)
                          (then that)
                          (catch ::something-else {:as my-ex-data :keys [foo bar]}
                            (do-something foo bar))))))

  (is (=
        {:body           '[(throw (ex-info "ex-msg" {:a 1}))],
         :catch-clauses  '[{:token            catch,
                            :exits+exceptions [[:exit ::foo]],
                            :binding          data,
                            :body             [(swap! state conj [::foo data])]}
                           {:token            catch,
                            :exits+exceptions [[:exit ::bar]],
                            :binding          {:as data, :keys [foo]},
                            :body             [(swap! state conj [::bar data foo])]}
                           {:token            catch,
                            :exits+exceptions [[:exception clojure.lang.ExceptionInfo]],
                            :binding          e,
                            :body             [(swap! state conj [:exception-info (ex-data e)])]}
                           {:token            catch,
                            :exits+exceptions [[:exception Exception]],
                            :binding          e,
                            :body             [(swap! state conj :exception)]}],
         :finally-clause '{:token finally, :body [(swap! state conj :finally)]}}
        (ex/parse-try '((throw (ex-info "ex-msg" {:a 1}))
                        (catch ::foo data (swap! state conj [::foo data]))
                        (catch ::bar {:as data :keys [foo]} (swap! state conj [::bar data foo]))
                        (catch clojure.lang.ExceptionInfo e (swap! state conj [:exception-info (ex-data e)]))
                        (catch Exception e (swap! state conj :exception))
                        (finally (swap! state conj :finally)))))))


(deftest try+-test

  (is (= :something (ex/try+ :something)))

  (let [state (atom [])]
    (ex/try+
      (throw (ex-info "ex-msg" {:a 1}))
      (catch ::foo data (swap! state conj [::foo data]))
      (catch ::bar {:as data :keys [foo]} (swap! state conj [::bar data foo]))
      (catch clojure.lang.ExceptionInfo e (swap! state conj [:exception-info (ex-data e)]))
      (catch Exception e (swap! state conj :exception))
      (finally (swap! state conj :finally)))
    (is (= [[:exception-info {:a 1}] :finally] @state)))

  (let [state (atom [])]
    (ex/try+
      (derive ::bar ::foo)
      (ex/exit ::bar "fail-msg" {:b 2 :c 3})
      (catch ::bar {:as data :keys [c]} (swap! state conj [::bar data c]))
      (catch ::foo data (swap! state conj [::foo data]))
      (catch clojure.lang.ExceptionInfo e (swap! state conj [:exception-info (ex-data e)]))
      (catch Exception _e (swap! state conj :exception))
      (finally (swap! state conj :finally)))
    (is (= [[::bar
             {:b        2
              :c        3
              :exit/msg "fail-msg"
              :type     ::bar} 3]
            :finally]
           @state)))

  (let [state (atom [])]
    (ex/try+
      (ex/exit ::bar "fail-msg" {:b 2 :c 3})
      (catch ::bar data (swap! state conj [::foo data])))
    (is (= [[::foo
             {:b 2, :c 3, :exit/msg "fail-msg", :type ::bar}]]
           @state)))

  (underive ::bar ::foo))

(deftest integration-test
  (is (= {:status :error, :msg "Request is not valid."}
         (offer-exchange {})))
  (is (= {:order-id #uuid"767df20e-93dd-4312-9258-c0f06216fffa",
          :status   :ok,
          :msg      "Your order with the id 767df20e-93dd-4312-9258-c0f06216fffa has been saved!"}
         (offer-exchange {:from        :EUR
                          :to          :USD
                          :valid-until #inst"2022-05-15T09:18:05.099-00:00"
                          :amount      100.0})))
  (is (= {:offer  {:from :EUR, :rate 1.0385, :to :USD, :valid-until #inst "2022-05-16T09:18:05.099-00:00"},
          :status :ok,
          :msg    "Based on your request, we would like to make you the following offer:"}
         (offer-exchange {:from        :EUR
                          :to          :USD
                          :valid-until #inst"2019-05-15T09:18:05.099-00:00"
                          :amount      100.0})))
  (is (= {:fail   #:exit{:_/type :currencies.retrieve/failed, :msg "test error"},
          :status :error
          :msg    "We are currently unable to make an offer. Try later."}
         (offer-exchange {:from   :EUR
                          :to     :USD
                          :amount 100.0
                          :offer  :currencies.retrieve/failed
                          :ex/gen :ex.gen/exits}))))

(declare side-effects)

(defn traced-test-function-0 [opts]
  (ex/with-ex opts (let [x 1] (ex/exchange :ex-traced (do (swap! side-effects conj "side-effect!") (+ x 0))
                                           :some-key 2
                                           :some-other-key 3
                                           :lazy (swap! side-effects conj "not allowed!")))))

(defn traced-test-function-1 [opts]
  (ex/with-ex opts (let [x 1] (ex/exchange :ex-traced-2 (+ x 0)
                                           :some-key 2
                                           :some-other-key 3))))

(declare test-log)
(declare test-opts)

(deftest exchange-test
  (def test-log (atom []))
  (def side-effects (atom []))
  (def test-opts {:ex/trace! #(swap! test-log conj %)})

  (is (= (let [minus (ex/exchange :minus (- a b)
                                  :default- -100)]
           (ex/exchange :multiply (* minus 2)))
         -200))

  (is (= (traced-test-function-0 (merge test-opts {:ex-traced false})) 1))
  (is (=* [[:ex-traced #:ex.ex{:result 1 :code '(do (swap! side-effects conj "side-effect!") (+ 1 0))}]]
          @test-log))
  (is (=* ["side-effect!"] @side-effects))

  (is (= (traced-test-function-1 test-opts) 1))
  (is (=* [[:ex-traced #:ex.ex{:result 1}]
           [:ex-traced-2 #:ex.ex{:result 1 :code '(+ 1 0)}]]
          @test-log))
  (is (=* ["side-effect!"] @side-effects))

  (is (= (traced-test-function-0 (merge test-opts {:ex-traced (fn [_opts _config] 22)})) 22))
  (is (=* '[[:ex-traced #:ex.ex{:result 1}]
            [:ex-traced-2 #:ex.ex{:result 1}]
            [:ex-traced #:ex.ex{:result 22}]]
          @test-log))
  (is (=* ["side-effect!"] @side-effects))

  (is (= (traced-test-function-0 (merge test-opts {:ex-traced :some-key})) 2))
  (is (=* [[:ex-traced #:ex.ex{:result 1}]
           [:ex-traced-2 #:ex.ex{:result 1}]
           [:ex-traced #:ex.ex{:result 22}]
           [:ex-traced #:ex.ex{:result 2}]]
          @test-log))
  (is (=* ["side-effect!"] @side-effects)))

(defmacro demonstrate [var-or-expr name-or-expr & body]
  (mapv (comp type) [var-or-expr name-or-expr body]))

(deftest with-ex-tracing-tools-test
  (is (= [clojure.lang.PersistentList
          clojure.lang.Symbol
          clojure.lang.PersistentList]
         (demonstrate (+ 1 2) $ (prn $))))
  (is (= [clojure.lang.Symbol
          clojure.lang.PersistentList
          nil]
         (demonstrate $ (+ 1 2))))
  (is (= [clojure.lang.Symbol
          clojure.lang.PersistentList
          clojure.lang.PersistentList]
         (demonstrate $ (prn 1 2) (+ 3 4))))
  (is (= [clojure.lang.Symbol
          clojure.lang.Symbol
          nil]
         (demonstrate $ $)))
  (is (=
        {:ex.as/expr  '(+ 1 2)
         :ex.as/sym   '$
         :ex.as/forms '[(prn $)]}
        (ex/destruct '(+ 1 2) '$ '[(prn $)])))
  (is (=
        {:ex.as/expr  '$
         :ex.as/sym   '$
         :ex.as/forms '[(+ 1 2)]}
        (ex/destruct '$ '(+ 1 2) nil)))
  (is (=
        {:ex.as/expr  '$
         :ex.as/sym   '$
         :ex.as/forms '[(prn 1 2) (+ 3 4)]}
        (ex/destruct '$ '(prn 1 2) '[(+ 3 4)])))
  (is (=
        {:ex.as/expr  '(+ 1 2)
         :ex.as/sym   '$
         :ex.as/forms '[$]}
        (ex/destruct '(+ 1 2) '$ nil)))
  (is (=
        {:ex.as/expr  '$
         :ex.as/sym   '$
         :ex.as/forms '[$]}
        (ex/destruct '$ '$ nil))))

(defn nested-1 [ctx]
  (ex/with-ex ctx ctx))

(defn nested-0 [ctx]
  (ex/with-ex ctx
              (nested-1 ctx)))

(defn with-id-seq [m]
  (let [state (atom -1)]
    (assoc m :ex/generate-id! (fn [] (swap! state inc)))))

(def trace-state (atom []))

(defn with-trace [m]
  (assoc m :ex/trace! (fn [x] (swap! trace-state conj x))))

(deftest with-ex-tracing-test
  (is (=* {:ex.trace/parent-id nil,
           :ex.trace/id        0}
          (ex/with-ex (with-id-seq {}) $
                      $)))

  (is (=* {:ex.trace/parent-id 0,
           :ex.trace/id        1}
          (nested-0 (with-id-seq {}))))

  (is (=* {:outer              [{:inner [2 3]}],
           :ex.trace/parent-id nil,
           :ex.trace/id        0}
          (ex/with-ex (with-id-seq {:outer [{:inner [1 2]}]}) $
                      (sp/transform [:outer sp/FIRST :inner sp/ALL] inc $))))

  (is (= 2
         (ex/with-ex {:test 1} $
                     (let [$ {:test 2}]
                       (:test $)))))

  (is (=* {:ex.trace/id        0
           :ex.trace/parent-id nil
           :actual             1
           :expected           1}
          (let [$ {:unexpected 2}]
            (ex/with-ex (with-id-seq {:actual 1}) $ (merge $ {:expected 1})))))

  (is (= 3
         (ex/with-ex {:actual 1} (+ 1 2))))

  (is (=* [{:ex.trace/parent-id nil
            :ex.trace/id 0}
            {:ex.trace/parent-id 0
            :ex.trace/id 1}
            {:ex.trace/parent-id 1
            :ex.trace/id 2}]
         (let [_   (reset! trace-state [])
               ctx (-> {}
                       (with-trace)
                       (with-id-seq))]
           (ex/with-ex ctx
                       ((:ex/trace! ctx) ctx)
                       (ex/with-ex ctx
                                   ((:ex/trace! ctx) ctx)
                                   (ex/with-ex ctx
                                               ((:ex/trace! ctx) ctx))))
           @trace-state))))