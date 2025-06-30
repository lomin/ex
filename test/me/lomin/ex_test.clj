(ns me.lomin.ex-test
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [clojure.xml :as xml]
   [com.rpl.specter :as sp]
   [malli.core :as m]
   [malli.generator :as mg]
   [me.lomin.ex :as ex]
   [me.lomin.sinho.matcher :refer [=*]])
  (:import
   (java.util
    Calendar
    Date)
   (javax.xml.parsers
    SAXParserFactory)))


(defn- non-validating
  [s ch]
  (-> (doto
       (SAXParserFactory/newInstance)
        (.setFeature
         "http://apache.org/xml/features/nonvalidating/load-external-dtd" false))
      (.newSAXParser)
      (.parse s ch)))


(def currencies
  [:enum
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


(defn retrieve-currencies
  [xml-input-stream]
  (reduce
   (fn [m {:keys [rate currency]}] (assoc m (keyword currency) rate))
   {}
   (sequence
    (comp (map :attrs)
          (map #(update % :rate edn/read-string)))
    (get-in (xml/parse xml-input-stream non-validating) [:content 2 :content 0 :content]))))


(defn ->xml-file-input-stream
  [path]
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


(defn- add-1d
  [^Date d]
  (.getTime (let [cal (Calendar/getInstance)]
              (doto cal
                (.setTime d)
                (.add Calendar/DATE 1)))))


(defn calculate-offer
  [exchange-request now currencies]
  (merge (select-keys exchange-request [:from :to])
         {:valid-until (add-1d now)
          :rate        (get currencies (:to exchange-request))}))


(defn valid-request?
  [exchange-request]
  (m/validate request-schema exchange-request))


(def inst<= (comp not pos? compare))


(defn valid-offer?
  [offer now]
  (inst<= now (:valid-until offer)))


(defn save-order!
  [& [_exchange-request]]
  #uuid"767df20e-93dd-4312-9258-c0f06216fffa")


(defn ok
  [msg & {:as data}]
  (merge data
         {:status :ok
          :msg    msg}))


(defn error
  [msg & {:as data}]
  (merge data
         {:status :error
          :msg    msg}))


(defn now!
  []
  #inst"2022-05-15T09:18:05.099-00:00")


(defn retrieve-offer!
  [exchange-request]
  exchange-request)


(defn save-offer!
  [offer]
  offer)


(defn offer-exchange
  [exchange-request]
  (ex/with-ex exchange-request
    (ex/with-examples [exchange-request (mg/generate request-schema)]
      (when (not (valid-request? exchange-request))
        (ex/exit :request/invalid))
      (let [offer
            (ex/exchange :offer (retrieve-offer! exchange-request)
                         :valid-offer (assoc exchange-request
                                             :valid-until #inst "2030-05-15T09:18:05.099-00:00")
                         :invalid-offer (mg/generate request-schema)
                         :currencies.retrieve/failed (ex/exit :currencies.retrieve/failed
                                                              "test error"))]
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
  (is (= nil
         (ex/parse-try '((catch RuntimeException e e)))))

  (let [form '(do (catch RuntimeException data body))]
    (is (= '{:body [do]
             :catch-clauses
             [#malli.core.Tags {:values {:token catch
                                         :exits+exceptions
                                         [#malli.core.Tag
                                           {:key :exception :value RuntimeException}]
                                         :binding data
                                         :body [body]}}]
             :finally-clause nil}
           (ex/parse-try '(do (catch RuntimeException data body)))))
    (is (= nil
           (ex/->exit-dispatch (ex/parse-try form))))
    (is (= nil
           (ex/->ex-info-dispatch (ex/parse-try form))))
    (is (= nil
           (ex/->ex-info-dispatch (ex/parse-try form))))
    (is (= '((catch RuntimeException data body))
           (ex/->exception-clauses (ex/parse-try form))))
    (is (= nil
           (ex/->finally-clause (ex/parse-try form)))))

  (let [form '(do (catch IllegalArgumentException :test/failure data (prn "test") data))]
    (is (= '{:body [do]
             :catch-clauses
             [#malli.core.Tags {:values
                                {:token catch
                                 :exits+exceptions
                                 [#malli.core.Tag
                                   {:key :exception :value IllegalArgumentException}
                                  #malli.core.Tag {:key :exit :value :test/failure}]
                                 :binding data
                                 :body [(prn "test") data]}}]
             :finally-clause nil}
           (ex/parse-try form)))

    (is (= nil
           (ex/->ex-info-dispatch (ex/parse-try form))))
    (is (= nil
           (ex/->ex-info-dispatch (ex/parse-try form))))
    (is (= '((catch IllegalArgumentException data (prn "test") data))
           (ex/->exception-clauses (ex/parse-try form))))
    (is (= nil
           (ex/->finally-clause (ex/parse-try form)))))

  (let [form '(do (catch [:data :var] data body))]
    (is (= '{:body [do]
             :catch-clauses
             [#malli.core.Tags {:values
                                {:token catch
                                 :exits+exceptions
                                 [#malli.core.Tag
                                   {:key :ex-info-navigators :value [:data :var]}]
                                 :binding data
                                 :body [body]}}]
             :finally-clause nil}
           (ex/parse-try form)))

    (is (= nil
           (ex/->exit-dispatch (ex/parse-try form))))
    (is (= '()
           (ex/->exception-clauses (ex/parse-try form))))
    (is (= nil
           (ex/->finally-clause (ex/parse-try form)))))

  (let [form '((do this)
               (then that)
               (catch ::something-else {:as my-ex-data :keys [foo bar]}
                 (do-something foo bar)))]
    (is (= '{:body [(do this) (then that)]
             :catch-clauses
             [#malli.core.Tags
               {:values {:token catch
                         :exits+exceptions [#malli.core.Tag
                                             {:key :exit
                                              :value :me.lomin.ex-test/something-else}]
                         :binding {:as my-ex-data :keys [foo bar]}
                         :body [(do-something foo bar)]}}]
             :finally-clause nil}
           (ex/parse-try form)))

    (is (= nil
           (ex/->ex-info-dispatch (ex/parse-try form))))
    (is (= nil
           (ex/->ex-info-dispatch (ex/parse-try form))))
    (is (= '()
           (ex/->exception-clauses (ex/parse-try form))))
    (is (= nil
           (ex/->finally-clause (ex/parse-try form)))))

  (is (=*
       '{:body [(throw (ex-info "ex-msg" {:a 1}))]
         :catch-clauses
         [{:values {:token catch
                    :exits+exceptions [{:key :exit :value :me.lomin.ex-test/foo}]
                    :binding data
                    :body [(swap! state conj [:me.lomin.ex-test/foo data])]}}
          {:values {:token catch
                    :exits+exceptions [{:key :exit :value :me.lomin.ex-test/bar}]
                    :binding {:as data :keys [foo]}
                    :body [(swap! state conj [:me.lomin.ex-test/bar data foo])]}}
          {:values {:token catch
                    :exits+exceptions [{:key :exception
                                        :value clojure.lang.ExceptionInfo}]
                    :binding e
                    :body [(swap! state conj [:exception-info (ex-data e)])]}}
          {:values {:token catch
                    :exits+exceptions [{:key :exception :value Exception}]
                    :binding e
                    :body [(swap! state conj :exception)]}}]
         :finally-clause {:values {:token finally
                                   :body [(swap! state conj :finally)]}}}
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


(defn traced-test-function-0
  [opts]
  (ex/with-ex opts (let [x 1] (ex/exchange :ex-test/traced (do (swap! side-effects conj "side-effect!") (+ x 0))
                                           :some-key 2
                                           :some-other-key 3
                                           :lazy (swap! side-effects conj "not allowed!")
                                           :nil nil))))


(defn traced-test-function-1
  [opts]
  (ex/with-ex opts (let [x 1] (ex/exchange :ex-traced-2 (+ x 0)
                                           :some-key 2
                                           :some-other-key 3))))


(declare test-log)
(declare test-opts)

(derive :ex-test.traced/child :ex-test/traced)


(defmethod ex/ex :ex-test.traced/child [context k]
  :multi-method)


(def trace-state (atom []))
(def side-effects (atom []))


(defn init-test-ctx
  [m]
  (reset! trace-state [])
  (reset! side-effects [])
  (assoc m :ex/trace! (fn [x] (swap! trace-state conj x))))


(defn add-logs
  [x]
  (prn x)
  [x @trace-state @side-effects])


(deftest exchange-test
  (is (= (let [minus (ex/exchange :minus (- a b)
                                  :default- -100)]
           (ex/exchange :multiply (* minus 2)))
         -200))

  (is (=* [1
           [[:ex-test/traced #:ex.ex{:result 1 :code '(do (swap! side-effects conj "side-effect!") (+ 1 0))}]]
           ["side-effect!"]]
          (add-logs (traced-test-function-0 (init-test-ctx {}))) 1))

  (is (=* [1
           [[:ex-traced-2 #:ex.ex{:result 1 :code '(+ 1 0)}]]
           []]
          (add-logs (traced-test-function-1 (init-test-ctx {}))) 1))

  (is (=* [22
           [[:ex-test/traced #:ex.ex{:result 22}]]
           []]
          (add-logs (traced-test-function-0 (init-test-ctx {:ex-test/traced (fn [_opts _config] 22)})))))

  (is (=* [2
           [[:ex-test/traced #:ex.ex{:result 2}]]
           []]
          (add-logs (traced-test-function-0 (init-test-ctx {:ex-test/traced :some-key}))) 2))

  (is (=* [:multi-method
           [[:ex-test/traced {}]]
           []]
          (add-logs (traced-test-function-0 (init-test-ctx {:ex-test/traced :ex-test.traced/child})))))

  (is (=* [nil
           [[:ex-test/traced #:ex.ex{:result nil}]]
           []]
          (add-logs (traced-test-function-0 (init-test-ctx {:ex-test/traced :nil}))) 1)))


(defmacro demonstrate
  [var-or-expr name-or-expr & body]
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
       {:ex.as/expr          '(+ 1 2)
        :ex.as/sym           '(+ 1 2)
        :ex.as/other-bindings []
        :ex.as/forms         '($ [(prn $)])}
       (ex/destruct '(+ 1 2) '$ '[(prn $)])))
  (is (=
       {:ex.as/expr          '$
        :ex.as/sym           '$
        :ex.as/other-bindings []
        :ex.as/forms         '((+ 1 2) nil)}
       (ex/destruct '$ '(+ 1 2) nil)))
  (is (=
       {:ex.as/expr          '$
        :ex.as/sym           '$
        :ex.as/other-bindings []
        :ex.as/forms         '((prn 1 2) [(+ 3 4)])}
       (ex/destruct '$ '(prn 1 2) '[(+ 3 4)])))
  (is (=
       {:ex.as/expr          '(+ 1 2)
        :ex.as/sym           '(+ 1 2)
        :ex.as/other-bindings []
        :ex.as/forms         '($ nil)}
       (ex/destruct '(+ 1 2) '$ nil)))
  (is (=
       {:ex.as/expr          '$
        :ex.as/sym           '$
        :ex.as/other-bindings []
        :ex.as/forms         '($ nil)}
       (ex/destruct '$ '$ nil))))

  (is (=
       {:ex.as/expr          'system
        :ex.as/sym           '$
        :ex.as/other-bindings []
        :ex.as/forms         '((prn $))}
       (ex/destruct '[$ system] '(prn $))))

  (is (=
       {:ex.as/expr          'system
        :ex.as/sym           '$
        :ex.as/other-bindings '[x 1 y 2]
        :ex.as/forms         '((+ $ x y))}
       (ex/destruct '[$ system x 1 y 2] '(+ $ x y))))


(defn nested-1
  [ctx]
  (ex/with-ex ctx ctx))


(defn nested-0
  [ctx]
  (ex/with-ex ctx
    (nested-1 ctx)))


(defn with-id-seq
  [m]
  (let [state (atom -1)]
    (assoc m :ex/generate-id! (fn [] (swap! state inc)))))


(deftest with-ex-tracing-test
  (is (=* {:ex.trace/parent-id nil,
           :ex.trace/id        0}
          (ex/with-ex [$ (with-id-seq {})]
            $)))

  (is (=* {:ex.trace/parent-id 0,
           :ex.trace/id        1}
          (nested-0 (with-id-seq {}))))

  (is (=* {:outer              [{:inner [2 3]}],
           :ex.trace/parent-id nil,
           :ex.trace/id        0}
          (ex/with-ex [$ (with-id-seq {:outer [{:inner [1 2]}]})]
            (sp/transform [:outer sp/FIRST :inner sp/ALL] inc $))))

  (is (= 2
         (ex/with-ex [$ {:test 1}]
           (let [$ {:test 2}]
             (:test $)))))

  (is (=* {:ex.trace/id        0
           :ex.trace/parent-id nil
           :actual             1
           :expected           1}
          (let [$ {:unexpected 2}]
            (ex/with-ex [$ (with-id-seq {:actual 1})] (merge $ {:expected 1})))))

  (is (= 3
         (ex/with-ex {:actual 1} (+ 1 2))))

  (is (=* [{:ex.trace/parent-id nil
            :ex.trace/id        0}
           {:ex.trace/parent-id 0
            :ex.trace/id        1}
           {:ex.trace/parent-id 1
            :ex.trace/id        2}]
          (let [_   (reset! trace-state [])
                ctx (-> {}
                        (init-test-ctx)
                        (with-id-seq))]
            (ex/with-ex ctx
              ((:ex/trace! ctx) ctx)
              (ex/with-ex ctx
                ((:ex/trace! ctx) ctx)
                (ex/with-ex ctx
                  ((:ex/trace! ctx) ctx))))
            @trace-state))))


(deftest additional-bindings-test
  (is (= "a" (ex/with-ex {} (ex/example "a" 1))))

  (is (= 7
         (ex/with-ex [$ {:test 3}
                      x 2
                      y 2] 
           (+ (:test $) x y))))

  (is (= 5
         (ex/with-ex [$ {:test 3}
                      {t :test :as system} $] 
                      (is (:ex.trace/id system))
           (+ t 2)))))

(defmacro wrapper [& body]
  `(ex/with-ex [~'$system {:test 1}
                ~'$test (update ~'$system :test inc)]
     (+ (:test ~'$test) (do ~@body))))

(comment
  (macroexpand '(wrapper (+ 1 1))))

(deftest wrapper-macro-test
  (is (= 4 (wrapper (+ 1 1)))))