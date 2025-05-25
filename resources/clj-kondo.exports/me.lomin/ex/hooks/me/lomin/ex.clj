(ns hooks.me.lomin.ex
  (:require [clj-kondo.hooks-api :as api]))

(defn with-ex [{:keys [node]}]
  (let [children (rest (:children node))
        ;; First child is always the system
        system-expr (first children)
        ;; Analyze remaining children to find if there's a catch
        rest-children (rest children)
        ;; Look for a child that starts with 'catch'
        catch-index (->> rest-children
                         (map-indexed vector)
                         (filter (fn [[_ child]]
                                   (when-let [token (some-> child :children first)]
                                     (= 'catch (api/sexpr token)))))
                         (map first)
                         first)
        ;; Separate body and catch clause
        body-exprs (if catch-index
                     (take catch-index rest-children)
                     rest-children)
        catch-expr (when catch-index
                     (nth rest-children catch-index))
        ;; Extract catch parts if it exists
        catch-parts (when catch-expr
                      (let [catch-children (rest (:children catch-expr))]
                        [(first catch-children)         ; error type (keyword)
                         (second catch-children)        ; error binding (symbol)
                         (drop 2 catch-children)]))     ; catch body
        ;; Create catch node with proper binding analysis if it exists
        new-catch (if catch-expr
                    (let [[error-type-node bind-node body] catch-parts
                          binding-symbol (api/sexpr bind-node)]
                      (api/list-node
                       (list
                        (api/token-node 'catch)
                        (api/token-node 'Exception)
                        bind-node
                          ;; Create a let node to make clj-kondo understand the binding
                          ;; Include the error-type in a binding to preserve its highlighting
                        (api/list-node
                         (list
                          (api/token-node 'let)
                          (api/vector-node
                           [(api/token-node '_error_type)
                            error-type-node]) ; Keep the original keyword node
                          (api/list-node
                           (list*
                            (api/token-node 'do)
                            body)))))))
                    ;; Add a dummy catch when none is provided
                    (api/list-node
                     (list
                      (api/token-node 'catch)
                      (api/token-node 'Exception)
                      (api/token-node '_dummy_e)
                      (api/token-node nil))))
        ;; Build the transformed node as a try/catch
        new-node (api/list-node
                  (list*
                   (api/token-node 'try)
                   (api/list-node
                    (list*
                     (api/token-node 'let)
                     (api/vector-node [(api/token-node '_system) system-expr])
                     body-exprs))
                   [new-catch]))]
    {:node new-node}))