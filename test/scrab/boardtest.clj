(ns scrab.boardtest
  (:use [clojure.test])
  (:use [scrab.board]))

(deftest add-to-intersections-test
         (is (= {:words [{:start [1 1]
                          :end [1 5]}
                         {:tiles [{:position [1 3]}
                                  {:position [2 3]}
                                  {:position [3 3]}
                                  {:position [4 3]}
                                  {:position [5 3]}
                                  {:position [6 3]}
                                  {:position [7 3]}]
                          :start [1 3]
                          :end [7 3]}]
                 :intersections [{:words [0 1] :at [1 3]}]}

                (add-word 
                  {:words [{:start [1 1]
                            :end [1 5]}]
                   :intersections []}
                  [{:position [1 3]}
                   {:position [2 3]}
                   {:position [3 3]}
                   {:position [4 3]}
                   {:position [5 3]}
                   {:position [6 3]}
                   {:position [7 3]}]))))
