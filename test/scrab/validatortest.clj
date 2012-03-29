(ns scrab.validatortest
  (:use [clojure.test])
  (:use [scrab.validator]))

;TODO: make some more valid tests
(defn valid-graphs [f]
  (do
    (is (f {1 {:start [1 1] :end [5 1] :neighbours [{:word 2}]}
            2 {:start [3 1] :end [3 4] :neighbours [{:word 1}]}}))))

(defn invalid-graphs [f]
  (do
    (is (f {1 {:start [1 1] :end [5 1]}
            2 {:start [4 1] :end [7 1]}})
        "Overlapping words on same line") 

    (is (f {1 {:start [1 1] :end [5 1]}
            2 {:start [2 1] :end [4 1]}})
        "Words contained within others on same line") 

    (is (f {1 {:tiles [] :start [1 1] :end [5 1] :neighbours []}
            2 {:tiles [] :start [4 2] :end [9 2] :neighbours []}})
        "Disconnected words") 

    (is (f {1 {:tiles [] :start [1 1] :end [5 1] :neighbours [{:word 2 :intersection 2}]}
            2 {:tiles [] :start [3 3] :end [3 6] :neighbours [{:word 2 :intersection 2}]}
            3 {:tiles [] :start [3 1] :end [3 4] :neighbours [{:word 1 :intersection 0}]}}))))

(deftest valid-board
         (valid-graphs valid-graph?)
         (invalid-graphs (comp not valid-graph?))) 
