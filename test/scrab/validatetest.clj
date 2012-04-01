(ns scrab.validatetest
  (:use [clojure.test])
  (:use [scrab.validate]))

;TODO: make some more valid tests
(defn valid-graphs [f]
  (do
    (is (f {:words [{:start [1 1] :end [5 1]}
                    {:start [3 1] :end [3 4]}]
            :intersections [{:words [0 1] :at [3 1]}]}))))

(defn invalid-graphs [f]
  (do
    (is (f {:words [{:start [1 1] :end [5 1]}
                    {:start [4 1] :end [7 1]}]
            :intersections []})
        "Overlapping words on same line") 

    (is (f {:words [{:start [1 1] :end [5 1]}
                    {:start [2 1] :end [4 1]}]
            :intersections []})
        "Words contained within others on same line") 

    (is (f {:words [{:tiles [] :start [1 1] :end [5 1]}
                    {:tiles [] :start [4 2] :end [9 2]}]
            :intersections []})
        "Disconnected words") 

    (is (f {:words [{:start [1 1] :end [5 1] :neighbours [{:word 2 :intersection 2}]}
                    {:start [3 3] :end [3 6] :neighbours [{:word 2 :intersection 2}]}
                    {:start [3 1] :end [3 4] :neighbours [{:word 1 :intersection 0}]}]
            :intersections [{:words [0 2] :at [3 1]}
                            {:words [1 2] :at [3 4]}]}))))

(deftest valid-board
         (valid-graphs valid-graph?)
         (invalid-graphs (comp not valid-graph?))) 
