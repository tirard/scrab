(ns scrab.boardtest
  (:use [clojure.test])
  (:use [scrab.board]))

(deftest wordify-test
         (is (= (set (wordify [{:position [1 1]}
                               {:position [1 2]}
                               {:position [1 4]}
                               {:position [1 7]}
                               {:position [1 8]}]
                              identity)) 
                #{{:start [1 1] :end [1 2] 
                   :tiles [{:position [1 1]} {:position [1 2]}]}
                  {:start [1 4] :end [1 4]
                   :tiles [{:position [1 4]}]}
                  {:start [1 7] :end [1 8]
                   :tiles [{:position [1 7]} {:position [1 8]}]}})) 

         (is (= (set (wordify [{:position [1 1]}
                               {:position [1 2]}
                               {:position [1 4]}
                               {:position [1 7]}
                               {:position [1 8]}]
                              reverse)) 
                #{{:start [1 1] :end [1 1] :tiles [{:position [1 1]}]}
                  {:start [1 2] :end [1 2] :tiles [{:position [1 2]}]}
                  {:start [1 4] :end [1 4] :tiles [{:position [1 4]}]}
                  {:start [1 7] :end [1 7] :tiles [{:position [1 7]}]}
                  {:start [1 8] :end [1 8] :tiles [{:position [1 8]}]}})))

(deftest add-to-words-test
         (is (= #{{:start [1 1] :end [1 5]
                   :tiles []}
                  {:start [1 5] :end [2 5]
                   :tiles [{:position [2 5]}]}
                  {:start [2 5] :end [2 10]
                   :tiles [{:position [2 5]}
                           {:position [2 6]}
                           {:position [2 7]}
                           {:position [2 8]}
                           {:position [2 9]}
                           {:position [2 10]}]}}
                (set (add-to-words
                       [{:start [1 1] :end [1 5] :tiles []}]
                       [{:position [2 5]}
                        {:position [2 6]}
                        {:position [2 7]}
                        {:position [2 8]}
                        {:position [2 9]}
                        {:position [2 10]}])))))


; TODO: make the tests not sensitive to order
;(deftest add-word-test
;         (is (= {:words '({:tiles [{:position [1 3]}
;                                  {:position [2 3]}
;                                  {:position [3 3]}
;                                  {:position [4 3]}
;                                  {:position [5 3]}
;                                  {:position [6 3]}
;                                  {:position [7 3]}]
;                          :start [1 3]
;                          :end [7 3]}
;                         {:tiles [{:position [1 1]}
;                                  {:position [1 2]}
;                                  {:position [1 3]}
;                                  {:position [1 4]}
;                                  {:position [1 5]}]
;                          :start [1 1]
;                          :end [1 5]})
;                 :intersections [{:words [1 0] :at [1 3]}]}
;
;                (add-word 
;                  {:words [{:tiles [{:position [1 1]}
;                                    {:position [1 2]}
;                                    {:position [1 3]}
;                                    {:position [1 4]}
;                                    {:position [1 5]}]
;                            :start [1 1]
;                            :end [1 5]}]
;                   :intersections []}
;                  [{:position [1 3]}
;                   {:position [2 3]}
;                   {:position [3 3]}
;                   {:position [4 3]}
;                   {:position [5 3]}
;                   {:position [6 3]}
;                   {:position [7 3]}]))
;             "Additional intersections")
;         
;         (is (= {:words [{:start [1 1]
;                          :end [1 6]
;                          :tiles [{:position [1 6]}]}
;                         ]
;                 :intersections []}
;                (add-word 
;                  {:words [{:start [1 1]
;                            :end [1 5]}]
;                   :intersections []}
;                  [{:position [1 6]}]))
;             "New tiles extends existing word")
;
;         ;TODO: new tiles join exisiting words
;         
;         (is (= {:words [{:start [1 1] :end [1 5]}
;                         {:start [2 4] :end [2 5]}
;                         {:start [1 4] :end [2 5]}
;                         {:start [1 5] :end [2 5]}]
;                 :intersections [{:words [0 2] :at [1 4]}
;                                 {:words [0 3] :at [1 5]}
;                                 {:words [1 2] :at [2 4]}
;                                 {:words [1 3] :at [2 5]}]}
;                (add-word {:words [{:start [1 1] :end [1 5]}]
;                           :intersections []}
;                          [{:position [2 4]} {:position [2 5]}]))))
