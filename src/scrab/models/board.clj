(def test-graph {1 {:tiles [] :start [1 1] :end [1 5] :neighbours [{:word 2 :intersection 2}]}
                 2 {:tiles [] :start [3 1] :end [3 4] :neighbours [{:word 1 :intersection 0}]}})

(def max-key (reduce #(max %1 %2) (keys test-graph)))

;TODO: intersects

(defn find-neighbours [graph word]
  (loop [current-word (first graph)
         rest (next graph)
         neighbours []]
    (cond
      ((not rest) neighbours)
      ((intersects word current-word) (recur (first rest) (next rest) (cons current-word neighbours)))
      (:else (recur (first rest) (next rest) neighbours)))))
    

; Assumes word is in the right order
(defn add-word [graph word]
  (let [i (+ 1 (max-key graph))
        start ((first word) :position)
        end ((last word) :position)
        neighbours (find-neighbours graph word)]
    (into graph {i {:tiles (word :tiles), :start start, :end end, :neighbours neighbours}})))
        
        