(def test-graph {1 {:tiles [] :start [1 1] :end [1 5] :neighbours [{:word 2 :intersection 2}]}
                 2 {:tiles [] :start [3 1] :end [3 4] :neighbours [{:word 1 :intersection 0}]}})

(def max-key (reduce #(max %1 %2) (keys test-graph)))

; TODO intersects
; returns true? or returns value?
                                        ; use cond-let maybe, to bind to intersects

(defn coords [word other-word]
  (let [x1 (first (word :start))
        x2 (first (word :end))
        y1 (last (word :start))
        y2 (last (word :end))
        x3 (first (other-word :start))
        x4 (first (other-word :end))
        y3 (last (other-word :start))
        y4 (last (other-word :end))]
    [x1 y1 x2 y2 x3 y3 x4 y4]))

(defn intersects [x1 y1 x2 y2 x3 y3 x4 y4]
  (let [denom (- (* (- y4 y3)
                    (- x2 x1))
                 (* (- x4 x3)
                    (- y2 y1)))
        ua-num (- (* (- x4 x3)
                     (- y1 y3))
                  (* (- y4 y3)
                     ( - x1 x3)))
        ub-num (- (* (- x2 x1)
                     (- y1 y3))
                  (* (- y2 y1)
                     (- x1 x3)))]
    (cond
      (and (= 0 denom)
           (= 0 ua-num)
           (= 0 ub-num))
      [x1 y1]      
      (= 0 denom) nil
      :else
      (let [ua (/ ua-num denom)
            ub (/ ub-num denom)]
        (if (and (<= 0 ua 1)
                 (<= 0 ub 1))
          [(+ x1 (* ua (- x2 x1))) 
           (+ y1 (* ua (- y2 y1)))] 
          nil)))))


(defn find-neighbours [graph word]
  (loop [current-word (first graph)
         rest (next graph)
         neighbours []]
    (cond
      (not rest) neighbours 
      (apply intersects (coords word (val current-word))) ; almost there -
      ; need to cons right value
      (recur (first rest) (next rest) (cons current-word neighbours))      
      :else (recur (first rest) (next rest) neighbours))))



; Assumes word is in the right order         
(defn add-word [graph word]
  (let [i (+ 1 (max-key graph))
        start ((first word) :position)
        end ((last word) :position)
        neighbours (find-neighbours graph word)]
    (into graph {i {:tiles (word :tiles), :start start, :end end, :neighbours neighbours}})))

