(ns scrab.board)

(defn end-points-from-tiles [word-tiles]
  (let [x1 (first ((first word-tiles) :position))
        x2 (first ((first word-tiles)
                    :position))
        y1 (last ((first word-tiles)
                   :position))
        y2 (last ((last word-tiles)
                  :position))]    
    [x1 y1 x2 y2]))

(defn end-points-from-word [word]
  (let [x1 (first (word :start))
        x2 (first (word :end))
        y1 (last (word :start))
        y2 (last (word :end))]    
     [x1 y1 x2 y2]))

(defn intersects [word-tiles word]
  (let [[x1 y1 x2 y2] (end-points-from-tiles word-tiles)
        [x3 y3 x4 y4] (end-points-from-word word)
        denom (- (* (- y4 y3)
                    (- x2 x1)))
        ua-num (- (* (- x4 x3) (- y1 y3)) (* (- y4 y3) (- x1 x3)))
        ub-num (- (* (- x2 x1)
                     (- y1 y3))
                  (* (- y2 y1)
                     (- x1 x3)))]        
    (cond
      (and (zero? denom)
           (zero? ua-num)
           (zero? ub-num))
      [x1 y1]      
      (zero? denom) nil
      :else
      (let [ua (/ ua-num denom)
            ub (/ ub-num denom)]
        (if (and (<= 0 ua 1)
                 (<= 0 ub 1))
          [(+ x1 (* ua (- x2 x1))) 
           (+ y1 (* ua (- y2 y1)))] 
          nil)))))

;; works, uses word structure similar to in graph
(defn find-neighbours [graph word-tiles]
  (loop [current-word (first graph)
         rest (next graph)
         neighbours []]
    (cond
      (not current-word) neighbours
      (intersects word-tiles (val current-word))
      (recur (first rest)
             (next rest)
             (conj
              neighbours
              {:word (key current-word)
               :intersection (intersects word-tiles (val current-word))}))
      :else (recur (first rest) (next rest) neighbours))))

; Assumes word is in the right order
; what is the word structure?
(defn add-word [graph word-tiles]
  (let [i (inc (max-key graph))
        start ((first word-tiles) :position)
        end ((last word-tiles) :position)
        neighbours (find-neighbours graph word-tiles)
        word {i {:tiles word-tiles, :start start, :end end, :neighbours neighbours}}]
    (loop [neighbour (graph ((first neighbours) :word))
           next-neighbours (rest neighbours)
           neighbour-key ((first neighbours) :word)
           new-graph {}]
      (if neighbour
        (recur (graph (first next-neighbours))
               (next next-neighbours)
               ((first next-neighbours) :word)
               (conj new-graph
                     {neighbour-key {:tiles (neighbour :tiles)
                                     :start (neighbour :start)
                                     :end (neighbour :end)
                                     :neighbours (conj (neighbour :neighbours)
                                                       {:word i
                                                        :intersection (get-inter
                                       }
                     ))
        new-graph
        ))))
