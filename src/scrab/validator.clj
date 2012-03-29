(ns scrab.validator)

;; These functions validate a scrabble board, assuming in all
;; cases that graph is a valid undirected graph

; Takes a sorted list of words and functions 1 and 2 which return coordinates the words are sorted in  
(defn no-overlapping-words-lin? [coord1 coord2 words]
  (loop [w1 (first words)
         w2 (second words)
         ws (rest (rest words))]
    (cond
      (not (and w1 w2)) true
      (> (coord2 (w1 :end)) (coord2 (w2 :start))) false
      :else (recur w2 (first ws) (rest ws)))))

; Checks horizontal words don't overlap, same for vertical ones
(defn no-overlapping-words? [graph]
  (let 
    [vert (sort #(compare (vec (reverse (%1 :start))) 
                          (vec (reverse (%2 :start))))
                (filter #(= (first (% :start))
                            (first (% :end)))
                        (vals graph))) 
     horiz (sort #(compare (%1 :start)
                           (%2 :start))
                 (filter #(= (second (% :start))
                             (second (% :end)))
                         (vals graph)))]
    (and (no-overlapping-words-lin? #(first %) #(second %) vert)
         (no-overlapping-words-lin? #(second %) #(first %) horiz))))

; Marks all reachable words from the first word returns false
; if not all words are marked afterwards
(defn mark-graph [graph]
  (loop [current-key (first (keys graph))
         remaining-keys []   
         marked-graph graph]
    (if current-key
      (let
        [new-remaining-keys
         (filter
           #(not
              (contains? (marked-graph %) :marked))
           (concat (map :word ((graph current-key) :neighbours))
                   remaining-keys)) 
         new-marked-graph (assoc-in marked-graph
                                    [current-key :marked]
                                    true)]
        (recur (first new-remaining-keys) 
               (rest new-remaining-keys)
               new-marked-graph))
      marked-graph))) 

(defn no-disconnected-words? [graph]
  (let [marked-graph (mark-graph graph)]
  (reduce #(and %1 (contains? (marked-graph %2) :marked))
                        true
                        (keys marked-graph)))) 

; Does all the above checks
(defn valid-graph? [graph]
  (and (no-overlapping-words? graph) (no-disconnected-words? graph)))


; Checks that all the words are real words
; Dict is a (sorted-)set
(defn check-valid-word [graph dict]
  (reduce #(and %1 (contains? dict (:word %2))) true (vals graph)))
