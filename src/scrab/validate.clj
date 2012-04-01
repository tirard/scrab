(ns scrab.validate)

;; These functions validate a scrabble board, assuming in all
;; cases that graph is a valid undirected graph

; Takes a sorted list of words and functions 1 and 2 which return coordinates the words are sorted in  
(defn- no-overlapping-words-lin? [coord1 coord2 words]
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
                        (graph :words))) 
     horiz (sort #(compare (%1 :start)
                           (%2 :start))
                 (filter #(= (second (% :start))
                             (second (% :end)))
                         (graph :words)))]
    (and (no-overlapping-words-lin? #(first %) #(second %) vert)
         (no-overlapping-words-lin? #(second %) #(first %) horiz))))

; Marks all reachable words from the first word returns false
; if not all words are marked afterwards
(defn- mark-words [graph]
  (loop [current (if (> (count (graph :words)) 0) 0 nil) 
         remaining []   
         marked-words (graph :words)]
    (if current
      (let
        [new-remaining
         (filter
           #(not
              (contains? (marked-words %) :marked))
           (concat (map
                     #(if (= (first %) current) (second %) (first %))
                     (filter (partial some (partial = current))
                             (map :words (graph :intersections))))
                   remaining)) 
         new-marked-words (assoc-in marked-words
                                    [current :marked]
                                    true)]
        (recur (first new-remaining) 
               (rest new-remaining)
               new-marked-words))
      (assoc-in graph [:words] marked-words)))) 

(defn no-disconnected-words? [graph]
  (let [marked-graph (mark-words graph)]
  (reduce #(and %1 (contains? %2 :marked))
                        true
                        (marked-graph :words)))) 

; Does all the above checks
(defn valid-graph? [graph]
  {:pre [(contains? graph :words)
         (contains? graph :intersections)]}

  (and (no-overlapping-words? graph) (no-disconnected-words? graph)))


; Checks that all the words are real words
; Dict is a (sorted-)set
(defn valid-word? [graph dict]
  (reduce #(and %1 (contains? dict (:word %2))) true (vals graph)))
