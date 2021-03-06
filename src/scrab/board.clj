(ns scrab.board)
; Functions for adding a word to the board

(defn end-points-from-word [word]
  (let [x1 (first (word :start))
        x2 (first (word :end))
        y1 (last (word :start))
        y2 (last (word :end))]    
     [x1 y1 x2 y2]))

(defn intersects [word1 word2]
  (let [[x1 y1 x2 y2] (end-points-from-word word1)
        [x3 y3 x4 y4] (end-points-from-word word2)
        denom (- (* (- y4 y3) (- x2 x1))
                 (* (- x4 x3) (- y2 y1)))
        ua-num (- (* (- x4 x3) (- y1 y3))
                  (* (- y4 y3) (- x1 x3)))
        ub-num (- (* (- x2 x1) (- y1 y3))
                  (* (- y2 y1) (- x1 x3)))]        
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

(defn intersects? [word1 word2]
  (not (nil? (intersects word1 word2))))

; Updates the intersections in the graph
(defn add-to-intersections [graph word]
  (let [wordi (.indexOf (graph :words) word)]
    (loop [i 0
           w (first (graph :words))
           ws (rest (graph :words))
           new-intersections []]
      (if-not w
        new-intersections
        (if (and (intersects? w word) (not (= w word))) 
          (recur (inc i)
                 (first ws)
                 (rest ws)
                 (cons {:words [i wordi]
                        :at (intersects word w)}
                       new-intersections))
          (recur (inc i)
                 (first ws)
                 (rest ws)
                 new-intersections))))))

; Creates a word and updates the words in the graph accordingly
; This may involve merging already existing words into the new one
; Some of the words in the graph will need to be extended with tiles
; form those being added
(defn follows? [word1 word2 order] 
  (and
    (= (inc (second (order (word1 :end))))
       (second (order (word2 :start))))
    (= (first (order (word1 :start)))
       (first (order (word2 :end))))))

(defn join [{ts1 :tiles s1 :start} {ts2 :tiles e2 :end}]
  {:tiles (concat ts1 ts2)
   :start s1
   :end e2})

; Makes one-letter words out of tiles
(defn wordify [word-tiles]
  (map
    (fn [tile]
      {:start (:position tile) :end (:position tile) :tiles [tile]})
    word-tiles))

; Makes one letter words which could will form new words with tiles
; TODO: correct this
(defn wordify-perp [words word-tiles order]
  (let
    [front (first (order ((first word-tiles) :position)))
     back (first (order ((last word-tiles) :position)))
     line (second (order ((first word-tiles) :position)))
     same-line? (fn [tile]
                  (some (partial = (first (order (:position tile))))
                        (map (comp first order :position) word-tiles)))
     adjacent? (fn [tile]
                 (or (some (partial = (inc (second (order (:position tile)))))
                       (map (comp second order :position) word-tiles))
                     (some (partial = (dec (second (order (:position tile)))))
                           (map (comp second order :position) word-tiles))))
     ;adjacent? (fn [tile]
     ;            (some (partial = 1)
     ;                  [(- (second (order (tile :position))) line)
     ;                   (- line (second (order (tile :position))))]))
     perp-words (filter #(= (second (order (:start %)))
                            (second (order (:end %)))) words)
     same-line-tiles (filter same-line? 
                            (reduce concat (map :tiles perp-words)))
     adjacent-tiles (filter adjacent? same-line-tiles)] 
    (wordify adjacent-tiles)))

; Returns all the newly created or extended words
; and the corresponding graph
; order decides whether it does vertical or horizontal words
(defn add-to-words-lin [words word-tiles order]
  (let
    [tile-words (wordify word-tiles)
     perp-words (wordify-perp words word-tiles order) 
     all-words (concat words tile-words perp-words)
     in-line-words (filter #(= (first (order (:start %))) (first (order (:end %))))
                           all-words)
     sorted-words (sort-by #(vec (order (:start %))) in-line-words)
     other-words (remove #(some (partial = %) in-line-words) all-words)]
    (loop
      [w1 (first sorted-words)
       w2 (second sorted-words)
       ws (drop 2 sorted-words)
       new-words []]
      (if-not (and w1 w2)
        (filter #(not= (:start %) (:end %)) (concat other-words new-words [w1])) 
        (if (follows? w1 w2 order)
          (recur (join w1 w2)
                 (first ws)
                 (rest ws)
                 new-words)
          (recur w2
                 (first ws)
                 (rest ws)
                 (conj new-words w1)))))))

(defn add-to-words-vert [words word-tiles]
  (add-to-words-lin words word-tiles identity))

(defn add-to-words-horiz [words word-tiles]
  (add-to-words-lin words word-tiles reverse))


(defn add-to-words [words word-tiles]
  (let [new-words1 (add-to-words-horiz words word-tiles)
        new-words2 (add-to-words-vert new-words1 word-tiles)]
    new-words2))

; Adds a list of tiles to a graph which represents a board
(defn add-word [graph word-tiles]
  {:pre [(contains? graph :words)
         (contains? graph :intersections)]}
  (let
    [start ((first word-tiles) :position)
     end ((last word-tiles) :position)
     new-words (add-to-words (graph :words) word-tiles)
     new-intersections (add-to-intersections graph new-words)]
    (assoc-in graph [:intersections] new-intersections)))
