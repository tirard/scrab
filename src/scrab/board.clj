(ns scrab.board)

(defn- end-points-from-word [word]
  (let [x1 (first (word :start))
        x2 (first (word :end))
        y1 (last (word :start))
        y2 (last (word :end))]    
     [x1 y1 x2 y2]))

(defn- intersects [word1 word2]
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

(defn- intersects? [word1 word2]
  (not (nil? (intersects word1 word2))))

(defn- add-to-intersections [graph word]
  (let [wordi (count (graph :words))]
    (loop [i 0
           w (first (graph :words))
           ws (rest (graph :words))
           new-intersections []]
      (if-not w
        new-intersections
        (if (intersects? w word)
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

(defn add-word [graph word-tiles]
  {:pre [(contains? graph :words)
         (contains? graph :intersections)]}
  (let
    [start ((first word-tiles) :position)
     end ((last word-tiles) :position)
     word {:tiles word-tiles, :start start, :end end}
     new-words (conj (graph :words) word) ;FIXME: may need to change existing words
     new-intersections (add-to-intersections graph word)]
    (assoc graph :intersections new-intersections 
                 :words new-words)))
