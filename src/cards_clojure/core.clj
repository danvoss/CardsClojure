(ns cards-clojure.core
  (:gen-class))

(def suits [:clubs :spades :hearts :diamonds])
(def ranks (range 1 14)) ;; range is exclusive

(defn create-deck []
  (set
    (for [suit suits 
          rank ranks]
      {:suit suit :rank rank})))
 
(defn create-hands [deck]
  (set
    (for [c1 deck
          c2 (disj deck c1)
          c3 (disj deck c1 c2)
          c4 (disj deck c1 c2 c3)]
      #{c1 c2 c3 c4})))

(defn flush? [hand] 
  (let [suits (set (map :suit hand))]
    (= 1 (count suits))))

(comment
  (defn straight? [hand]
    (let [ranks (vec (sort (map :rank hand)))]
      (and (= 1 (- (get ranks 3) (get ranks 2)))
           (= 1 (- (get ranks 2) (get ranks 1)))
           (= 1 (- (get ranks 1) (get ranks 0)))))))

(defn straight? [hand]
  (let [ranks (sort (map :rank hand))
        first-rank (first ranks)
        new-ranks (range first-rank (+ 4 first-rank))]
    (= ranks new-ranks)))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))
      
(defn four-of-a-kind? [hand]
  (let [ranks (set (map :rank hand))]
    (= 1 (count ranks))))

(defn three-of-a-kind? [hand]
  (let [ranks (map :rank hand)]
    (contains? (set (vals (frequencies ranks))) 3)))
    
(defn two-pair? [hand]
  (let [ranks (map :rank hand)]
    (= 2 (count (filter #(= 2 %) (vals (frequencies ranks)))))))
  
(defn -main []
  (let [deck (create-deck)
        hands (create-hands deck)
        two-pairs (filter two-pair? hands)]
    (count two-pairs)))
