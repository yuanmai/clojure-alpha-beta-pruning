(ns yuan.game.alpha-beta-pruning
  (:use [clojure.tools.trace]
        [yuan.game.game-tree]))

(def ^:dynamic *current-player*)
(def ^:dynamic *turn-fn*)
(def ^:dynamic *eval-fn*)

(declare ab-rate-position)

(defn ab-get-rating-max [upper-limit lower-limit {:keys [moves board] :as tree}]
  (letfn [(f [[move & more] lower-limit]
            (if-let [x (and move
                            (ab-rate-position upper-limit lower-limit move))]
              (conj (if (>= x upper-limit)
                      ()
                      (f more (max x lower-limit)))
                    x)))]
    (f moves lower-limit)))

(defn ab-get-rating-min [upper-limit lower-limit {:keys [moves board] :as tree}]
  (letfn [(f [[move & more] upper-limit]
            (if-let [x (and move
                            (ab-rate-position upper-limit lower-limit move))]
              (conj (if (<= x lower-limit)
                      ()
                      (f more (min x upper-limit)))
                    x)))]
    (f moves upper-limit)))

(defn ab-rate-position [upper-limit lower-limit {:keys [moves board] :as tree}]
  (if (seq moves)
    (if (= (*turn-fn* board) *current-player*)
      (apply max (ab-get-rating-max upper-limit
                                    lower-limit
                                    tree))
      (apply min (ab-get-rating-min upper-limit
                                    lower-limit
                                    tree)))
    (*eval-fn* *current-player* board)))

(defn opportunistic [f]
  (fn g [tree]
    (let [ratings (f tree)]
      (if (and (> *ai-level* 1)
               (apply = ratings))
        (binding [*ai-level* (dec *ai-level*)]
          (g tree))
        ratings))))

(defn make-agent
  [turn-fn eval-fn]
  (fn best-move [{:keys [moves board] :as tree}]
    {:pre [(seq moves)]}
    (binding [*turn-fn* turn-fn
              *eval-fn* eval-fn
              *current-player* (trace :turn (turn-fn board))]
      (let [f (opportunistic (comp (partial ab-get-rating-max
                                            (game-scores :win)
                                            (game-scores :lose))
                                   limit-tree-depth))
            ratings (trace :ratings (f tree))
            best (apply max ratings)]
        (nth moves (.indexOf ratings best))))))
