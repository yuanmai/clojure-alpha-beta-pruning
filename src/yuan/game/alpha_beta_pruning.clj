(ns yuan.game.alpha-beta-pruning
  (:use [clojure.tools.trace]
        [yuan.game.game-tree]))

(def ^:dynamic *current-player*)
(def ^:dynamic *turn-fn*)
(def ^:dynamic *eval-fn*)

(declare ab-rate-position)

(defn ab-get-rating-max [{:keys [moves board] :as tree} upper-limit lower-limit]
  (letfn [(f [[move & more] lower-limit]
            (if-let [x (and move
                            (ab-rate-position move upper-limit lower-limit))]
              (conj (if (>= x upper-limit)
                      ()
                      (f more (max x lower-limit)))
                    x)))]
    (f moves lower-limit)))

(defn ab-get-rating-min [{:keys [moves board] :as tree} upper-limit lower-limit]
  (letfn [(f [[move & more] upper-limit]
            (if-let [x (and move
                            (ab-rate-position move upper-limit lower-limit))]
              (conj (if (<= x lower-limit)
                      ()
                      (f more (min x upper-limit)))
                    x)))]
    (f moves upper-limit)))

(defn ab-rate-position [{:keys [moves board] :as tree} upper-limit lower-limit]
  (if (seq moves)
    (if (= (*turn-fn* board) *current-player*)
      (apply max (ab-get-rating-max tree
                                    upper-limit
                                    lower-limit))
      (apply min (ab-get-rating-min tree
                                    upper-limit
                                    lower-limit)))
    (*eval-fn* *current-player* board)))

(defn make-agent
  [turn-fn eval-fn]
  (fn [{:keys [moves board] :as tree}]
    {:pre [(seq moves)]}
    (binding [*turn-fn* turn-fn
              *eval-fn* eval-fn
              *current-player* (trace :turn (turn-fn board))]
      (let [ratings (trace :ratings
                           (ab-get-rating-max (limit-tree-depth tree)
                                              1
                                              -1))
            best (apply max ratings)]
        (:board (nth moves (.indexOf ratings best)))))))
