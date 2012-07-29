(ns yuan.game.game-tree
  (:use [clojure.tools.trace]))

(def ^:dynamic *ai-level* 5)

(defn game-tree
  [make-move board]
  {:board board
   :moves (map (partial game-tree make-move)
                  (make-move board))})

(defn limit-tree-depth [tree]
  (letfn [(f [tree depth]
            (update-in tree
                       [:moves]
                       (if (pos? depth)
                         (partial map (fn [tree] (f tree (dec depth))))
                         (constantly ()))))]
    (f tree *ai-level*)))

(defn random-agent [tree]
  (rand-nth (:moves tree)))
