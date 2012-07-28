(ns yuan.game.game-tree)

(def ^:dynamic *ai-level* 5)

(defn game-tree
  [make-move board]
  {:board board
   :moves (map (partial game-tree make-move)
                  (make-move board))})

(defn limit-tree-depth [tree & {:keys [depth] :or {depth *ai-level*}}]
  (update-in tree
             [:moves]
             (if (pos? depth)
               (partial map (fn [tree] (limit-tree-depth tree :depth (dec depth))))
               (constantly ()))))
