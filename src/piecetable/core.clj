(ns piecetable.core
  (:require [clojure.data.finger-tree :as ft]
            [clojure.pprint :as pp]))

(deftype Piece [fname offset len])

(defn split-piece [p offset]
  (let [l (.len p) b (.fname p) o (.offset p)]
    [(Piece. b o offset)
     (Piece. b (+ o offset) (- l offset))]))

(def pt (atom nil))

(defn clear-piece-table [] (reset! pt (ft/finger-tree
                   (ft/meter .len 0 +)))
  nil)

(swap! pt conj (Piece. :orig 0 500))
(swap! pt conj (Piece. :app 0 50))
(swap! pt conj (Piece. :orig 500 200))
(swap! pt conj (Piece. :app 200 100))

(defn split-table [t offset]
  (let [[l m r] (ft/split-tree t #(> % offset))
        mlen (.len m)
        diff (- offset (ft/measured l))]
    (cond (= 0 diff)
          [l (ft/consl r m)]
          (> diff mlen)
          [(conj l m) r]
          :else
          (let [[l-chunk r-chunk] (split-piece m diff)]
            [(conj l l-chunk) (ft/consl r r-chunk)]))))

(defn delete-piece [tree offset len]
  ;; could be a recursive solution too...
  ;; find the seams, delete, stich back together
  (let [[l _] (split-table tree offset)
        [_ r] (split-table tree (+ offset len))]
    (ft/ft-concat l r)))

(defn insert-piece [tree offset piece]
  (let [[l r] (split-table tree offset)]
    (ft/ft-concat (conj l piece) r)))


