(ns piecetable.core
  ^{:doc "Implementation of a persistent piece-table"
      :author "Ghadi Shayban"}
  (:require [clojure.data.finger-tree :as ft]
            [clojure.pprint :as pp]))

;; implementation of a piece-table data structure
;;
;; typical use: text editor buffers
;; bit.ly/o2WaPp has a nice paper on the subject
;;
;; Get an empty one with (empty-piece-table) and (insert-piece) or (delete-region)
;; from it
;;
;; gshayban@gmail.com
;; 


;; fname is a keyword pointing to a buffer, either the original or the append buffer
;; offset -> relative to the buffer, not the whole structure
;; len -> length
(deftype Piece [fname offset len])

(defn split-piece 
  "Splits a piece into two 
  at the specified offset"
  [p offset]
  (let [l (.len p) 
        b (.fname p) 
        o (.offset p)]
    [(Piece. b o offset)
     (Piece. b (+ o offset) (- l offset))]))

(defn empty-piece-table 
  "makes an empty piece table, with the size of the initial file"
  [size]
  (conj (ft/finger-tree (ft/meter .len 
                            0
                            +))
        (Piece. :orig 0 size)))

(defn split-table 
  "Splits the table (a finger tree) into two
  finger trees at the specified offset.  If the offset
  is within a Piece, it will split that Piece"
  [t offset]
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

(defn delete-region 
  "Deletes a region from the whole structure.
  Implemented by making two seams in the finger-tree,
  omitting the middle, and reassembling.
  So elegant it hurts"
  [pt offset len]
  ;; could be a recursive solution too...
  ;; find the seams, delete, stich back together
  (let [[l _] (split-table pt offset)
        [_ r] (split-table pt (+ offset len))]
    (ft/ft-concat l r)))

(defn insert-piece 
  "Makes a seam at the specified offset
  and inserts the piece"
  [pt offset piece]
  (let [[l r] (split-table pt offset)]
    (ft/ft-concat (conj l piece) r)))

