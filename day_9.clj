(ns aoc.day-9
  (:require [clojure.string :refer [split split-lines]]
            [clojure.set :refer [difference union]]))

(def input-file "day_9.txt")

(defn parse-input []
  (map (fn [i] (map #(Integer/parseInt %) (split i #""))) (split-lines (slurp input-file))))

(def data (parse-input))

(defn neighbours [m n [x y]]
  (filter
    (fn [[x y]] (and (>= x 0) (>= y 0) (< y m) (< x n)))
    [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]))

(defn get-point [[x y]]
  (nth (nth data y) x))

(defn low-point-coords []
  (let [get-neighbours (partial neighbours (count data) (count (nth data 0)))
        filter-neighbours (fn [pred [x y]] (filter pred (map get-point (get-neighbours [x y]))))]
    (filter identity (apply concat (map-indexed (fn [cur-y row]
                                                       (map-indexed
                                                         (fn [cur-x point] (if (empty? (filter-neighbours #(<= % point) [cur-x cur-y])) [cur-x cur-y]))
                                                         row))
                                                     data)))))

(defn navigate-basin [[cur & remaining] in-basin visited]
  (if (nil? cur)
    in-basin
    (let [get-neighbours (partial neighbours (count data) (count (nth data 0)))
          filter-neighbours (fn [pred [x y]] (filter pred (get-neighbours [x y])))
          cur-point (get-point cur)
          neighbours-in-basin (set (filter-neighbours (fn [c] (let [p (get-point c)] (and (< p 9) (> p cur-point)))) cur))
          to-visit (concat remaining (difference neighbours-in-basin visited))]
      (recur to-visit (union neighbours-in-basin in-basin) (conj visited cur-point)))))

(defn part-one []
  (reduce + (map inc (map get-point (low-point-coords)))))

(defn part-two []
  (reduce * (take-last 3 (sort (map count (map #(conj (navigate-basin [%] #{} #{}) %) (low-point-coords)))))))
