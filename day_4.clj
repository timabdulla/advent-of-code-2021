(ns advent-of-code-2021.day-4
  (:require  [clojure.set :as set :refer [difference]]
             [clojure.string :as str :refer [join split split-lines]]))

(def file "day_4.txt")

(defn process-board-input [board-input]
  (let [process-row (fn [r]
                        (->> (split r #" ")
                             (filter (complement empty?))
                             (map #(Integer/parseInt %))))
        board (vec (map #(vec (process-row %)) board-input))
        board-with-transposition (concat board (apply map vector board))]
    board-with-transposition))

(defn parse-bingo-numbers-and-boards []
  (let [[bingo-seq-line & board-lines] (split-lines (slurp file))
        bingo-numbers (map #(Integer/parseInt %) (split bingo-seq-line #","))
        board-inputs (map #(split % #"\n") (split (join "\n" (drop 1 board-lines)) #"\n\n"))
        boards (map process-board-input board-inputs)]
    [bingo-numbers boards]))

(defn calculate-board-score-for-rank [bingo-numbers boards rank]
  (let [num-set (set bingo-numbers)
        num-map (into {} (map vector bingo-numbers (range 1 (+ 1 (count bingo-numbers)))))
        score-row (fn [row] (if (empty? (difference (set row) num-set)) (apply max (map #(get num-map %) row)) 0))
        scored-boards (map (fn [board] (map (fn [row] [row (score-row row)]) board)) boards)
        matching-board (nth (sort-by (fn [board] (first (sort (filter #(> % 0) (map last board))))) scored-boards) (- rank 1))
        winning-row (first (sort-by last matching-board))
        winning-number (nth bingo-numbers (- (last winning-row) 1))
        unmarked-numbers (difference (set (flatten (map first matching-board))) (set (take (get num-map winning-number) bingo-numbers)))]
    (* (reduce + unmarked-numbers) winning-number)))

;; part one
(println (apply calculate-board-score-for-rank (conj (parse-bingo-numbers-and-boards) 1)))
;; part two
(println (apply calculate-board-score-for-rank (conj (parse-bingo-numbers-and-boards) 100)))
