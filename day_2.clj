(ns advent-of-code-2021.day-2
  (:require [clojure.string :as str :refer [split split-lines]]))

(def input-file "day-2.txt")

(defn change-position [[change-type delta] [x y aim]]
  (condp = change-type
    :forward [(+ x delta) (+ y (* aim delta)) aim]
    :down [x y (+ aim delta)]
    :up [x y (- aim delta)]))

(defn multi-change-position [changes position]
  (reduce #(change-position %2 %1) position changes))

(defn parse-changes [file]
  (let [lines (split-lines (slurp file))
        line-tokens (map #(split % #" ") lines)]
    (map (fn [[change-type delta]] [(keyword change-type) (Integer/parseInt delta)]) line-tokens)))

(defn -main []
  (let [initial-position [0 0 0]
        changes (parse-changes input-file)
        [final-x final-y final-aim] (multi-change-position changes initial-position)]
    (print (* final-x final-y))))

(-main)
