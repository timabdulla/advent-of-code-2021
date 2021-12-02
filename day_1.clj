(ns advent-of-code-2021.day-1)

(defn count-pairwise-increases [s]
  (->> s
   (partition 2 1)
   (filter #(> (second %) (first %)))
   (count)))

(defn count-summed-window-increases [window-size s]
  (let [summed-windows (map #(reduce + %) (partition window-size 1 s))]
    (count-pairwise-increases summed-windows)))
