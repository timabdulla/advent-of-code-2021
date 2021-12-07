(require '[clojure.string :refer [split trim]])

(def generations (into [] (frequencies (map #(Integer/parseInt (trim %)) (split (slurp "day_6.txt") #",")))))

(defn generations-seq [init]
  (let [next-gen (reduce
                   (fn [g [timer c]]
                     (if (= timer 0)
                       (update (update g 8 (fnil + 0) c) 6 (fnil + 0) c)
                       (assoc g (- timer 1) c))) {}
                   (reverse (sort-by first init)))]
        (lazy-seq (cons next-gen (generations-seq next-gen)))))

(defn count-fish [days]
  (reduce + (map last (last (take days (generations-seq generations))))))

;; part 1
(println (count-fish 18))
;; part 2
(println (count-fish 256))
