(ns advent-of-code-2021.day-3
  (:require [clojure.string :as str :refer [join split-lines]]))

(def file "day_3.txt")

(defn create-bit-registers [register-count]
  (map (fn [_count] [0 0]) (range register-count)))

(defn update-bit-register [bit [zero-count one-count]]
  (if (= bit 0) [(+ zero-count 1) one-count] [zero-count (+ one-count 1)]))

(defn update-bit-registers [binary-seq bit-registers]
  (map #(update-bit-register %1 %2) binary-seq bit-registers))

(defn populate-bit-registers [binary-seqs]
  (let [bit-registers (create-bit-registers (count (first binary-seqs)))]
    (reduce #(update-bit-registers %2 %1) bit-registers binary-seqs)))

(defn most-common-bits [bit-registers]
  (map (fn [[zero-count one-count]] (if (> zero-count one-count) 0 1)) bit-registers))

(defn least-common-bits [bit-registers]
  (map (fn [[zero-count one-count]] (if (< one-count zero-count) 1 0)) bit-registers))

(defn binary-seq-to-integer [binary-seq]
  (Integer/parseInt (join binary-seq) 2))

(defn filter-binary-seqs-by-bit-val [binary-seqs pos bit]
  (filter #(= (nth % pos) bit) binary-seqs))

(defn criteria-filtered-binary-seqs
  ([bit-criteria binary-seqs] (criteria-filtered-binary-seqs bit-criteria binary-seqs 0))
  ([bit-criteria binary-seqs pos]
   (if (>= pos (count (first binary-seqs)))
     nil
     (let [bit-registers (populate-bit-registers binary-seqs)
           criteria-bits (bit-criteria bit-registers)
           filtered-seqs (filter-binary-seqs-by-bit-val binary-seqs pos (nth criteria-bits pos))]
       (lazy-seq (cons filtered-seqs (criteria-filtered-binary-seqs bit-criteria filtered-seqs (inc pos))))))))

(defn calculate-rating [binary-seqs bit-criteria]
  (binary-seq-to-integer (first (first (filter #(= (count %) 1) (criteria-filtered-binary-seqs bit-criteria binary-seqs))))))

(defn oxygen-generator-rating [binary-seqs]
  (calculate-rating binary-seqs most-common-bits))

(defn co2-scrubber-rating [binary-seqs]
  (calculate-rating binary-seqs least-common-bits))

(defn gamma-rate [bit-registers]
  (binary-seq-to-integer (most-common-bits bit-registers)))

(defn epsilon-rate [bit-registers]
  (binary-seq-to-integer (least-common-bits bit-registers)))

(defn parse-binary-numbers [file]
  (let [lines (split-lines (slurp file))]
    (map (fn [line] (map #(Character/digit % 10) (seq line))) lines)))

(defn first-part []
  (let [binary-seqs (parse-binary-numbers file)
        bit-registers (populate-bit-registers binary-seqs)]
    (* (gamma-rate bit-registers) (epsilon-rate bit-registers))))

(defn second-part []
  (let [binary-seqs (parse-binary-numbers file)]
    (* (oxygen-generator-rating binary-seqs) (co2-scrubber-rating binary-seqs))))
