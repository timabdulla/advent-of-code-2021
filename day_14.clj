(ns aoc.day-14
  (:require [clojure.string :refer [split split-lines join]]
            [clojure.set :refer [difference union]]))

(def file "day_14.txt")

(defn parse-input-file []
  (let [lines (split-lines (slurp file))
        template (first lines)
        rules (map #(split % #" -> " ) (nthrest lines 2))]
    [template rules]))

(defn pairs-from-template [template l r]
  (let [pairs-without-context (vec (map join (partition 2 1 template)))]
    (map-indexed (fn [idx p]
                   (let [p-l (or (get pairs-without-context (dec idx)) l)
                         p-r (or (get pairs-without-context (inc idx)) r)]
                     [p-l p p-r])) pairs-without-context)))

(defn apply-rules-to-uncontextualized-pair [pair rules]
  (let [[_rp i] (first (filter (fn [[rp _i]] (= rp pair)) rules))
        mapped (join "" (interpose (or i "") (split pair #"")))]
    mapped))

(defn apply-rules-to-pair [[l p r] rules]
  (let [l (and l (subs (apply-rules-to-uncontextualized-pair l rules) 0 2))
        r (and r (subs (apply-rules-to-uncontextualized-pair r rules) 0 2))]
    (pairs-from-template (apply-rules-to-uncontextualized-pair p rules) l r)))

(defn process-grouped-pairs [grouped-pairs rules times]
  (if (zero? times)
    grouped-pairs
    (let [updated-grouped-pairs (reduce (fn [g [pair cnt]]
                                          (let [new-pairs (apply-rules-to-pair pair rules)
                                                g (update g pair (fn [old cnt] (if old (- old cnt) 0)) cnt)]
                                            (reduce (fn [g p] (update g p (fnil + 0) cnt)) g new-pairs)))
                                        grouped-pairs grouped-pairs)]
      (recur updated-grouped-pairs rules (dec times)))))

(defn letter-count-from-grouped-pairs [grouped-pairs]
  (reduce (fn [cs [[l p r] c]]
            (let [updates (if (nil? l) (partition 2 (interleave (split p #"") (repeat 2 c))) [[(str (last p)) c]])]
              (reduce (fn [cs [p c]] (update cs p (fnil + 0) c)) cs updates))) {} grouped-pairs))

(defn answer-puzzle [times]
  (let [[template rules] (parse-input-file)
        processed (process-grouped-pairs (frequencies (pairs-from-template template nil nil)) rules times)
        freqs (sort (vals (letter-count-from-grouped-pairs processed)))]
    (- (last freqs) (first freqs))))

(println "part one: " (answer-puzzle 10))
(println "part two " (answer-puzzle 40))
