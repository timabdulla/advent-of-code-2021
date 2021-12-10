(require '[clojure.string :refer [split trim split-lines join]]
         '[clojure.set :as set :refer [difference intersection union subset?]])

(defn parse-input []
  (->> (slurp "day_8.txt")
       (split-lines)
       (map (fn [line] (map (fn [patterns] (map trim (filter (complement empty?) patterns))) (map (fn [seg] (split seg #" ")) (split line #"\|")))))
       (map (fn [parts] (map vec parts)))
       (map vec)))

(defn part-one [input]
  (->> input
       (map second)
       (flatten)
       (map count)
       (filter #{2 3 4 7})
       (count)))

(defn raw-signal-to-set [raw suffix]
  (into #{} (map #(str % suffix) (split raw #""))))

(def digit-defs
  (into (sorted-map)
    (map-indexed
      (fn [idx s] [idx (raw-signal-to-set s "'")])
      ["abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdfeg" "acf" "abcdefg" "abcdfg"])))

(defn simplify-mappers [mappers]
  (let [simplified (reduce
    (fn [simplified [f t]]
      (let [to-simplify (filter (fn [[f2 t2]] (and (not (= f2 f)) (subset? f2 f))) mappers)]
      (conj (concat simplified (map (fn [[f2 t2]] [(difference f f2) (difference t t2)]) to-simplify)) [f t])))
    [] mappers)] simplified))

(defn apply-mappers [mappers signal]
  (into #{} (reduce (fn [s [from to]]
                  (if (subset? from (set s)) (replace (apply hash-map (interleave from to)) s) s)) 
                signal mappers)))

(defn unfuzz-signal [[original corrected uncorrected known-digit] mappers]
  (let [mapped (apply-mappers mappers uncorrected)
        newly-corrected (difference (difference mapped uncorrected) corrected)
        new-corrected (union corrected newly-corrected)
        new-uncorrected (intersection uncorrected mapped)
        combined-signal (union new-corrected new-uncorrected)]
        (if (nil? known-digit)
          (let [digit-intersections (map (fn [[d s]] [d (count (intersection s combined-signal))]) (filter (fn [[d s]] (= (count s) (count combined-signal))) digit-defs))
                max-intersection (last (sort-by second digit-intersections))
                maybe-digit (if (= 1 (get (frequencies (map second digit-intersections)) (second max-intersection))) (first max-intersection))]
            [[original new-corrected new-uncorrected maybe-digit] mappers])
          (let [known-signal (get digit-defs known-digit)
                known-without-corrected (difference known-signal new-corrected)
                new-mappers (if (= (count known-without-corrected) (count new-uncorrected)) (conj mappers [new-uncorrected known-without-corrected]) mappers)]
            [[original (union known-signal new-corrected) #{} known-digit] (simplify-mappers new-mappers)]))))

(defn unfuzz-signals [signal-structs mappers]
  (if (every? some? (map #(nth % 3) signal-structs))
    [signal-structs mappers]
    (let [to-unfuzz (sort-by (fn [[o c u k]] [(not k) (count o)]) (filter #(not-empty (nth % 2)) signal-structs))
          unfuzzed (reduce (fn [[structs mappers] signal-struct]
                             (let [[ret-struct ret-mappers] (unfuzz-signal signal-struct mappers)]
                               [(conj structs ret-struct) ret-mappers])) [[] mappers] to-unfuzz)]
      (recur (concat (filter #(empty? (nth % 2)) signal-structs) (first unfuzzed)) (second unfuzzed)))))

(defn part-two [[signals digits]]
  (let [make-signal-struct (fn [s known-digit] [s #{} s known-digit])
        signals (map #(raw-signal-to-set % "") signals)
        primers (map (fn [[d s]] (make-signal-struct s d)) (partition 2 (interleave [1 7 4 8] (sort-by count (filter #(#{2 3 4 7} (count %)) signals)))))
        other-signals (map #(make-signal-struct % nil) (filter #((complement #{2 3 4 7}) (count %)) signals))
        [final-signal-structs final-mappers] (unfuzz-signals (concat primers other-signals) [])
        unfuzzed-numbers (into {} (map (fn [[o c u k]] [(join (sort (vec o))) k]) final-signal-structs))]
    (Integer/parseInt (join (map (fn [d] (get unfuzzed-numbers d)) (map #(join (vec (sort (vec %)))) digits))))))

(println (reduce + (map part-two (parse-input))))
