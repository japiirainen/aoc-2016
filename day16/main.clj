#!/usr/bin/env bb

(require '[clojure.string :as str])

(defn bin->bool [b]
  (for [c b] (if (= c \1) true false)))

(defn bool->bin [b]
  (str/join (for [c b] (if c \1 \0))))

(defn expand [n seed]
  (loop [m n s seed]
    (let [ne (concat s [false] (->> (reverse s) (map not)))]
      (if (>= (count ne) m) (take m ne) (recur m ne)))))

(defn checksum [v]
  (loop [cs v]
    (let [ne (for [[a b] (partition 2 cs)] (= a b))]
      (if (not= 0 (mod (count ne) 2)) ne (recur ne)))))

(defn solve [cfg v]
  (->> (expand cfg v) checksum bool->bin))

(defn solve-file [fp]
  (println (str "Solving for file for file : " fp))
  (doseq [[part n] [[1 272] [2 35651584]]]
    (println (str "Part " part " : " (->> (slurp fp) str/trim bin->bool (solve n))))))

(doseq [fp *command-line-args*]
  (solve-file fp))
