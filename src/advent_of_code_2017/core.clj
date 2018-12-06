(ns advent-of-code-2017.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def direction->coordinate-change
  {"n" [0 +1 -1],
   "nw" [-1 +1 0],
   "ne" [+1 0 -1],
   "sw" [-1 0 +1],
   "se" [+1 -1 0],
   "s" [0 -1 +1]})

(def day11-input (slurp (io/resource "day11-input")))

(defn abs [n] (if (pos? n) n (- n)))

(defn day11-part1
  [day11-input]
  (->> (str/split (str/trim day11-input) #",")
       (reduce (fn [current movement]
                 (let [[x y z] current
                       [dx dy dz] (direction->coordinate-change movement)]
                   [(+ x dx) (+ y dy) (+ z dz)]))
         [0 0 0])
       (map abs)
       (apply max)))

(defn day11-part2
  [day11-input]
  (->> (str/split (str/trim day11-input) #",")
       (reductions (fn [current movement]
                     (let [[x y z] current
                           [dx dy dz] (direction->coordinate-change movement)]
                       [(+ x dx) (+ y dy) (+ z dz)]))
                   [0 0 0])
       (map #(->> %
                  (map abs)
                  (reduce max)))
       (reduce max)))

(def day12-input (str/split-lines (slurp (io/resource "day12-input"))))

(defn parse-group
  [group]
  (-> group
      (str/replace " <->" ",")
      (str/split #", ")
      (->> (map #(Integer/parseInt %))
           set)))

(defn day12-part1 [day12-input] (map parse-group day12-input))
