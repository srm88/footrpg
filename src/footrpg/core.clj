(ns footrpg.core
  (:require [footrpg.ascii :as ascii])
  (:gen-class))

(defn make-game []
  {:players []
   :teams {:home nil :away nil}
   :ball nil
   :score {:home 0
           :away 0}})

(defn make-team []
  {:lineup []
   :name ""})

(defn make-pitch []
  {:width 58
   :height 37})

(defn pitch-center [p]
  [(-> (:width p) (/ 2))
   (-> (:height p) (/ 2))])

(defn pitch-top-right [p]
  [(dec (:width p)) 0])

(defn pitch-bottom-right [p]
  [(dec (:width p))
   (dec (:height p))])

(defn pitch-top-left [p]
  [0 0])

(defn pitch-bottom-left [p]
  [0 (dec (:height p))])

(defn make-location []
  {:vect nil
   :loc []})

(defn make-ball []
  {:kind :ball
   :loc (make-location)})

(defn make-player []
  {:kind :player
   :team nil
   :number nil
   :name nil
   :loc (make-location)}
  )

(defn add-vect [location [dx dy]]
  (-> location
      (assoc :vect [dx dy])
      (update-in [:loc 0] + dx)
      (update-in [:loc 1] + dy)))

(defn move [thing vect]
  (update thing :loc add-vect vect))

;; This is now meta-game

(defn bounded [[x y] pitch]
  [(-> x (max 0) (min (dec (:width pitch))))
   (-> y (max 0) (min (dec (:height pitch))))])

(defn cursor-left [s]
  (-> s
    (update-in [:cursor 0] dec)
    (update :cursor bounded (:pitch s))))

(defn cursor-right [s]
  (-> s
    (update-in [:cursor 0] inc)
    (update :cursor bounded (:pitch s))))

(defn cursor-up [s]
  (-> s
    (update-in [:cursor 1] dec)
    (update :cursor bounded (:pitch s))))

(defn cursor-down [s]
  (-> s
    (update-in [:cursor 1] inc)
    (update :cursor bounded (:pitch s))))

(def main-mode {:left cursor-left
                :right cursor-right
                :up cursor-up
                :down cursor-down
                :escape (constantly :exit)
                \q (constantly :exit)})

(defn make-state []
  {:cursor [0 0]})

(def state (make-state))

;; Location utils
(def formations
     {:433 {:gk [0. 0.5]
            :left-back [0.34 0.15]
            :left-cb [0.34 0.38]
            :right-cb [0.34 0.62]
            :right-back [0.34 0.85]
            :left-mid [0.57 0.25]
            :center-mid [0.57 0.5]
            :right-mid [0.57 0.75]
            :left-wing [0.8 0.25]
            :striker [0.8 0.5]
            :right-wing [0.8 0.75]}})

(defn home-loc [[x y] pitch]
  [(-> x (* (/ (:width pitch) 2)) int)
   (-> y (* (:height pitch)) int)])

(defn away-loc [[x y] pitch]
  [(- (dec (:width pitch)) (-> x (* (/ (:width pitch) 2)) int))
   (- (dec (:height pitch)) (-> y (* (:height pitch)) int))])

(defn init-game [pitch]
  (let [home (fn [number name* position] (assoc (make-player) :team :home :number number :name name* :loc {:vert nil :loc (-> formations :433 position (home-loc pitch))}))
        away (fn [number name* position] (assoc (make-player) :team :away :number number :name name* :loc {:vert nil :loc (-> formations :433 position (away-loc pitch))}))]
    (-> (make-game)
        (assoc :teams {:home {:name :real-madrid :color {:bg :white :fg :black}}
                       :away {:name :barcelona :color {:bg :red :fg :white}}})
        (assoc :players
               [(home 25 "Courtois" :gk)
                (home 12 "Marcelo" :left-back)
                (home 4 "Ramos" :left-cb)
                (home 5 "Varane" :right-cb)
                (home 6 "Nacho" :right-back) ;; lol
                (home 8 "Kroos" :left-mid)
                (home 14 "Casemiro" :center-mid)
                (home 10 "Modric" :right-mid)
                (home 22 "Isco" :left-wing)
                (home 9 "Benzema" :striker)
                (home 11 "Bale" :right-wing)
                (away 1 "Ter Stegen" :gk)
                (away 20 "Roberto" :right-back)
                (away 3 "Pique" :right-cb)
                (away 15 "Lenglet" :left-cb)
                (away 18 "Alba" :left-back)
                (away 4 "Rakitic" :right-mid)
                (away 5 "Busquets" :center-mid)
                (away 8 "Arthur" :left-mid)
                (away 12 "Rafinha" :right-wing)
                (away 9 "Suarez" :striker)
                (away 7 "Coutinho" :left-wing)]))))

(defn init []
  (let [pitch (make-pitch)]
    (def state (-> state
                   (assoc :pitch pitch)
                   (assoc :mode main-mode)
                   (assoc :game (init-game pitch))
                   (assoc :cursor (pitch-center pitch))))))

(defn main-loop []
  (loop [s state]
    (ascii/redraw s)
    (let [c (ascii/input)
          f ((:mode s) c)
          s2 (if (nil? f) s (apply f [s]))]
      (if (= :exit s2)
        nil
        (recur s2)))))

(defn main [ui-type]
  (ascii/init)
  (try
    (init)
    (main-loop)
    (finally (ascii/stop))))

(defn -main [& args]
  (main :ascii))
