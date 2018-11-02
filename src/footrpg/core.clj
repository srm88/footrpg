(ns footrpg.core
  (:require [footrpg.ascii :as ascii]
            [footrpg.util :refer [debug-log]])
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
  [(-> (:width p) (quot 2))
   (-> (:height p) (quot 2))])

(defn pitch-top-right [p]
  [(dec (:width p)) 0])

(defn pitch-bottom-right [p]
  [(dec (:width p))
   (dec (:height p))])

(defn pitch-top-left [p]
  [0 0])

(defn pitch-bottom-left [p]
  [0 (dec (:height p))])

(defn make-ball []
  {:kind :ball
   :vect nil
   :tile []})

(defn make-player []
  {:kind :player
   :team nil
   :number nil
   :name nil
   :quick 3
   :vect nil
   :tile []})

(defn add-vect [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn abs [x]
  (if (< x 0) (* x -1) x))

(defn magnitude [[x y]]
  (+ (abs x) (abs y)))

(defn levenshtein [[x y] [x* y*]]
  (+ (abs (- x x*)) (abs (- y y*))))

(defn move [thing vect]
  (-> thing
      (update :vect vect)
      (update :tile add-vect vect)))

;; TODO: 0 ending momentum if you move less than max
(defn can-reach? [player tile]
  (>= (:quick player) (levenshtein (:tile player) tile)))

(defn at-tile [s tile]
  (->> s :game :players (filter #(= tile (:tile %))) first))

(def up [0 -1])
(def down [0 1])
(def left [-1 0])
(def right [1 0])

(defn in-bounds? [[x y] pitch]
  (and (>= x 0) (< x (dec (:width pitch)))
       (>= y 0) (< y (dec (:height pitch)))))

(defn bounded [[x y] pitch]
  [(-> x (max 0) (min (dec (:width pitch))))
   (-> y (max 0) (min (dec (:height pitch))))])


(defn adjacent [tile pitch]
  (->> [up down left right]
       (map #(add-vect tile %))
       (filter #(in-bounds? % pitch))))

(defn player-range
  ([player pitch] (player-range player pitch #{} (list (:tile player))))
  ([player pitch seen tiles]
    (if (empty? tiles)
      (vec seen)
      (let [tile (first tiles)
            tiles* (pop tiles)]
        (if (or (contains? seen tile) (not (in-bounds? tile pitch)) (not (can-reach? player tile)))
          (recur player pitch seen tiles*)
          (recur player pitch (conj seen tile)
                 (conj tiles* (add-vect tile up)
                              (add-vect tile down)
                              (add-vect tile left)
                              (add-vect tile right))))))))

;; This is now meta-game

(defn pitch-cursor-left [s]
  (-> s
    (update-in [:cursor 0] dec)
    (update :cursor bounded (:pitch s))))

(defn pitch-cursor-right [s]
  (-> s
    (update-in [:cursor 0] inc)
    (update :cursor bounded (:pitch s))))

(defn pitch-cursor-up [s]
  (-> s
    (update-in [:cursor 1] dec)
    (update :cursor bounded (:pitch s))))

(defn pitch-cursor-down [s]
  (-> s
    (update-in [:cursor 1] inc)
    (update :cursor bounded (:pitch s))))

;; Only returns players for now

(defn pitch-select [s]
  (if-let [player (at-tile s (:cursor s))]
    {:into-mode [:player-select player s]}
    s))

(def player-select-mode-handlers {:left pitch-cursor-left
                                  :right pitch-cursor-right
                                  :up pitch-cursor-up
                                  :down pitch-cursor-down
                                  :escape (constantly :exit)
                                  \q (constantly :exit)})

(defn player-select-mode [player s]
  {:name :player-select
   :handlers player-select-mode-handlers
   :player player
   :selected (player-range player (:pitch s))})

(def pitch-mode-handlers {:left pitch-cursor-left
                          :right pitch-cursor-right
                          :up pitch-cursor-up
                          :down pitch-cursor-down
                          :enter pitch-select
                          :escape (constantly :hard-exit)
                          \q (constantly :hard-exit)})

(defn pitch-mode []
  {:name :pitch
   :handlers pitch-mode-handlers})

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

(defn home-tile [[x y] pitch]
  [(-> x (* (quot (:width pitch) 2)) int)
   (-> y (* (:height pitch)) int)])

(defn away-tile [[x y] pitch]
  [(- (dec (:width pitch)) (-> x (* (quot (:width pitch) 2)) int))
   (- (dec (:height pitch)) (-> y (* (:height pitch)) int))])

(defn init-game [pitch]
  (let [home (fn [number name* position & args] (apply assoc (make-player) :team :home
                                                              :number number
                                                              :name name*
                                                              :tile (-> formations :433 position (home-tile pitch)) args))
        away (fn [number name* position & args] (apply assoc (make-player) :team :away
                                                              :number number
                                                              :name name*
                                                              :tile (-> formations :433 position (away-tile pitch)) args))]
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
                (home 10 "Modric" :right-mid :quick 4)
                (home 22 "Isco" :left-wing :quick 4)
                (home 9 "Benzema" :striker)
                (home 11 "Bale" :right-wing :quick 4)
                (away 1 "Ter Stegen" :gk)
                (away 20 "Roberto" :right-back)
                (away 3 "Pique" :right-cb)
                (away 15 "Lenglet" :left-cb)
                (away 18 "Alba" :left-back :quick 4)
                (away 4 "Rakitic" :right-mid)
                (away 5 "Busquets" :center-mid)
                (away 8 "Arthur" :left-mid )
                (away 12 "Rafinha" :right-wing)
                (away 9 "Suarez" :striker)
                (away 7 "Coutinho" :left-wing :quick 4)]))))

(defn init []
  (let [pitch (make-pitch)]
    (def state (-> state
                   (assoc :pitch pitch)
                   (assoc :mode (list (pitch-mode)))
                   (assoc :game (init-game pitch))
                   (assoc :cursor (pitch-center pitch))))))

;; Dispatcher to avoid circular function dependencies
(def modes {:pitch pitch-mode
            :player-select player-select-mode})

(defn make-mode [mode & args]
  (apply (modes mode) args))

(defn main-loop []
  (loop [s state]
    (ascii/redraw s)
    (debug-log "mode stack: " (into [] (map :name (:mode s))))
    (let [handler ((-> s :mode peek :handlers) (ascii/input))
          reply (if (nil? handler) s (apply handler [s]))]
      (cond
        (= :hard-exit reply) nil
        (= :exit reply) (recur (update s :mode pop))
        (:into-mode reply) (->> (:into-mode reply) (apply make-mode) (update s :mode conj) recur)
        :else (recur reply)))))

(defn main [ui-type]
  (ascii/init)
  (try
    (init)
    (main-loop)
    (finally (ascii/stop))))

(defn -main [& args]
  (main :ascii))
