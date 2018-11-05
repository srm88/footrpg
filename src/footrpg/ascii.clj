(ns footrpg.ascii
  (:require [lanterna.screen :as s]
            [clojure.string :as string]))

(def ascii-pitch (string/split-lines (slurp "resources/ascii-pitch.txt")))

(def screen)

;; Probably should bring in stuart sierra component shortly
(defn init []
  (def screen (s/get-screen :text))
  (s/start screen)
  ;; Hack recommended in a clojure-lanterna issue https://github.com/sjl/clojure-lanterna/issues/7
  (s/get-key screen)
  (s/redraw screen))

(defn transpose-tile [[x y] pitch]
  [(-> x (* 2) inc)
   (inc y)])

(defn player-glyph [player]
  (let [[num-first num-second] (str (:number player))]
    (str num-first (or num-second " "))))

(def ball-glyph "@")

(defn put-pitch [pitch tile & args]
  (let [[x y] (transpose-tile tile pitch)]
    (apply s/put-string screen x y args)))

(defn draw-player [state player]
  (let [color (get-in state [:game :teams (:team player) :color])
        glyph (player-glyph player)]
    (put-pitch (:pitch state) (:tile player) (str glyph) color)))

(defn redraw [state]
  (doseq [[i line] (map-indexed vector ascii-pitch)]
    (s/put-string screen 0 i line))
  (doseq [tile (-> state :mode :move-range)]
    (if (-> state :mode :move-to-tile (= tile))
      (put-pitch (:pitch state) tile "[]" {:bg :cyan :fg :white})
      (put-pitch (:pitch state) tile "  " {:bg :cyan :fg :cyan})))
  (doseq [player (-> state :game :players)]
    (draw-player state player))
  (let [[curs-x curs-y] (transpose-tile (:cursor state) (:pitch state))]
    (s/move-cursor screen curs-x curs-y))
  (put-pitch (:pitch state) (-> state :game :ball :tile ) ball-glyph)
  (s/put-string screen 0 (-> state :pitch :height inc inc) (str "> " (:status-line state)))
  (s/redraw screen))

(defn input []
  (s/get-key-blocking screen))

(defn stop []
  (s/stop screen))

;; API scratch
;; move cursor
;; select tile
