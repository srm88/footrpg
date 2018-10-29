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

(defn transpose-loc [[x y] pitch]
  [(-> x (* 2) inc)
   (inc y)])

(defn player-glyph [player]
  (let [[num-first num-second] (str (:number player))]
    (str num-first (or num-second " "))))

(defn redraw [state]
  (doseq [[i line] (map-indexed vector ascii-pitch)]
    (s/put-string screen 0 i line))
  (doseq [player (-> state :game :players)]
    (let [color (get-in state [:game :teams (:team player) :color])
          glyph (player-glyph player)
          [x y] (transpose-loc (-> player :loc :loc) (:pitch state))]
      (s/put-string screen x y (str glyph) color)))
  (let [[curs-x curs-y] (transpose-loc (:cursor state) (:pitch state))]
    (s/move-cursor screen curs-x curs-y))
  (s/redraw screen))

(defn input []
  (s/get-key-blocking screen))

(defn stop []
  (s/stop screen))

;; API scratch
;; move cursor
;; select tile
