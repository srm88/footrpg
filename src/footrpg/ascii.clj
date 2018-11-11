(ns footrpg.ascii
  (:require [footrpg.core :as f]
            [footrpg.util :refer [debug-log]]
            [lanterna.screen :as s]
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

(defn draw-player [s player]
  (let [color (get-in s [:game :teams (:team player) :color])
        glyph (player-glyph player)]
    (put-pitch (:pitch s) (:tile player) (str glyph) color)))

(defn redraw [s]
  (doseq [[i line] (map-indexed vector ascii-pitch)]
    (s/put-string screen 0 i line))
  (when (= (-> s :mode :name) :player-move)
    (doseq [tile (-> s :mode :tile-range)]
      (put-pitch (:pitch s) tile "  " {:bg :cyan :fg :cyan})))
  ;; XXX: show kick-to-tile, move-to-tile
  (when (= (-> s :mode :name) :player-kick)
    (doseq [tile (-> s :mode :tile-range)]
      (put-pitch (:pitch s) tile "  " {:bg :magenta :fg :magenta})))
  (when-let [move-to (get-in s [:mode :move-to-tile])]
    (put-pitch (:pitch s) move-to "[]" {:bg :cyan :fg :white}))
  (when-let [kick-to (get-in s [:mode :kick-to-tile])]
    (put-pitch (:pitch s) kick-to ball-glyph {:bg :magenta :fg :white}))
  (doseq [player (f/players s)]
    (draw-player s player))
  (let [[curs-x curs-y] (transpose-tile (:cursor s) (:pitch s))]
    (s/move-cursor screen curs-x curs-y))
  (put-pitch (:pitch s) (-> s :game :entities :ball :tile ) ball-glyph)
  (s/put-string screen 0 (-> s :pitch :height inc inc) (apply str (repeat 80 " ")))
  (s/put-string screen 0 (-> s :pitch :height inc inc) (str "> " (:status-line s)))
  (s/redraw screen))

(defn input []
  (s/get-key-blocking screen))

(defn stop []
  (s/stop screen))

;; API scratch
;; move cursor
;; select tile
