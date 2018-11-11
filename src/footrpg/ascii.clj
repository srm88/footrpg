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

(defn vector-glyph [v]
  (condp = v
    f/up "| "
    f/down "| "
    f/left "--"
    f/right "--"
    f/up-left "'."
    f/up-right ".'"
    f/down-left ".'"
    f/down-right "'."))

(defn draw-vector [s from to color]
  (let [path (f/path* from to)]
    (reduce (fn [tile step]
              (let [next-tile (f/add-vect tile step)]
                (put-pitch (:pitch s) next-tile (vector-glyph step) color)
                next-tile))
            from
            path)))

(defn draw-border [[x y] width height]
  (s/put-string screen (dec x) (dec y) (str "." (apply str (repeat width "~")) "."))
  (doseq [y* (range height)]
    (s/put-string screen (dec x) (+ y y*) "|")
    (s/put-string screen (+ x width) (+ y y*) "|"))
  (s/put-string screen (dec x) (+ y height) (str "'" (apply str (repeat width "~")) "'")))

(defn r-pad-to [s length]
  ;; New string will be padded on the right with spaces until it has length `length`
  (let [trimmed (f/subs* s length)
        padding (apply str (-> (dec length)
                               (- (count trimmed))
                               (repeat " ")))]
    (str trimmed padding)))

(defn draw-menu [s menu]
  (let [width 25
        start-x 10
        start-y 5
        border? true
        lines (->> (:menu menu)
                   (map :text)
                   (map #(r-pad-to % width))
                   (map-indexed (fn [idx line]
                                  (str (if (= idx (:cursor menu)) ">" " ") line))))]
    (draw-border [start-x start-y] width (count lines))
    (doseq [[idx line] (map-indexed vector lines)]
      (s/put-string screen start-x (+ start-y idx) line))))

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
  (when (= (-> s :mode :name) :player-kick)
    (doseq [tile (-> s :mode :tile-range)]
      (put-pitch (:pitch s) tile "  " {:bg :magenta :fg :magenta})))
  ;; XXX need a multimethod approach here to render each action
  (when-let [player-move (:move (first (f/action-taken (:mode s) :move)))]
    (draw-vector s (:from player-move) (:to player-move) {:bg :cyan :fg :white})
    (put-pitch (:pitch s) (:to player-move) "[]" {:bg :cyan :fg :white}))
  (when-let [ball-move (:move (first (f/action-taken (:mode s) :kick)))]
    (draw-vector s (:from ball-move) (:to ball-move) {:bg :magenta :fg :white})
    (put-pitch (:pitch s) (:to ball-move) ball-glyph {:bg :magenta :fg :white}))
  (doseq [player (f/players s)]
    (draw-player s player))
  (let [[curs-x curs-y] (transpose-tile (:cursor s) (:pitch s))]
    (s/move-cursor screen curs-x curs-y))
  (when (contains? (:mode s) :menu)
    (draw-menu s (:mode s)))
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
