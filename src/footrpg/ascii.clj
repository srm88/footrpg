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

(defmulti render (fn [s thing]
                   (cond
                     (contains? thing :tile-range) :range
                     :else (:render thing))))

(defmethod render :menu [s thing]
  (let [menu (:menu thing)
        width 25
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

(defmethod render :vector [s thing]
  (let [from (:from thing)
        to (:to thing)
        glyph (:glyph thing)
        color {:fg :white
               :bg :cyan}
        path (f/path* from to)]
    (reduce (fn [tile step]
              (let [next-tile (f/add-vect tile step)]
                (put-pitch (:pitch s) next-tile (vector-glyph step) color)
                next-tile))
            from
            path)
    (put-pitch (:pitch s) to glyph color)))

(defmethod render :range [s thing]
  (let [tiles (:tile-range thing)
        color (let [c (case (:name thing)
                        :player-move :cyan
                        :player-kick :magenta
                        :blue)]
                {:fg c :bg c})]
    (doseq [tile tiles]
      (put-pitch (:pitch s) tile "  " color))))

(defmethod render :player [s thing]
  (let [player (:player thing)
        color (get-in s [:game :teams (:team player) :color])
        glyph (player-glyph player)]
    (put-pitch (:pitch s) (:tile player) (str glyph) color)))

(defmethod render :default [s thing] nil)

(defn redraw [s]
  (doseq [[i line] (map-indexed vector ascii-pitch)]
    (s/put-string screen 0 i line))

  ;; XXX this hand-done ordering determines which things render on top --
  ;; should build z-index instead
  (render s (:mode s))

  ;; XXX multimethod for previewing actions
  (doseq [player-move (->> (f/action-taken (:mode s) :move) (map :move))]
    (render s {:render :vector
               :from (:from player-move)
               :to (:to player-move)
               :glyph "[]"
               :color {:bg :cyan :fg :white}}))

  (when-let [ball-move (:move (first (f/action-taken (:mode s) :kick)))]
    (render s {:render :vector
               :from (:from ball-move)
               :to (:to ball-move)
               :glyph ball-glyph
               :color {:bg :magenta :fg :white}}))

  (doseq [player (f/players s)]
    (render s {:render :player
               :player player}))

  (let [[curs-x curs-y] (transpose-tile (:cursor s) (:pitch s))]
    (s/move-cursor screen curs-x curs-y))

  (when (contains? (:mode s) :menu)
    (render s {:render :menu
               :menu (:mode s)}))

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
