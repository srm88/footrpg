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

(defn put-pitch [pitch tile & args]
  (let [[x y] (transpose-tile tile pitch)]
    (apply s/put-string screen x y args)))

;; XXX consider drawing these glyphs on only one of the two columns for the
;; tile so that we can show two vectors at the same time
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

(defn momentum-glyph [v]
  (condp = v
    f/up "^ "
    f/down "v "
    f/left "< "
    f/right "> "
    f/up-left "< "
    f/up-right "> "
    f/down-left "< "
    f/down-right "> "))

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

(defmulti glyph (fn [s thing]
                  (cond
                    (= (:kind thing) :player) :player
                    (= (:kind thing) :ball) :ball)))

(defmethod glyph :player [s player]
  (let [[num-first num-second] (str (:number player))]
    (str num-first (or num-second " "))))

(defmethod glyph :ball [s ball]
  "@ ")

(defmulti color (fn [s thing]
                  (cond
                    (= (:kind thing) :player) :player
                    (= (:kind thing) :ball) :ball)))

(defmethod color :player [s player]
  (get-in s [:game :teams (:team player) :color]))

(defmethod color :ball [s ball]
  {:bg :magenta :fg :white})

(defn renderables [s]
  (flatten [(f/active-modes s)
            (map :actions (f/active-modes s))
            (-> s :game :entities vals)]))

;; Used for render and z-index
(defn render-kind [s thing]
  (cond
    (contains? thing :tile-range) :range
    (contains? thing :menu) :menu
    (= (:name thing) :momentums) :momentums
    (= (:kind thing) :player) :simple-entity
    (= (:kind thing) :ball) :simple-entity
    (and (= (:kind thing) :action)
         (= (:action thing) :move)) :move
    :else (:render thing)))

(defmulti z-index render-kind)
(defmethod z-index :range [_ _] 1)
(defmethod z-index :menu [_ _] 100)
(defmethod z-index :simple-entity [_ entity] (if (= (:kind entity) :ball) 10 5))
(defmethod z-index :move [_ _] 2)
(defmethod z-index :momentums [_ _] 3)
(defmethod z-index :default [_ _] 0)

(defmulti render render-kind)

;; XXX actions and modes should render themselves and contribute their sub-renderables
(defmethod render :menu [s mode]
  (let [width 25
        start-x 10
        start-y 5
        border? true
        lines (->> (:menu mode)
                   (map :text)
                   (map #(r-pad-to % width))
                   (map-indexed (fn [idx line]
                                  (str (if (= idx (:cursor mode)) ">" " ") line))))]
    (draw-border [start-x start-y] width (count lines))
    (doseq [[idx line] (map-indexed vector lines)]
      (s/put-string screen start-x (+ start-y idx) line))))

(defn draw-vect [s from to glyph-fn color]
  (reduce (fn [tile step]
            (let [next-tile (f/add-vect tile step)]
              (put-pitch (:pitch s) next-tile (glyph-fn step) color)
              next-tile))
          from
          (f/path* from to)))

(defmethod render :move [s action]
  (let [m (:move action)
        entity (get-in s [:game :entities (:entity m)])
        glyph (glyph s entity)
        color (color s entity)]
    (draw-vect s (:from m) (:to m) vector-glyph color)
    (put-pitch (:pitch s) (:to m) glyph color)))

(defmethod render :momentums [s mode]
  (doseq [entity (:entities mode)]
    (let [from (:tile entity)
          to (f/add-vect from (:vect entity))
          color (color s entity)]
      (draw-vect s from to momentum-glyph color))))

(defmethod render :range [s mode]
  (let [tiles (:tile-range mode)
        color (let [c (case (:name mode)
                        :player-move :cyan
                        :player-kick :magenta
                        :blue)]
                {:fg c :bg c})]
    (doseq [tile tiles]
      (put-pitch (:pitch s) tile "  " color))))

(defmethod render :simple-entity [s entity]
  (put-pitch (:pitch s) (:tile entity) (glyph s entity) (color s entity)))

(defmethod render :default [s thing] nil)

(defn redraw [s]
  (doseq [[i line] (map-indexed vector ascii-pitch)]
    (s/put-string screen 0 i line))

  (->> (renderables s) (sort-by #(z-index s %)) (map #(render s %)) doall)

  ;; XXX the cursor should become mode-local state and render as a part of
  ;; pertinent modes
  (let [[curs-x curs-y] (transpose-tile (:cursor s) (:pitch s))]
    (s/move-cursor screen curs-x curs-y))

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
