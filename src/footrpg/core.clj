(ns footrpg.core
  (:require [footrpg.ascii :as ascii]
            [footrpg.util :refer [debug-log]]
            [clojure.core.matrix.linear :as linear]
            [clojure.core.matrix :as matrix])
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
   :id (gensym "player")
   :team nil
   :number nil
   :name nil
   :quick 3
   :vect nil
   :tile []})

(defn add-vect [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn subtract-vect [[x y] [dx dy]]
  [(- x dx) (- y dy)])

(defn magnitude [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn levenshtein [[x y] [x* y*]]
  (+ (Math/abs (- x x*)) (Math/abs (- y y*))))

(defn move [thing tile]
  (debug-log "move " thing " to " tile)
  (-> thing
      (assoc :vect (subtract-vect tile (:tile thing)))
      (assoc :tile tile)))

(defn floor [x y]
  [(int x) (int y)])

(defn cosine-similarity [a b]
  (if (or (= [0 0] a) (= [0 0] b))
    0
    (/ (matrix/dot a b)
       (* (linear/norm a) (linear/norm b)))))

;; A tile here is roughly 2yd or 1.8m
;; Fastest players can hit speeds of 35km/h, which is 9.7 m/s
;; So let's say top pace is 10 yd/s
;; Maybe a turn is 2s
;; So Mbappe could go 20 yards in a turn, which is 10 tiles
;; TODO: 0 ending momentum if you move less than max
(defn new-can-reach? [player tile]
  (let [start-v (or (:vect player) [0 0])
        end-v (subtract-vect tile (:tile player))
        dv (subtract-vect end-v start-v)
        cos-sim (cosine-similarity start-v dv)
        ;; decelerate-discount will be negative; it decreases the magnitude of the
        ;; change in momentum
        decelerate-discount (if (< cos-sim 0)
                              (-> cos-sim (/ 3) (* (magnitude start-v)))
                              0)]
    (<= (+ (magnitude dv) decelerate-discount) (:quick player))))

(def can-reach? new-can-reach?)

(defn can-kick? [player ball tile]
  (let [player-to-ball (subtract-vect (:tile ball) (:tile player))
        ball-to-tile (subtract-vect tile (:tile ball))]
    (< 0.5 (cosine-similarity player-to-ball ball-to-tile))))

(defn player-key [s player]
  (first (keep-indexed (fn [id p]
                         (if (= (:id player) (:id p)) id nil))
                       (-> s :game :players))))

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

(defn player-kick-range
  [player ball pitch]
  (set (for [x (range (:width pitch))
             y (range (:height pitch))
             :when (can-kick? player ball [x y])]
         [x y])))

; Returns the set of tiles the player can reach
(defn player-range
  ([player pitch] (player-range player pitch #{} (list (add-vect (:tile player) (or (:vect player) [0 0])))))
  ([player pitch seen tiles]
    (if (empty? tiles)
      seen
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
  {:new-state (-> s
                  (update-in [:cursor 0] dec)
                  (update :cursor bounded (:pitch s)))})

(defn pitch-cursor-right [s]
  {:new-state (-> s
                  (update-in [:cursor 0] inc)
                  (update :cursor bounded (:pitch s)))})

(defn pitch-cursor-up [s]
  {:new-state (-> s
                  (update-in [:cursor 1] dec)
                  (update :cursor bounded (:pitch s)))})

(defn pitch-cursor-down [s]
  {:new-state (-> s
                  (update-in [:cursor 1] inc)
                  (update :cursor bounded (:pitch s)))})

;; Only returns players for now

(defn pitch-select [s]
  (if-let [player (at-tile s (:cursor s))]
    {:mode-into [:player-select player s]}
    nil))

(defn to-player-kick [s]
  (let [ball-tile (get-in s [:game :ball :tile])
        player-tile (get-in s [:mode :player :tile])]
    (when (-> ball-tile (subtract-vect player-tile) magnitude (< 2))
      {:mode-into [:player-kick (-> s :mode :player) s]})))

(defn player-move-select [s]
  (let [tile (:cursor s)]
    (when (contains? (-> s :mode :move-range) tile)
      {:new-state (assoc-in s [:mode :move-to-tile] tile)})))

(defn player-kick-select [s]
  (let [tile (:cursor s)]
    (debug-log "kick to " tile " , contains? " (contains? (-> s :mode :kick-range) tile))
    (if (contains? (-> s :mode :kick-range) tile)
      {:new-state (update-in s [:game :ball] move tile)
       :mode-done true})))

(defn player-turn-commit [s]
  (when-let [tile (-> s :mode :move-to-tile)]
    {:new-state (update-in s [:game :players (player-key s (-> s :mode :player))] move tile)
     :mode-done true}))

(def player-kick-mode-handlers {:left pitch-cursor-left
                                :right pitch-cursor-right
                                :up pitch-cursor-up
                                :down pitch-cursor-down
                                :enter player-kick-select
                                :escape (constantly {:mode-done true})
                                \q (constantly {:mode-done true})})

(defn player-kick-mode [player s]
  {:name :player-kick
   :handlers player-kick-mode-handlers
   :player player
   :kick-to-tile nil
   :kick-range (player-kick-range player (get-in s [:game :ball]) (:pitch s))})

(def player-select-mode-handlers {:left pitch-cursor-left
                                  :right pitch-cursor-right
                                  :up pitch-cursor-up
                                  :down pitch-cursor-down
                                  \m player-move-select
                                  \k to-player-kick
                                  :enter player-turn-commit
                                  :escape (constantly {:mode-done true})
                                  \q (constantly {:mode-done true})})

(defn player-select-mode [player s]
  {:name :player-select
   :handlers player-select-mode-handlers
   :player player
   :move-to-tile nil
   :move-range (player-range player (:pitch s))})

(def pitch-mode-handlers {:left pitch-cursor-left
                          :right pitch-cursor-right
                          :up pitch-cursor-up
                          :down pitch-cursor-down
                          :enter pitch-select
                          :escape (constantly :game-done)
                          \q (constantly :game-done)})

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
        (assoc :ball (assoc (make-ball) :tile (pitch-center pitch)))
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
                   (assoc :mode (pitch-mode))
                   (assoc :modes (list))
                   (assoc :game (init-game pitch))
                   (assoc :cursor (pitch-center pitch))))))

;; Dispatcher to avoid circular function dependencies
(def modes {:pitch pitch-mode
            :player-select player-select-mode
            :player-kick player-kick-mode})

(defn make-mode [mode & args]
  (apply (modes mode) args))

(defn next-state [reply s]
  (if (= :game-done reply)
    :game-done
    (cond-> (or (:new-state reply) s)
      (contains? reply :mode-into) (-> (update :modes conj (:mode s))
                                       (assoc :mode (apply make-mode (:mode-into reply)))
                                       (assoc :status-line (str "new mode " (first (:mode-into reply)))))
      (contains? reply :mode-done) (-> (assoc :mode (-> s :modes peek))
                                       (update :modes pop)))))

(defn status-line [s]
  (->> (:modes s)
       (into [(:mode s)])
       (map :name)
       str))

(defn main-loop []
  (loop [s state]
    (ascii/redraw s)
    (debug-log "mode: " (-> s :mode :name)  ", stack: " (into [] (map :name (:modes s))))
    (let [input-key (ascii/input)
          s* (or (some-> s
                         :mode
                         :handlers
                         (get input-key)
                         (apply [s])
                         (next-state s))
                 s)]
      (when-not (= s* :game-done) (recur s*)))))

(defn main [ui-type]
  (ascii/init)
  (try
    (init)
    (main-loop)
    (finally (ascii/stop))))

(defn -main [& args]
  (main :ascii))
