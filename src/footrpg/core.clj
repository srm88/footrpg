(ns footrpg.core
  (:require [footrpg.util :refer [debug-log]]
            [clojure.string :as string]
            [clojure.core.matrix.linear :as linear]
            [clojure.core.matrix :as matrix])
  (:gen-class))

(defn make-game []
  {:entities {:ball nil}
   :teams {:home nil :away nil}
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

(def up [0 -1])
(def down [0 1])
(def left [-1 0])
(def right [1 0])
(def up-left [-1 -1])
(def up-right [1 -1])
(def down-left [-1 1])
(def down-right [1 1])

(defn add-vect [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn subtract-vect [[x y] [dx dy]]
  [(- x dx) (- y dy)])

(defn magnitude [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn levenshtein [[x y] [x* y*]]
  (+ (Math/abs (- x x*)) (Math/abs (- y y*))))

(defn path* [from to]
  (let [lev (levenshtein from to)
        ;; Want to minimize score
        score (fn [tile step] (levenshtein (add-vect tile step) to))
        options [up down left right up-left up-right down-left down-right]]
    (loop [tile from
           acc []]
      (if (= tile to)
        acc
        (let [step (first (sort-by #(score tile %) options))]
          (recur (add-vect tile step) (conj acc step)))))))

(defn path [from to]
  (let [[x y] (subtract-vect to from)
        horiz (if (>= x 0) right left)
        verti (if (>= y 0) down up)
        x* (Math/abs x)
        y* (Math/abs y)
        horizontals (repeat x* horiz)
        verticals (repeat y* verti)
        steps (cond-> (interleave horizontals verticals)
                (not= x* y*) (concat (if (> x* y*)
                                       (repeat (- x* y*) horiz)
                                       (repeat (- y* x*) verti))))]
    (into [] steps)))

(defn move [thing tile]
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

(defn players [s]
  (->> s :game :entities (vals) (filter #(= (:kind %) :player))))

(defn at-tile [s tile]
  (->> s players (filter #(= tile (:tile %))) first))

(defn in-bounds? [[x y] pitch]
  (and (>= x 0) (< x (dec (:width pitch)))
       (>= y 0) (< y (dec (:height pitch)))))

(defn bounded* [x limit]
  (-> x (max 0) (min (dec limit))))

(defn bounded [[x y] pitch]
  [(bounded* x (:width pitch))
   (bounded* y (:height pitch))])

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

(def game-done (constantly :game-done))

(declare pitch-mode)
(declare menu-mode)
(declare player-select-mode)
(declare maybe-player-move-mode)
(declare maybe-player-kick-mode)
(declare player-turn-commit)
(declare tile-input-mode)

(defn dump-modes [s]
  (->> (:modes s)
       (into [(:mode s)])
       (map :name)
       (string/join " < ")))

(defn subs* [s length]
  (subs s 0 (min (count s) length)))

(defn dump-mode [s]
  (let [m (:mode s)
        mode-name (:name m)]
    (str mode-name "\n  " (string/join "\n  " (for [[k v] (dissoc m :name :handlers)]
                                                (str k ": " (subs* (str v) 20)))))))

(defn dump-state [s]
  (str "State\n"
       "  modes: " (dump-modes s) "\n"
       "  returns: " (str (:returns s)) "\n"
       "  mode: " (dump-mode s)
       ))

(defn set-status-line [s]
  (assoc s :status-line (dump-modes s)))

(defn mode-into [s mode-fn & args]
  (let [current (:mode s)
        mode (apply mode-fn s args)]
    (-> s
        (update :modes conj current)
        (assoc :mode mode)
        set-status-line)))

(defn expect-return [s handler mode-fn & args]
  (debug-log "expecting return in mode " (-> s :mode :name))
  (apply mode-into (assoc-in s [:mode :return-handler] handler)
                   mode-fn args))

(defn mode-done [s]
  (if-let [previous (-> s :modes peek)]
    (-> s
        (assoc :mode (cond-> previous
                       (empty? (:returns s)) (assoc :return-handler nil)))
        (update :modes pop)
        set-status-line)
    :game-done))

(defn return [s value]
  (-> s
      (update :returns conj value)
      mode-done))

;; Multimethod?
(defn act [s action]
  (if-let [m (:move action)]
    (update-in s [:game :entities (:entity m)] move (:to m))
    s))

(defn player-move-action [player to]
  {:kind :action
   :action :move
   :move {:entity (:id player)
          :from (:tile player)
          :to to}})

(defn player-kick-action [player ball to]
  {:kind :action
   :action :kick
   :player player
   :move {:entity :ball
          :from (:tile ball)
          :to to}})

(defn menu-cursor-up [s]
  (-> s
      (update-in [:mode :cursor] dec)
      (update-in [:mode :cursor] bounded* (-> s :mode :menu count))))

(defn menu-cursor-down [s]
  (-> s
      (update-in [:mode :cursor] inc)
      (update-in [:mode :cursor] bounded* (-> s :mode :menu count))))

(defn menu-select [s]
  (let [m (:mode s)
        item (nth (:menu m) (:cursor m))]
    ((:fn item) s)))

(declare menu-mode-handlers)
(defn menu-mode [s mode-name menu-opts]
  {:name mode-name
   :cursor 0
   ;; how can we return from a menu?
   ;; list of {:text, :fn}
   :menu menu-opts
   :handlers menu-mode-handlers})

(def menu-mode-handlers {:down menu-cursor-down
                         :up menu-cursor-up
                         :enter menu-select
                         :escape mode-done
                         \q mode-done})

(defn player-turn-commit [s]
  (let [m (:mode s)
        player (:player m)
        ball (:ball m)
        actions (:actions m)]
    ;; Generic 'apply list of actions'
    (mode-done (if (seq actions)
                 (reduce act s actions)
                 s))))

(defn move-player [s player]
  (expect-return s (fn [s* tile]
                     (-> s*
                         (update-in [:mode :player] move tile)
                         (update-in [:mode :actions] conj (player-move-action player tile))))
                 tile-input-mode :player-move (player-range player (:pitch s))))

(defn pitch-select [s]
  (if-let [player (at-tile s (:cursor s))]
    (-> s
        (mode-into player-select-mode player)
        (move-player player))
    nil))

(declare player-select-mode-handlers)
(defn player-select-mode [s player]
  {:name :player-select
   :handlers player-select-mode-handlers
   :player player
   :ball (get-in s [:game :entities :ball])
   :actions []})

(defn action-taken [mode action-kind]
  (seq (filter #(= (:action %) action-kind) (:actions mode))))

;; ### kick
(defn maybe-player-move-mode [s]
  (when (not (action-taken (:mode s) :move))
    (move-player s (get-in s [:mode :player]))))

(defn player-info-menu [s]
  (let [player (get-in s [:mode :player])]
    (mode-into s menu-mode :player-info-menu [{:text "Foobar"
                                               :fn (fn [s] s)}])))

(defn maybe-player-kick-mode [s]
  ;; XXX need a better approach for factoring hypothetical actions into
  ;; our interrogation of the game state
  (let [ball (get-in s [:game :entities :ball])
        ball-tile (:tile ball)
        player (get-in s [:mode :player])
        player-tile (:tile player)]
    (when (and (-> ball-tile (subtract-vect player-tile) magnitude (< 2))
               (not (action-taken (:mode s) :kick)))
      (expect-return s (fn [s* tile]
                         (-> s*
                             (update-in [:mode :ball] move tile)
                             (update-in [:mode :actions] conj (player-kick-action player ball tile))))
                     tile-input-mode :player-kick (player-kick-range player ball (:pitch s))))))

(def pitch-mode-handlers {:left pitch-cursor-left
                          :right pitch-cursor-right
                          :up pitch-cursor-up
                          :down pitch-cursor-down
                          :enter pitch-select
                          :escape game-done
                          \q game-done})

;; This will become a menu ... 
(def player-select-mode-handlers {:left pitch-cursor-left
                                  :right pitch-cursor-right
                                  :up pitch-cursor-up
                                  :down pitch-cursor-down
                                  \k maybe-player-kick-mode
                                  \m maybe-player-move-mode
                                  \i player-info-menu
                                  :enter player-turn-commit
                                  :escape mode-done
                                  \q mode-done})

(defn return-tile-input [s]
  (let [tile (:cursor s)]
    (when (contains? (-> s :mode :tile-range) tile)
      (return s tile))))

(declare tile-input-handlers)
(defn tile-input-mode [s mode-name tile-range]
  {:name mode-name
   :tile-range tile-range
   :handlers tile-input-handlers})

(def tile-input-handlers {:left pitch-cursor-left
                          :right pitch-cursor-right
                          :up pitch-cursor-up
                          :down pitch-cursor-down
                          :enter return-tile-input
                          :escape mode-done
                          \q mode-done})

(defn pitch-mode [s]
  {:name :pitch
   :handlers pitch-mode-handlers})

(defn make-state []
  {:cursor [0 0]
   :returns (list)})

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
        (assoc :entities {:ball (assoc (make-ball) :tile (pitch-center pitch))})
        (assoc :teams {:home {:name :real-madrid :color {:bg :white :fg :black}}
                       :away {:name :barcelona :color {:bg :red :fg :white}}})
        (update :entities merge (->> [(home 25 "Courtois" :gk)
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
                                      (away 7 "Coutinho" :left-wing :quick 4)]
                                     (map #(vector (:id %) %))
                                     (into {}))))))
