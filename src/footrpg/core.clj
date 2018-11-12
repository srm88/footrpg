(ns footrpg.core
  (:require [footrpg.util :refer [debug-log]]
            [clojure.string :as string]
            [clojure.core.matrix.linear :as linear]
            [clojure.core.matrix :as matrix])
  (:gen-class))

(defn make-state []
  {:cursor [0 0]
   :kind :state
   :returns (list)})

(defn make-game []
  {:entities {:ball nil}
   :kind :game
   :teams {:home nil :away nil}
   :score {:home 0
           :away 0}})

(defn make-pitch []
  ;; This is roughly the size in yards, divided by 2, of Wembley in london
  {:width 58
   :height 37})

(defn pitch-center [p]
  [(-> (:width p) (quot 2))
   (-> (:height p) (quot 2))])

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

(def game-done (constantly :game-done))

(declare menu-mode)
(declare forever-mode)
(declare turn-mode)
(declare ->tile-info-menu)
(declare player-select-mode)
(declare maybe-player-move-mode)
(declare maybe-player-kick-mode)
(declare player-turn-commit)
(declare tile-input-mode)
(declare ->momentums-mode)

(defn modes [s]
  (into [(:mode s)] (:modes s)))

(defn dump-modes [ms]
  (->> ms
       (map :name)
       (string/join " < ")))

(defn active-modes [s]
  (let [active (into [] (take-while #(not (:modal? %)) (modes s)))
        terminal (first (filter :modal? (modes s)))]
    (cond-> active
      (some? terminal) (conj terminal))))

(defn subs* [s length]
  (subs s 0 (min (count s) length)))

(defn dump-mode [s]
  (let [m (:mode s)
        mode-name (:name m)]
    (str mode-name "\n  " (string/join "\n  " (for [[k v] (dissoc m :name :handlers)]
                                                (str k ": " (subs* (str v) 20)))))))

(defn dump-state [s]
  (str "State\n"
       "  modes:   " (dump-modes (modes s)) "\n"
       "  active:  " (dump-modes (active-modes s)) "\n"
       "  returns: " (str (:returns s)) "\n"
       "  mode:    " (dump-mode s)
       ))

(defn set-status-line [s]
  (assoc s :status-line (dump-modes (active-modes s))))

(defn mode-into [s mode-fn & args]
  (-> s
      (update :modes conj (:mode s))
      (assoc :mode (apply mode-fn s args))
      set-status-line))

(defn expect-return [s handler mode-fn & args]
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

(defmulti act (fn [s action] (:action action)))

(defmethod act :move [s action]
  (let [m (:move action)]
    (update-in s [:game :entities (:entity m)] move (:to m))))

(defmethod act :default [s action] s)

(defn action-taken [mode action-kind]
  (seq (filter #(= (:action %) action-kind) (:actions mode))))

(defn player-move-action [player to]
  {:kind :action
   :action :move
   :move {:entity (:id player)
          :from (:tile player)
          :to to}})

(defn player-kick-action [player ball to]
  {:kind :action
   :action :move
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
    (when-let [f (:fn item)] (f s))))

(declare menu-mode-handlers)
(defn menu-mode [s mode-name menu-opts]
  {:name mode-name
   :kind :mode
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

(declare forever-mode-handlers)
(defn forever-mode [s]
  {:name "turn 0"
   :kind :mode
   :turn 0
   :team :home
   :handlers forever-mode-handlers})

(defn next-turn [s]
  (let [turn* (-> s :mode :turn inc)
        team* (if (= :home (get-in s [:mode :team])) :away :home)]
    (-> s
        (update :mode merge {:name (str "turn " turn*)
                             :turn turn*
                             :team team*})
        (mode-into turn-mode team*))))

(def forever-mode-handlers {:enter next-turn
                            :escape game-done
                            \v ->momentums-mode
                            \q game-done})

(declare turn-mode-handlers)
(defn turn-mode [s team-id]
  {:name :turn-mode
   :kind :mode
   :modal? true
   :team team-id
   :handlers turn-mode-handlers
   :actions-by-player {}
   :actions []})

(defn ->player-info-menu [s player]
  (mode-into s menu-mode :player-info-menu (->> [(str (:name player) " #" (:number player))
                                                 (str "Acc " (:quick player))]
                                                (map #(hash-map :text %)))))

(defn ->players-info-menu [s]
  (mode-into s menu-mode :players-info-menu
             (->> (players s)
                  (map #(hash-map :text (str (:name %) " #" (:number %))
                                  :fn (fn [s*] (->player-info-menu s* %)))))))

;; XXX multimethod for tile-info
(defn ->tile-info-menu [s]
  (when-let [player (at-tile s (:cursor s))]
    (->player-info-menu s player)))

;; XXX player-info-mode should show momentums
(declare ephemeral-pitch-mode-handlers)
(defn momentums-mode [s]
  {:kind :mode
   :name :momentums
   :entities (into [] (filter :vect (-> s :game :entities vals)))
   :handlers ephemeral-pitch-mode-handlers})

(defn ->momentums-mode [s]
  (mode-into s momentums-mode))

(defn return-actions [s]
  ;; XXX confirmation menu
  (return s (get-in s [:mode :actions])))

(defn commit-actions [s]
  ;; Generic 'apply list of actions'
  (mode-done (if-let [actions (seq (get-in s [:mode :actions]))]
               (reduce act s actions)
               s)))

(defn move-player [s player]
  (expect-return s (fn [s* tile]
                     (-> s*
                         (update-in [:mode :player] move tile)
                         (update-in [:mode :actions] conj (player-move-action player tile))))
                 tile-input-mode :player-move (player-range player (:pitch s))))

(defn control-player [s]
  (if-let [player (at-tile s (:cursor s))]
    (if (= (:team player) (get-in s [:mode :team]))
      (-> s
          (expect-return (fn [s* actions]
                           (let [actions-by-player (assoc (get-in s* [:mode :actions-by-player]) (:id player) actions)]
                             (update s* :mode merge {:actions-by-player actions-by-player
                                                     :actions (flatten (vals actions-by-player))})))
                         player-select-mode player)
          (move-player player))
      (->tile-info-menu s))
    nil))

(declare player-select-mode-handlers)
(defn player-select-mode [s player]
  {:name :player-select
   :kind :mode
   :handlers player-select-mode-handlers
   :player player
   :ball (get-in s [:game :entities :ball])
   :actions []})

(defn maybe-player-move-mode [s]
  (when (not (action-taken (:mode s) :move))
    (move-player s (get-in s [:mode :player]))))

(defn maybe-player-kick-mode [s]
  ;; XXX need a better approach for factoring hypothetical actions into
  ;; our interrogation of the game state
  (let [ball (get-in s [:game :entities :ball])
        player (get-in s [:mode :player])]
    (when (and (-> (:tile ball)
                   (subtract-vect (:tile player))
                   magnitude
                   (< 2))
               (not (action-taken (:mode s) :kick)))
      (expect-return s (fn [s* tile]
                         (-> s*
                             (update-in [:mode :ball] move tile)
                             (update-in [:mode :actions] conj (player-kick-action player ball tile))))
                     tile-input-mode :player-kick (player-kick-range player ball (:pitch s))))))

(def turn-mode-handlers {:left pitch-cursor-left
                         :right pitch-cursor-right
                         :up pitch-cursor-up
                         :down pitch-cursor-down
                         \i ->tile-info-menu
                         \p ->players-info-menu
                         :enter control-player
                         \g (fn [s] (-> s commit-actions next-turn))
                         \v ->momentums-mode
                         :escape mode-done
                         \q mode-done})

(def player-select-mode-handlers {:left pitch-cursor-left
                                  :right pitch-cursor-right
                                  :up pitch-cursor-up
                                  :down pitch-cursor-down
                                  \k maybe-player-kick-mode
                                  \i #(->player-info-menu % (get-in % [:mode :player]))
                                  \m maybe-player-move-mode
                                  \v ->momentums-mode
                                  :enter return-actions
                                  :escape mode-done
                                  \q mode-done})

(defn return-tile-input [s]
  (let [tile (:cursor s)]
    (when (contains? (-> s :mode :tile-range) tile)
      (return s tile))))

(declare tile-input-handlers)
(defn tile-input-mode [s mode-name tile-range]
  {:name mode-name
   :kind :mode
   :tile-range tile-range
   :handlers tile-input-handlers})

(def ephemeral-pitch-mode-handlers {:left pitch-cursor-left
                                    :right pitch-cursor-right
                                    :up pitch-cursor-up
                                    :down pitch-cursor-down
                                    :enter mode-done
                                    :escape mode-done
                                    \q mode-done})

(def tile-input-handlers {:left pitch-cursor-left
                          :right pitch-cursor-right
                          :up pitch-cursor-up
                          :down pitch-cursor-down
                          :enter return-tile-input
                          \v ->momentums-mode
                          :escape mode-done
                          \q mode-done})
