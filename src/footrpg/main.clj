(ns footrpg.main
  (:require [footrpg.ascii :as ascii]
            [footrpg.util :refer [debug-log]]
            [footrpg.mode :refer [set-status-line]]
            [footrpg.core :as f])
  (:gen-class))

(def state (f/make-state))

(defn debug-state [s & msg]
  (when msg (debug-log (apply str "==========" msg)))
  (debug-log (f/dump-state s))
  s)

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
  (let [home (fn [number name* position & args] (apply assoc (f/make-player) :team :home
                                                              :number number
                                                              :name name*
                                                              :tile (-> formations :433 position (home-tile pitch)) args))
        away (fn [number name* position & args] (apply assoc (f/make-player) :team :away
                                                              :number number
                                                              :name name*
                                                              :tile (-> formations :433 position (away-tile pitch)) args))]
    (-> (f/make-game)
        (assoc :entities {:ball (assoc (f/make-ball) :tile (f/pitch-center pitch))})
        (assoc :teams {:home {:name :real-madrid :color {:bg :white :fg :black}}
                       :away {:name :barcelona :color {:bg :red :fg :white}}})
        (update :entities merge (->> [(home 25 "Courtois" :gk)
                                      (home 12 "Marcelo" :left-back)
                                      (home 4 "Ramos" :left-cb)
                                      (home 5 "Varane" :right-cb)
                                      (home 6 "Nacho" :right-back)
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

(defn init []
  (let [pitch (f/make-pitch)
        s (-> state
              (assoc :pitch pitch)
              (assoc :modes (list))
              (assoc :game (init-game pitch))
              (assoc :cursor (f/pitch-center pitch)))]
    (def state (-> s
                   (assoc :mode (f/forever-mode s))
                   (f/next-turn)))))

(defn handle-input [s input]
  (or (when-let [handler (when input (get-in s [:mode :handlers input]))]
        (handler s))
      s))

(defn set-modes [s modes]
  (assoc s :mode (first modes) :modes (rest modes)))

(defn reap-modes [s]
  (->> (conj (:modes s) (:mode s))
       (remove nil?)
       (remove :done)
       (set-modes s)))

(defn game-done? [s]
  (or (:game-done (:mode s)) (some :game-done (:modes s))))

(defn update-entities [s]
  s)

(defn update-entities-n-times [s n]
  (reduce (fn [s* _] (update-entities s*)) s (range n)))

(defn process-input [s]
  (-> s
      (handle-input (ascii/input))
      reap-modes
      set-status-line)) ; XXX This should be part of render

(defn constant-update-and-frame-rate-main-loop [updates-per-sec]
  (let [ms-per-update (/ 1000 updates-per-sec)]
    (loop [s state
           sleep 0]
      (when (> sleep 0)
        (Thread/sleep sleep))
      (let [start (System/currentTimeMillis)]
        (when-not (game-done? s)
          (-> s
              process-input
              update-entities
              (doto ascii/redraw)
              (recur (-> ms-per-update
                         (- (System/currentTimeMillis))
                         (+ start)))))))))

; For posterity -- this is almost certainly excessive for our game
; https://gameprogrammingpatterns.com/game-loop.html
(defn constant-update-variable-frame-rate-main-loop [updates-per-sec]
  (let [ms-per-update (/ 1000 updates-per-sec)]
    (loop [s state
           start (System/currentTimeMillis)
           lag 0]
      (let [now (System/currentTimeMillis)
            elapsed (- now start)
            start* now
            lag* (+ lag elapsed)
            steps (inc (quot lag ms-per-update))
            lag* (- lag (* steps ms-per-update))]
        (when-not (game-done? s)
          (-> s
              process-input
              ;(debug-state "after process input")
              (update-entities-n-times steps)
              ;(debug-state "after update")
              (doto ascii/redraw)
              (recur start* lag*)))))))

(defn main []
  (ascii/init)
  (try
    (init)
    (constant-update-and-frame-rate-main-loop 24)
    (finally (ascii/stop))))

(defn -main [& args]
  (main))
