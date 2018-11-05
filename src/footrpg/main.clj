(ns footrpg.main
  (:require [footrpg.ascii :as ascii]
            [footrpg.util :refer [debug-log]]
            [footrpg.core :as f])
  (:gen-class))

(def state (f/make-state))

(defn init []
  (let [pitch (f/make-pitch)]
    (def state (-> state
                   (assoc :pitch pitch)
                   (assoc :mode (f/pitch-mode))
                   (assoc :modes (list))
                   (assoc :game (f/init-game pitch))
                   (assoc :cursor (f/pitch-center pitch))))))

(defn main-loop []
  (loop [s state]
    (ascii/redraw s)
    (debug-log "mode: " (-> s :mode :name)  ", stack: " (into [] (map :name (:modes s))))
    (let [input-key (ascii/input)
          s* (or (doto (some-> s
                         :mode
                         :handlers
                         (get input-key)
                         (apply [s])) (debug-log " from handler of mode " (-> s :mode :name)))
                 s)]
      (when-not (= s* :game-done) (recur s*)))))

(defn main []
  (ascii/init)
  (try
    (init)
    (main-loop)
    (finally (ascii/stop))))

(defn -main [& args]
  (main))
