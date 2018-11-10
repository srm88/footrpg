(ns footrpg.main
  (:require [footrpg.ascii :as ascii]
            [footrpg.util :refer [debug-log]]
            [footrpg.core :as f])
  (:gen-class))

(def state (f/make-state))

(defn init []
  (let [pitch (f/make-pitch)
        s (-> state
              (assoc :pitch pitch)
              (assoc :modes (list))
              (assoc :game (f/init-game pitch))
              (assoc :cursor (f/pitch-center pitch)))]
    (def state (assoc s :mode (f/pitch-mode s)))))

(defn main-loop []
  (loop [s state]
    (ascii/redraw s)
    (debug-log "mode: " (-> s :mode :name)  ", stack: " (into [] (map :name (:modes s))))
    (let [input-key (ascii/input)
          s* (or (apply (-> s :mode :handler) [s input-key])
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
