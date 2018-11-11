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

(defn dispatcher [s input]
  (let [handlers (get-in s [:mode :handlers])
        return-handler (get-in s [:mode :return-handler])]
    (cond
      (= :key (:kind input)) (when-let [f (get handlers (:value input))] (f s))
      (and (= :return (:kind input))
           (some? return-handler)) (return-handler (assoc-in s [:mode :return-handler] nil) (:value input)))))

(defn get-input [s]
  (if (seq (:returns s))
    (do
      (debug-log "Got a stored input: " (-> s :returns peek))
      [{:kind :return :value (-> s :returns peek)} (update s :returns pop)])
    [{:kind :key :value (ascii/input)} s]))

(defn main-loop []
  (loop [s state]
    (ascii/redraw s)
    (let [[input s*] (get-input s)
          s** (or (dispatcher s* input) s*)]
      (when-not (= s** :game-done)
        (debug-log (f/dump-state s**))
        (recur s**)))))

(defn main []
  (ascii/init)
  (try
    (init)
    (main-loop)
    (finally (ascii/stop))))

(defn -main [& args]
  (main))
