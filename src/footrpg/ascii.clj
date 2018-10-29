(ns footrpg.ascii
  (:require [lanterna.screen :as s]
            [clojure.string :as string]))

(def ascii-pitch (string/split-lines (slurp "resources/ascii-pitch.txt")))

(defn make-ui []
  {})

(defn ui-loop []
  )

(defn redraw [ui]
  )

;; Probably should bring in stuart sierra component shortly
(defn main []
  (let [screen (s/get-screen :text)]
    (s/in-screen screen
                 (doseq [[i line] (map-indexed vector ascii-pitch)]
                   (s/put-string screen 0 i line))
                 (s/redraw screen)
                 (s/get-key-blocking screen)
                 (s/stop screen))))
