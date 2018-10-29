(ns footrpg.core
  (:require [footrpg.ascii :as ascii])
  (:gen-class))

(defn make-game-state []
  {:teams {:home nil
           :away nil}
   :ball nil
   :score {:home 0
           :away 0}})

(defn make-pitch []
  {:width 58
   :height 37})

(defn pitch-center [p]
  [(-> (:width p) (/ 2))
   (-> (:height p) (/ 2))])

(defn pitch-top-right [p]
  [(dec (:width p)) 0])

(defn pitch-bottom-right [p]
  [(dec (:width p))
   (dec (:height p))])

(defn pitch-top-left [p]
  [0 0])

(defn pitch-bottom-left [p]
  [0 (dec (:height p))])

(defn make-location []
  {:vect nil
   :loc []})

(defn make-ball []
  {:kind :ball
   :location (make-location)})

(defn make-player []
  {:kind :player
   :team nil
   :number nil
   :name nil
   :location (make-location)}
  )

(defn add-vect [location [dx dy]]
  (-> location
      (assoc :vect [dx dy])
      (update-in [:loc 0] + dx)
      (update-in [:loc 1] + dy)))

(defn move [thing vect]
  (update thing :location add-vect vect))

;; This is now meta-game

(defn bounded [[x y] pitch]
  [(-> x (max 0) (min (dec (:width pitch))))
   (-> y (max 0) (min (dec (:height pitch))))])

(defn cursor-left [s]
  (-> s
    (update-in [:cursor 0] dec)
    (update :cursor bounded (:pitch s))))

(defn cursor-right [s]
  (-> s
    (update-in [:cursor 0] inc)
    (update :cursor bounded (:pitch s))))

(defn cursor-up [s]
  (-> s
    (update-in [:cursor 1] dec)
    (update :cursor bounded (:pitch s))))

(defn cursor-down [s]
  (-> s
    (update-in [:cursor 1] inc)
    (update :cursor bounded (:pitch s))))

(def main-mode {:left cursor-left
                :right cursor-right
                :up cursor-up
                :down cursor-down
                :escape (constantly :exit)
                \q (constantly :exit)})

(defn make-state []
  {:cursor [0 0]})

(def state (make-state))

(defn init []
  (let [pitch (make-pitch)]
    (def state (-> state
                   (assoc :pitch pitch)
                   (assoc :mode main-mode)
                   (assoc :cursor (pitch-center pitch))))))

(defn main-loop []
  (loop [s state]
    (ascii/redraw s)
    (let [c (ascii/input)
          f ((:mode s) c)
          s2 (if (nil? f) s (apply f [s]))]
      (if (= :exit s2)
        nil
        (recur s2)))))

(defn main [ui-type]
  (ascii/init)
  (try
    (init)
    (main-loop)
    (finally (ascii/stop))))

(defn -main [& args]
  (main :ascii))
