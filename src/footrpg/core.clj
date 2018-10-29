(ns footrpg.core
  (:require [footrpg.ascii :as ascii])
  (:gen-class))

(defn make-state []
  {:teams {:home nil
           :away nil}
   :ball nil
   :score {:home 0
           :away 0}})

(defn make-pitch []
  {:width 56
   :height 37})

(defn make-location []
  {:vect nil
   :x nil
   :y nil})

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

(defn add-vect [location vect]
  (-> location
      (assoc :vect vect)
      (update :x + (:x vect))
      (update :y + (:y vect))))

(defn move [thing vect]
  (update thing :location add-vect vect))

(defn main [ui-type]
  (ascii/main))

(defn -main [& args]
  (main :ascii))
