(ns footrpg.util
  (:require [clojure.string :as string]))

(defn debug-log [& args]
  (let [msg (apply str args)]
    (spit "footrpg.log" (if (string/ends-with? msg "\n") msg (str msg "\n")) :append true)))
