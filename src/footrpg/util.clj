(ns footrpg.util
  (:require [clojure.string :as string]))

(defn subs* [s length]
  (subs s 0 (min (count s) length)))

(defn debug-log [& args]
  (let [msg (apply str args)]
    (spit "footrpg.log" (if (string/ends-with? msg "\n") msg (str msg "\n")) :append true)))
