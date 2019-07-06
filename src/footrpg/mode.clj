(ns footrpg.mode
  (:require [footrpg.util :refer [subs*]]
            [clojure.string :as string]))

(defn modes [s]
  (conj (:modes s) (:mode s)))

(defn active-modes [s]
  (let [active (into [] (take-while #(not (:modal? %)) (modes s)))
        terminal (first (filter :modal? (modes s)))]
    (cond-> active
      (some? terminal) (conj terminal))))

(defn dump-modes [ms]
  (->> ms
       (map :name)
       (string/join " < ")))

(defn dump-mode [s]
  (let [m (:mode s)
        mode-name (:name m)]
    (str mode-name "\n  " (string/join "\n  " (for [[k v] (dissoc m :name :handlers)]
                                                (str k ": " (subs* (str v) 20))))
         (if-let [return-handler (:return-handler m)]
           (str "\n " return-handler)
           ""))))

(defn set-status-line [s]
  (assoc s :status-line (dump-modes (active-modes s))))

(defn make-mode [s mode-fn args]
  (-> (apply mode-fn s args)
      (assoc :id (gensym "mode"))))

(defn mode-into [s mode-fn & args]
  (-> s
      (update :modes conj (:mode s))
      (assoc :mode (make-mode s mode-fn args))
      set-status-line))

(defn expect-return [s handler mode-fn & args]
  (-> s
      (update :modes conj (:mode s))
      (assoc :mode (-> (make-mode s mode-fn args)
                       (assoc :return-handler handler)))
      set-status-line))

(defn return [s value]
  ;; XXX: we only support returning to the next mode on the stack
  ;; Invoke the handler with the calling mode as the active mode (hacky),
  ;; then retrieve from the modified state the modified calling mode
  ;; and restore it.
  (let [handler (get-in s [:mode :return-handler])
        calling-mode (first (:modes s))
        returning-mode (:mode s)
        s* (handler (-> s
                        (update :modes rest)
                        (assoc :mode calling-mode))
                    value)
        calling-mode* (:mode s*)]
    (-> s*
        (update :modes conj calling-mode*)
        (assoc :mode (-> returning-mode
                         (dissoc :return-handler)
                         (assoc :done true))))))

(defn mode-done [s]
  (assoc-in s [:mode :done] true))

