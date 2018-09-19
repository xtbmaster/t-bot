(ns t-bot.third-parties.telegram
  (:require
    [clojure.edn :as edn]
    [morse.handlers :as h]
    [morse.api :as api]))

(def ^:private token
  (->
    (clojure.core/slurp "resources/api_keys.edn")
    (edn/read-string)
    :telegram))

(h/defhandler bot-api
  (h/command "start" {{id :id :as chat} :chat}
    (println "Bot joined new chat: " chat)
    (api/send-text token id "Welcome!"))

  (h/command "help" {{id :id :as chat} :chat}
    (println "Help was requested in " chat)
    (api/send-text token id "Help is on the way"))

  (h/message message (println "Intercepted message:" message)))

(api/send-text token chat-id "hello")
