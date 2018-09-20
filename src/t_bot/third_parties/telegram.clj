(ns t-bot.third-parties.telegram
  (:require
    [morse.handlers :as h]
    [morse.api :as api]))

#_(h/defhandler bot-api
  (h/command "start" {{id :id :as chat} :chat}
    (println "Bot joined new chat: " chat)
    (api/send-text token id "Welcome!"))

  (h/command "help" {{id :id :as chat} :chat}
    (println "Help was requested in " chat)
    (api/send-text token id "Help is on the way"))

  (h/message message (println "Intercepted message:" message)))

(defn send-msg! [{:keys [telegram-api-key telegram-chat-id] :as config} msg]
  (api/send-text telegram-api-key telegram-chat-id msg))
