(ns closure.core
  (:require [closure.cell :as cell]
            [closure.cmd :as cmd]
            [closure.item :as item]
            [closure.ui :as ui]))

(def overworld-mode {:left cmd/left
                     :right cmd/right
                     :up cmd/up
                     :down cmd/down
                     :escape ui/quit
                     \i cmd/inventory
                     \g cmd/get-room
                     })

;; Main overworld
(def gprot {:kind :prot
            :bag (item/new-bag)
            :name "joe"})

(def sword {:kind :item
            :name "sword"})

(def gstate {:pos [1 1]
             :prot gprot
             :grid [[:oob (cell/new) (cell/new) (cell/new) :oob]
                    [(cell/new) (cell/new gprot) (cell/new) (cell/new) (cell/new)]
                    [:oob (cell/new sword) (cell/new) (cell/new) :oob]]
             :mode overworld-mode
             })

(defn main
  [screen-type]
  (ui/init screen-type)
  (loop [state gstate]
    (ui/redraw (:grid state))
    (let [c (ui/getch)
          fun ((:mode state) c)
          state2 (if (nil? fun) state (apply fun [state]))]
      (cond
        (= :exit state2) nil
        :else            (recur state2)))))

(defn -main [& args]
  (let [args (set args)
        screen-type (cond
                      (args ":swing") :swing
                      (args ":text")  :text
                      :else           :auto)]
    (main screen-type)))
