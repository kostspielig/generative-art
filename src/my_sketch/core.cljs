(ns my-sketch.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [sketch.happy :as happy]))

(q/defsketch my-sketch
  :host "my-sketch"
  :size [happy/width happy/height]
  ; setup function called only once, during sketch initialization.
  :setup happy/setup
  ; update-state is called on each iteration before draw-state.
  ; :update happy/update-state
  :draw happy/draw
  :key-pressed happy/onkeypressed
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  ; :middleware [m/fun-mode]
  )
