(ns sketch.utils
  (:require [quil.core :as q]))

(defn clamp [x lower upper]
  "Restrict the value to the given range [lower upper]"
  (max lower (min x upper)))

(defn gauss [mean variance]
  "Samples a single value from a Gaussian distribution with the given
   mean and variance"
  (+ mean (* variance (q/random-gaussian))))

;; Color function
(defn make-palette [fname]
  "Creates a color palette from a given image. Also adds black and
   white to the palette"
  (let [b         (q/load-image fname)
        coords    (for [x (range (.-width b))
                        y (range (.-height b))]
                    [x y])
        add-color (fn [palette [x y]]
                    (let [c      (q/get-pixel b x y)
                          exists (some #(= c %) palette)]
                      (if exists
                        palette
                        (conj palette c))))
        palette   (reduce add-color [] coords)]
    (concat palette
            (repeat 6 (q/color 255))
            (repeat 6 (q/color 0)))))

(defn uuid [] (random-uuid))
