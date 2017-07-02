(ns sketch.happy
  (:require [quil.core :as q :include-macros true]
            [sketch.utils :as utils]))

(def dim 500)
(def width dim)
(def height dim)

;; Define global variables
(def freunde (atom []))
(def radius 50)
(def num-points 64)
(def num-sands 30)
(def num-friends 64)
(def max-con 10)
(defn angle [x]
  (/ (* q/TWO-PI x) num-points))

(def img-id (atom (utils/uuid)))
(def img-num (atom 0))

;; Define colors to use
(def good-colors (atom []))

(defn some-color []
  (rand-nth @good-colors))

(defprotocol ISand
  (paint [this x y ox oy]))

(deftype Sand [p
               c ; color
               ^:volatile-mutable g]

  ISand
  (paint [this x y ox oy]
    (let [gg (utils/clamp (+ g (q/random -0.050 0.050))
                          -0.22 0.22)
          w (/ g 10.0)]
      (q/stroke (q/red c) (q/green c) (q/blue c) 28)
      (q/point (+ ox (* (- x ox) (q/sin p)))
               (+ oy (* (- y oy) (q/sin p))))
      (set! g gg)
      (doseq [i (range 11)]
        (let [a (/ (- 0.1 i) 110)]
          (q/stroke (q/red c) (q/green c) (q/blue c) (* 256 a))
          (q/point (+ ox (* (- x ox) (q/sin (+ p (q/sin (* i w))))))
                   (+ oy (* (- y oy) (q/sin (+ p (q/sin (* i w)))))))
          (q/point (+ ox (* (- x ox) (q/sin (- p (q/sin (* i w))))))
                   (+ oy (* (- y oy) (q/sin (- p (q/sin (* i w))))))))))))

(defn sand []
  (let [p (q/random 1.0)
        c (some-color)
        g (q/random 0.01, 0.1)]
    (Sand. p c g)))

(defprotocol IFreund
  (render [this])
  (get-x [this])
  (get-y [this])
  (set-x [this nx])
  (set-y [this ny])
  (move [this])
  (connect-to [this f])
  (expose [this])
  (expose-connections [this])
  (render-connections [this])
  (find-happiness [this]))

(deftype Freund [^:volatile-mutable x
                 ^:volatile-mutable y
                 id
                 color
                 lencon
                 ^:volatile-mutable sands
                 ^:volatile-mutable connections
                 ^:volatile-mutable r
                 ^:volatile-mutable dx
                 ^:volatile-mutable dy
                 ^:volatile-mutable vx
                 ^:volatile-mutable vy]
  IFreund
  (render [this])

  (get-x [this] x)
  (set-x [this nx]
    (set! x nx))

  (get-y [this] y)
  (set-y [this ny]
    (set! y ny))

  (move [this]
    (set! x (+ x vx))
    (set! y (+ y vy))
    (set! vx (* vx 0.92))
    (set! vy (* vy 0.92)))

  (connect-to [this idx]
    (set! connections (conj connections idx)))

  (expose [this]
    (doseq [dx (range -2 3)]
      (let [a (- 0.5 (/ (q/abs dx) 5.0))]
        (q/stroke 0 (* 256 a))
        (q/point (+ x dx) y)
        (q/stroke 255 (* 256 a))
        (q/point (+ x (- dx 1)) (- y 1))))
    (doseq [dy (range -2 3)]
      (let [a (- 0.5 (/ (q/abs dy) 5.0))]
        (q/stroke 0 (* 256 a))
        (q/point x (+ y dy))
        (q/stroke 255 (* 256 a))
        (q/point (- x 1) (+ y (- dy 1))))))

  (expose-connections [this]
    "Draw connection lines to all friends"
    (doseq [conn connections]
      (let [ox (get-x (get @freunde conn))
            oy (get-y (get @freunde conn))]
        (doseq [j (range num-sands)]
          (paint (get sands j) x y ox oy)))))

  (render-connections [this]
    (doseq [conn connections]
      (let [friend (get @freunde conn)
            ddx (- (get-x friend) x)
            ddy (- (get-y friend) y)
            m (int (+ 1 (/ (q/sqrt (+ (* ddx ddx) (* ddy ddy))) 6)))]
        (doseq [j (range m)]
          (let [t (/ (+ 1 (q/cos (* j (/ q/PI m)))) 2)
                px (int (+ x (* t ddx)))
                py (int (+ y (* t ddy)))]
            (q/stroke 51 51 51)
            (q/point px py))))))

  (find-happiness [this]
    "Move closer to friends, further from enemies"
    (let [friends @freunde
          iter (fn [[ax ay] i]
                 (if (= (get friends i) this)
                   [ax ay]
                   (let [f (get friends i)
                         ddx (- (get-x f) x)
                         ddy (- (get-y f) y)
                         d (q/sqrt (+ (* ddx ddx) (* ddy ddy)))
                         t (q/atan2 ddy ddx)
                         is-friend (contains? connections i)]
                     (cond
                       (and is-friend (> d lencon))
                       [(+ ax (* 4.0 (q/cos t)))
                        (+ ay (* 4.0 (q/sin t)))]
                       (and (not is-friend) (< d lencon))
                       [(+ ax (* (- lencon d) (q/cos (+ t q/PI))))
                        (+ ay (* (- lencon d) (q/sin (+ t q/PI))))]
                       :else [ax ay]))))
          [ax ay] (reduce iter [0.0 0.0] (range num-friends))]
      (set! vx (+ vx (/ ax 42.22)))
      (set! vy (+ vy (/ ay 42.22))))))

(defn make-freund [x y id color]
  (Freund. x y id color (+ 10 (q/random 50))
           (vec (for [_ (range num-sands)]
                  (sand)))
           #{}
           10 x y 1.0 1.0))

(def time (atom 0))

(defn draw []
  (doseq [f @freunde]
    (move f))
  (doseq [f @freunde]
    (expose f)
    (expose-connections f))
  (if (= (mod (swap! time inc) 2) 0)
    (doseq [f @freunde]
      (find-happiness f))))

(defn restart-happy-place []
  (reset! img-num 0)
  (reset! img-id (utils/uuid))
  (reset! freunde
          (vec
            (for [i (range num-friends)]
              (let [fx (+ (/ dim 2) (* 0.4 dim (q/cos (angle i))))
                    fy (+ (/ dim 2) (* 0.4 dim (q/sin (angle i))))]
                (make-freund fx fy i (some-color))))))
  ;; Make random connections
  (doseq [i (range (* 2.2 num-friends))]
    (let [a (int (q/floor (q/random num-friends)))
          b (int (q/floor (mod (+ a (q/random 22)) num-friends)))
          b (if (or (>= b num-points) (< b 0)) 0 b)]
      (if (not= a b)
        (do
          (connect-to (get @freunde a) b)
          (connect-to (get @freunde b) a))))))

(defn onkeypressed []
  (cond
    (= (int (q/key-code)) 83) ; key: S
    (do
      (q/save-frame (str "sketch-" @img-id "-" @img-num ".tif"))
      (swap! img-num inc))
    (= (int (q/key-code)) 82) ; key: r
    (do (q/background (some-color))
        (restart-happy-place))))

(defn setup []
  (q/color-mode :rgb 255)
  (q/background 255)
  (q/frame-rate 30)
  (reset! good-colors (utils/make-palette "brendan.gif"))
  (restart-happy-place)
  (enable-console-print!))

(comment
;;; OLD FUNCTIONS
  (defn setup []
                                        ; Set frame rate to 30 frames per second.
    (q/frame-rate 30)
                                        ; Set color mode to HSB (HSV) instead of default RGB.
    (q/color-mode :hsb)
                                        ; setup function returns initial state. It contains
                                        ; circle color and position.
    {:color 0
     :angle 0})

  (defn update-state [state]
                                        ; Update sketch state by changing circle color and position.
    {:color (mod (+ (:color state) 0.7) 255)
     :angle (+ (:angle state) 0.1)})

  (defn draw-state [state]
                                        ; Clear the sketch by filling it with light-grey color.
    (q/background 240)
                                        ; Set circle color.
    (q/fill (:color state) 255 255)
                                        ; Calculate x and y coordinates of the circle.
    (let [angle (:angle state)
          x (* 150 (q/cos angle))
          y (* 150 (q/sin angle))]
                                        ; Move origin point to the center of the sketch.
      (q/with-translation [(/ (q/width) 2)
                           (/ (q/height) 2)]
                                        ; Draw the circle.
        (q/ellipse x y 100 100))))
  )
