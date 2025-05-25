(ns plc-lib.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.string :as str]))

;; Simulated hardware state
(defonce plc-state
  (atom {:pins {}
         :scan-value 0
         :inputs {:x0 false :x1 false :x2 false :x3 false :x4 false :x5 false}
         :outputs {:y0 false :y1 false :y2 false :y3 false}
         :timers {:t0 {:state 0 :period 1000}}
         :latches {:y2 false}}))

;; Pin constants (simulated)
(def pin-config
  {:x0 :a0 :x1 :a1 :x2 :a2 :x3 :a3 :x4 :a4 :x5 :a5
   :y0 3 :y1 5 :y2 6 :y3 9})

;; Simulated I/O functions
(defn digital-read [pin]
  (get-in @plc-state [:inputs pin] 0))

(defn digital-write [pin value]
  (swap! plc-state assoc-in [:outputs pin] (if (= value 1) true false)))

(defn millis []
  (System/currentTimeMillis))

;; Core PLC functions
(defn in [input]
  (let [val (if (keyword? input) (digital-read input) input)]
    (swap! plc-state assoc :scan-value (if val 1 0))
    (:scan-value @plc-state)))

(defn out [output]
  (let [val (:scan-value @plc-state)]
    (when (keyword? output)
      (digital-write output val))
    val))

(defn and-bit [input]
  (let [val (:scan-value @plc-state)
        in-val (if (keyword? input) (digital-read input) input)]
    (swap! plc-state assoc :scan-value (bit-and val (if in-val 1 0)))
    (:scan-value @plc-state)))

(defn or-bit [input]
  (let [val (:scan-value @plc-state)
        in-val (if (keyword? input) (digital-read input) input)]
    (swap! plc-state assoc :scan-value (bit-or val (if in-val 1 0)))
    (:scan-value @plc-state)))

(defn latch [set reset output]
  "Implements a basic latch"
  (let [set-int (if (boolean? set)
                  (if set 1 0)
                  (if (keyword? set)
                    (if (digital-read set) 1 0)
                    set))
        reset-int (if (boolean? reset)
                    (if reset 1 0)
                    (if (keyword? reset)
                      (if (digital-read reset) 1 0)
                      reset))
        output-int (if (keyword? output)
                     (get-in @plc-state [:latches output] 0)
                     (if (boolean? output) (if output 1 0) output))
        ;; Ensure all values are integers before bit operations
        set-int-bit (if (boolean? set-int) (if set-int 1 0) set-int)
        reset-int-bit (if (boolean? reset-int) (if reset-int 1 0) reset-int)
        output-int-bit (if (boolean? output-int) (if output-int 1 0) output-int)
        latch-val (bit-or (bit-and set-int-bit 1)
                          (bit-and (bit-not reset-int-bit) output-int-bit))]
    (swap! plc-state assoc :scan-value latch-val)
    (when (keyword? output)
      (digital-write output (:scan-value @plc-state))
      (swap! plc-state assoc-in [:latches output] (if (= (:scan-value @plc-state) 1) true false)))
    (:scan-value @plc-state)))

(defn timer-on [timer-key]
  (let [val (:scan-value @plc-state)
        timer (get-in @plc-state [:timers timer-key])
        period (:period timer)]
    (if (zero? val)
      (do (swap! plc-state assoc-in [:timers timer-key :state] 0)
          (swap! plc-state assoc :scan-value 0))
      (if (zero? (:state timer))
        (do (swap! plc-state assoc-in [:timers timer-key :state] (millis))
            (swap! plc-state assoc :scan-value 0))
        (if (>= (- (millis) (:state timer)) period)
          (swap! plc-state assoc :scan-value 1)
          (swap! plc-state assoc :scan-value 0))))
    (:scan-value @plc-state)))

;; Drawing functions
(defn draw-contact [pin x y]
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill 255)
  (q/rect-mode :center)
  (q/rect x y 50 30)
  (q/fill 0)
  (q/text-align :center :center)
  (q/text (name pin) x y)
  (q/no-fill)
  (let [active? (get-in @plc-state [:inputs pin])]
    (if active?
      (q/stroke 0 255 0)  ;; Green for active
      (q/stroke 255 0 0)) ;; Red for inactive
    (q/rect x y 54 34)))

(defn draw-coil [pin x y]
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill 255)
  (q/ellipse-mode :center)
  (q/ellipse x y 50 30)
  (q/fill 0)
  (q/text-align :center :center)
  (q/text (name pin) x y)
  (q/no-fill)
  (let [active? (get-in @plc-state [:outputs pin])]
    (if active?
      (q/stroke 0 255 0)  ;; Green for active
      (q/stroke 255 0 0)) ;; Red for inactive
    (q/ellipse x y 54 34)))

(defn draw-connection [x1 y1 x2 y2 active?]
  (q/stroke-weight 2)
  (if active?
    (q/stroke 0 255 0)  ;; Green for active
    (q/stroke 0))       ;; Black for inactive
  (q/line x1 y1 x2 y2))

;; Quil sketch logic
(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  @plc-state)

(defn update-state [state]
  ;; Rung 1: X0 AND X1 -> Y0
  (in :x0)
  (and-bit :x1)
  (out :y0)

  ;; Rung 2: X2 -> Timer T0 -> Y1
  (in :x2)
  (timer-on :t0)
  (out :y1)

  ;; Rung 3: X3 OR X4 -> Y3
  (in :x3)
  (or-bit :x4)
  (out :y3)

  ;; Rung 4: X5 SET/RESET -> Y2 (Latch)
  (in :x5)
  (latch :x5 :x4 :y2)  ;; X5 sets, X4 resets

  @plc-state)

(defn draw-state [state]
  (q/background 240)

  ;; Rung 1: X0 AND X1 -> Y0
  (draw-contact :x0 100 100)
  (draw-connection 50 100 100 100 (digital-read :x0))
  (draw-contact :x1 200 100)
  (draw-connection 150 100 200 100 (and (digital-read :x0) (digital-read :x1)))
  (draw-coil :y0 300 100)
  (draw-connection 250 100 300 100 (get-in @plc-state [:outputs :y0]))
  (draw-connection 350 100 400 100 (get-in @plc-state [:outputs :y0]))

  ;; Rung 2: X2 -> T0 -> Y1
  (draw-contact :x2 100 160)
  (draw-connection 50 160 100 160 (digital-read :x2))
  (q/stroke 0)
  (q/text "T0" 200 160)
  (draw-connection 150 160 250 160 (>= (- (millis) (get-in @plc-state [:timers :t0 :state] 0)) 1000))
  (draw-coil :y1 300 160)
  (draw-connection 250 160 300 160 (get-in @plc-state [:outputs :y1]))
  (draw-connection 350 160 400 160 (get-in @plc-state [:outputs :y1]))

  ;; Rung 3: X3 OR X4 -> Y3
  (draw-contact :x3 100 220)
  (draw-connection 50 220 100 220 (digital-read :x3))
  (draw-contact :x4 200 220)
  (draw-connection 150 220 200 220 (or (digital-read :x3) (digital-read :x4)))
  (draw-coil :y3 300 220)
  (draw-connection 250 220 300 220 (get-in @plc-state [:outputs :y3]))
  (draw-connection 350 220 400 220 (get-in @plc-state [:outputs :y3]))

  ;; Rung 4: X5 SET/RESET -> Y2 (Latch)
  (draw-contact :x5 100 280)
  (draw-connection 50 280 100 280 (digital-read :x5))
  (q/stroke 0)
  (q/text "LATCH" 200 280)
  (draw-contact :x4 200 300)  ;; X4 as reset below
  (draw-connection 150 280 250 280 (get-in @plc-state [:latches :y2]))
  (draw-coil :y2 300 280)
  (draw-connection 250 280 300 280 (get-in @plc-state [:outputs :y2]))
  (draw-connection 350 280 400 280 (get-in @plc-state [:outputs :y2]))

  ;; Power rails
  (q/stroke 0)
  (q/line 50 100 50 280)
  (q/line 400 100 400 280)

  ;; Instructions
  (q/fill 0)
  (q/text-align :left :top)
  (q/text "Click contacts to toggle" 10 10)
  (q/text "Rung 1: X0 AND X1 -> Y0" 10 30)
  (q/text "Rung 2: X2 -> T0 -> Y1" 10 50)
  (q/text "Rung 3: X3 OR X4 -> Y3" 10 70)
  (q/text "Rung 4: X5 SET, X4 RESET -> Y2" 10 90))

(defn mouse-clicked [state event]
  (let [x (:x event)
        y (:y event)]
    (doseq [[pin px py] [[:x0 100 100] [:x1 200 100] [:x2 100 160]
                         [:x3 100 220] [:x4 200 220] [:x5 100 280]]]
      (when (and (<= (- px 25) x (+ px 25))
                 (<= (- py 15) y (+ py 15)))
        (swap! plc-state update-in [:inputs pin] not))))
  @plc-state)

(defn -main []
  (q/sketch
   :title "Enhanced PLC Simulator with Quil"
   :size [450 350]
   :setup setup
   :update update-state
   :draw draw-state
   :mouse-clicked mouse-clicked
   :features [:keep-on-top]
   :middleware [m/fun-mode]))

;; Run simulation
(-main)

;; clojure -M -m plc-lib.core