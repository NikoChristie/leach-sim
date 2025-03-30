(ns sim2.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def width 500)
(def height 500)

;; Energy
(def e-b 50) ;; power required to transmit or receive 1 bit
(def e-amp 100) ;; power required to transmit 1 bit to the required distance

(defn transmission-energy [number-of-bits distance]
  (+ (* e-b number-of-bits) (* e-amp number-of-bits (* distance distance))))

(defn receiving-energy [number-of-bits distance]
  (* e-b number-of-bits))

(def initial-energy 1)

(def types [:cluster-head :cluster-member :dead])

(def node-size 15)

(def P 0.05)    ;; desired percentage of cluster heads
(def G (/ 1 P)) ;; amount of time a node will wait before becoming the cluster head again

;; Node

(defn new-node [x y]
  {:x x, :y y
   :type :cluster-member
   :g 0
   :colour [(rand 255) (rand 255) (rand 255)]}) ;; random colour

(defn draw-node [node]
  (let [draw-fn (case (:type node)
                  :cluster-member q/ellipse
                  :cluster-head   q/rect)]
    (draw-fn 0 0 node-size node-size)))

(defn distance-between-nodes [node-a node-b]
  (let [delta (fn [key] (apply #(Math/pow (- %1 %2) 2) (map key [node-a node-b])))
        x-delta (delta :x)
        y-delta (delta :y)]
    (Math/sqrt (+ x-delta y-delta))))

(defn find-cluster-head [nodes node]
  (let [cluster-heads (filter #(= (:type %) :cluster-head) nodes)]
    (first (sort-by #(distance-between-nodes % node) cluster-heads))))

(defn get-node-colour [nodes node]
  (case (:type node)
    :cluster-head    (:colour node)
    :cluster-member (:colour (find-cluster-head nodes node))))

(defn create-random-nodes [n]
  (repeatedly n #(new-node (rand width) (rand height))))

;; Election

(defn eligible-cluster-head? [node]
  (<= (:g node) 0))

(defn cluster-head-threshold [node round]
  (if (eligible-cluster-head? node) (/ P (- 1 (* P (mod round (Math/round (/ 1 P)))))) ;; calculate prob of becoming a cluster head
      0))

(defn become-cluster-head? [node round]
  (<= (rand) (cluster-head-threshold node round))) ;; do threshold test to decide whether we're gonna be a cluster head

(defn do-election [node round]
  (if (and (become-cluster-head? node round)
           (<= (:g node) 0))
    (-> node
        (assoc :g (dec (Math/round G))) ;; set self election cool down
        (assoc :type :cluster-head)) ;; we are now a cluster head
    (assoc node :type :cluster-member))) ;; 

(defn do-round [state]
  (let [round (:round state)
        reset-ch? (zero? (mod round G))] ;; is it time to reset cluster heads?
    (-> state
        (update :round inc)
        (update :nodes (fn [nodes] (map #(update % :g (fn [g] (if reset-ch? 0 g))) nodes))) ;; reset g if its time (this keeps the distribution of cluster heads even)
        (update :nodes (fn [nodes] (map #(do-election % round) nodes)))))) ;; have nodes decide whether or not to elect themselves

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (do-round
   {:round 0
    :nodes (create-random-nodes 100)}))

(defn update-state [state]
  state)

(defn key-pressed [state event]
  (if (= (:key event) :space)
    (do-round state)
    state))

(defn draw-state [state]
  (q/background 240)
  (doseq [node (:nodes state)]
    (q/with-translation [(:x node) (:y node)]
      (q/with-fill (get-node-colour (:nodes state) node)
        (draw-node node)))))
    ;;(q/ellipse (:x node) (:y node) node-size node-size)))
;;  (q/fill (:color state) 255 255)
;;  (let [angle (:angle state)
;;        x (* 150 (q/cos angle))
;;        y (* 150 (q/sin angle))]
;;    ; Move origin point to the center of the sketch.
;;    (q/with-translation [(/ (q/width) 2)
;;                         (/ (q/height) 2)]
;;      ; Draw the circle.
;;      (q/ellipse x y 100 100))))

(q/defsketch sim2
  :title "sim"
  :size [width height]
  :setup setup
  :update update-state
  :key-pressed key-pressed
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
