(ns sim2.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def width 500)
(def height 500)

;; Energy
(def starting-power 500000000000) ;; pJ
(def e-b 50000) ;; power required to transmit or receive 1 bit (pJ)
(def e-amp 100) ;; power required to transmit 1 bit to the required distance (pJ)
(def packet-size 2000) ;; size of a message (bits)

(defn transmission-energy [number-of-bits distance]
  (+ (* e-b number-of-bits) (* e-amp number-of-bits (* distance distance))))

(defn receiving-energy [number-of-bits distance]
  (* e-b number-of-bits))


(def types [:cluster-head :cluster-member :cluster-not :dead])

(def node-size 15)
(def sink-size (* node-size 2))
(def sink-colour [0 255 0])

(def P 0.05)    ;; desired percentage of cluster heads
(def G (/ 1 P)) ;; amount of time a node will wait before becoming the cluster head again

;; Node

(defn new-node [x y]
  {:x x, :y y
   ;;:type :cluster-member
   :type :cluster-not
   :g 0
   :colour [(rand 255) (rand 255) (rand 255)]}) ;; random colour

(defn draw-sink [sink]
  (q/with-fill sink-colour
      (q/rect (:x sink) (:y sink) sink-size sink-size)))

(defn draw-node [node]
  (case (:type node)
    :cluster-member (q/ellipse 0 0 node-size node-size)
    :cluster-head   (q/rect    0 0 node-size node-size)
    :cluster-not    (q/triangle (/ node-size -2) 0
                                (/ node-size  2) 0
                                0 node-size)))
  

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
    :cluster-head   (:colour node)
    :cluster-member (:colour (find-cluster-head nodes node))
    :cluster-not    sink-colour))
  

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

;; FIXME: This math isnt actually true to the paper (remeber the times 2)
(defn shouldnt-cluster? [nodes sink node]
  (let [cluster-head  (find-cluster-head nodes node)
        ch-distance   (if-not cluster-head 0 (distance-between-nodes node cluster-head)) ;; TODO: Remove this if its wrong
        sink-distance (distance-between-nodes node sink)]
    (< sink-distance ch-distance)))

(defn do-election [node round]
  (if (and (become-cluster-head? node round)
           (<= (:g node) 0))
    (-> node
        (assoc :g (dec (Math/round G))) ;; set self election cool down
        (assoc :type :cluster-head)) ;; we are now a cluster head
    (assoc node :type :cluster-member))) ;; 

(defn do-not-cluster [nodes sink node]
  (if (shouldnt-cluster? nodes sink node)
    (assoc node :type :cluster-not)
    node))
  

(defn do-round [state]
  (let [round (:round state)
        reset-ch? (zero? (mod round G))] ;; is it time to reset cluster heads?
    (-> state
        (update :round inc)
        (update :nodes (fn [nodes] (map #(update % :g (fn [g] (if reset-ch? 0 g))) nodes))) ;; reset g if its time (this keeps the distribution of cluster heads even)
        (update :nodes (fn [nodes] (map #(do-election % round) nodes))) ;; have nodes decide whether or not to elect themselves
        (update :nodes (fn [nodes] (map #(do-not-cluster nodes (:sink state) %) nodes)))))) ;; nodes that a close enough to sink shouldnt cluster
                                                         


(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (do-round
   {:round 0
    :sink {:x (- (/ width  2) (/ sink-size 2))
           :y (- (/ height 2) (/ sink-size 2))}
    :nodes (create-random-nodes 100)}))

(defn update-state [state]
  state)

(defn key-pressed [state event]
  (if (= (:key event) :space)
    (do-round state)
    state))

(defn draw-state [state]
  (q/background 240)
  ;; Draw Nodes
  (doseq [node (:nodes state)]
    (q/with-translation [(:x node) (:y node)]
      (q/with-fill (get-node-colour (:nodes state) node)
        (draw-node node))))
  ;; Draw Sink
  (draw-sink (:sink state)))

(q/defsketch sim2
  :title "sim"
  :size [width height]
  :setup setup
  :update update-state
  :key-pressed key-pressed
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
