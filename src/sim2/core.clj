(ns sim2.core
  (:require [clojure.math :as math]
            [quil.core :as q]
            [quil.middleware :as m]))

(def size 500) ;; gotta be a square
(def width  size) 
(def height size) 

(defn coordinate-colour [x y]
  (let [[center-x center-y] [(/ width 2) (/ height 2)]
        ang (+ (math/atan2 (- y center-y) (- x center-x)) math/PI)
        mag (Math/sqrt (+  (Math/pow (- x center-x) 2) (Math/pow (- y center-y) 2)))
        h (/ ang (* math/PI 2))
        s (/ mag (/ size 2))
        v 1.0]
    (map #(* 255 %) [h s v])))

;; Energy

(def starting-power 500000000000) ;; pJ
(def e-b 50000) ;; power required to transmit or receive 1 bit (pJ)
(def e-amp 100) ;; power required to transmit 1 bit to the required distance (pJ)

(defn transmission-energy [number-of-bits distance]
  (+ (* e-b number-of-bits) (* e-amp number-of-bits (* distance distance))))

(defn receiving-energy [number-of-bits]
  (* e-b number-of-bits))

(def types [:cluster-head :cluster-member :cluster-not :dead])

(def node-size 25)
(def sink-size (* node-size 2))
(def sink-colour (coordinate-colour (/ width 2) (/ height 2)))

(def P 0.05)    ;; desired percentage of cluster heads
(def G (/ 1 P)) ;; amount of time a node will wait before becoming the cluster head again


;; Node

(defn new-node [x y]
  {:x x, :y y
   :type :cluster-member
   :g 0
   :energy starting-power
   :colour (coordinate-colour x y)})

(defn is-type? [node type]
  (= (:type node) type))

(defn draw-sink [sink]
  (q/with-fill sink-colour
      (q/rect (:x sink) (:y sink) sink-size sink-size)))

(defn draw-node [node]
  (do 
    (case (:type node) ;; draw shape according to class
      :cluster-member (q/ellipse 0 0 node-size node-size)
      :cluster-head   (q/rect    (/ node-size -2) (/ node-size -2) node-size node-size)
      :cluster-not    (q/triangle (/ node-size -2) (/ node-size 2), 0 (/ node-size -2), (/ node-size  2) (/ node-size 2)))
    (q/with-fill [0 0 0] ;; draw battery percent
      (q/text (format " %.0f%%" (* (float (/ (:energy node) starting-power)) 100))
              (int (/ node-size -2)) 0)
      (q/text (str "G " (:g node))
              (int (/ node-size -2)) (int (/ node-size -2)))))) 

(defn distance-between-nodes [node-a node-b]
  (let [delta (fn [key] (apply #(Math/pow (- %1 %2) 2) (map key [node-a node-b])))
        x-delta (delta :x)
        y-delta (delta :y)]
    (Math/sqrt (+ x-delta y-delta))))

(defn find-cluster-head [nodes node]
  (let [cluster-heads (filter #(= (:type %) :cluster-head) nodes)]
    (first (sort-by #(distance-between-nodes % node) cluster-heads))))

(defn find-cluster-members [nodes node]
  (do
    (assert (is-type? node :cluster-head))
    (filter #(= (find-cluster-head nodes %) node) (filter #(is-type? % :cluster-member) nodes))))

(defn cluster-network [nodes]
  (let [cluster-heads   (filter #(is-type? % :cluster-head  ) nodes)
        cluster-members (filter #(is-type? % :cluster-member) nodes)
        cluster-nots    (filter #(is-type? % :cluster-not   ) nodes)]
    {:cluster-nots cluster-nots
     :cluster-orphans (if (empty? cluster-heads) cluster-members []) ;; edge case where no one is elected
     :clusters (map (fn [ch] {:cluster-head ch
                              :cluster-members (find-cluster-members nodes ch)}) cluster-heads)}))

(defn get-node-colour [nodes node]
  (case (:type node)
    :cluster-head   (:colour node)
    :cluster-member (or (:colour (find-cluster-head nodes node)) (:colour node)) ;; [0 0 255])
    :cluster-not    sink-colour))

;;(defn get-node-colour [nodes node]
;;  (:colour node))

(defn create-random-nodes [n]
  (repeatedly n #(new-node (rand width) (rand height))))

(defn create-circle-nodes [n radius]
  (for [i (range n)]
    (let [angle (* 2 (Math/PI) (/ i n))
          x     (+ (/ width  2) (* radius (Math/cos angle)))
          y     (+ (/ height 2) (* radius (Math/sin angle)))]
      (new-node x y))))

;; Messages (How much energy each message costs to send)

(def message-id-size 32) ;; bits for node ids
(def data-size 2000) ;; bits for data collected

(def round-length 60) ;; new round every 60s
(def advertisment-phase-length (* round-length 0.10)) ;; 10% of round is for advertisment
(def cluster-head-listen-time (- round-length advertisment-phase-length)) ;; Cluster head listens always except when its advertising

;; How far do we have to broadcast a message?
;;(defn required-broadcast-distance [node [x-min x-max] [y-min y-max]]
;;  (let [[x y] [(:x node) (:y node)]]
;;        (apply max (map #(Math/abs %) [(- x x-min) (- y y-min) (- x x-max) (- y y-max)])))) ;; find what direction it has to broadcast the farthest in

(defn required-broadcast-distance [node [x-min x-max] [y-min y-max]]
  (let [points [{:x x-min :y y-min}
                {:x x-min :y y-max}
                {:x x-max :y y-min}
                {:x x-max :y y-max}]]
        (apply max (map #(distance-between-nodes node %) points))))

(defn cluster-broadcast-range [cluster cluster-head]
  (let [xs (map :x (conj cluster cluster-head))
        ys (map :y (conj cluster cluster-head))]
    (required-broadcast-distance cluster-head
                                 [(apply min xs) (apply max xs)]
                                 [(apply min ys) (apply max ys)])))

(defn send-message-to-node [node-start node-end message-size]
  (transmission-energy message-size (distance-between-nodes node-start node-end)))
                                
;; Cluster Head Messages

;; Send a message to all nodes declaring that you are a cluster head
(defn nominate-cluster-head-message [cluster-head]
  (let [message-size message-id-size
        distance (required-broadcast-distance cluster-head [0 width] [0 height])]
    (transmission-energy message-size distance)))


;; Send TDMA schedule to all cluster members
(defn broadcast-tdma-schedule-message [cluster cluster-head]
  (let [cluster-size (count cluster)
        message-size (* message-id-size cluster-size)
        distance (cluster-broadcast-range cluster cluster-head)]
    (transmission-energy message-size distance)))

(defn listen-for-cluster-member-messages []
  (* (receiving-energy message-id-size) cluster-head-listen-time))

;; Send fused data to the sink
(defn send-data-to-sink-message [sink cluster-head]
    (send-message-to-node sink cluster-head data-size))


(defn cluster-head-energy-spent [sink cluster cluster-head]
  (+ (nominate-cluster-head-message cluster-head) ;; nominate self
     (broadcast-tdma-schedule-message cluster cluster-head) ;; broadcast tdma schedule to all members
     (listen-for-cluster-member-messages) ;; list for cluster member data
     (send-data-to-sink-message sink cluster-head))) ;; finally send data to sink 

;; Cluster Member Messages

;; Listen for cluster head nomination messages
(defn listen-for-cluster-head-nomination-message []
  (* (receiving-energy message-id-size) advertisment-phase-length))

;; Inform cluster that you're joining
(defn declare-cluster-membership-message [cluster-member cluster-head]
  (send-message-to-node cluster-member cluster-head message-id-size))

;; Send Data to Cluster Head
(defn send-data-to-cluster-head-message [cluster-member cluster-head]
  (send-message-to-node cluster-member cluster-head data-size))

(defn cluster-member-energy-spent [cluster-head cluster-member]
  (+ (listen-for-cluster-head-nomination-message) ;; list for advertisment messages
     (declare-cluster-membership-message cluster-member cluster-head)
     (send-data-to-cluster-head-message cluster-member cluster-head)))

(defn cluster-orphan-energy-spent [cluster-member] ;; special edge case when there arent any cluster heads
  (listen-for-cluster-head-nomination-message))

;; Cluster Not Messages
(defn cluster-not-energy-spent [sink cluster-not]
  (+ (listen-for-cluster-head-nomination-message)
     (send-data-to-sink-message sink cluster-not)))

;; Election

(defn eligible-cluster-head? [node]
  (<= (:g node) 0))

(defn cluster-head-threshold [node round]
  (if (eligible-cluster-head? node) (/ P (- 1 (* P (mod round (Math/round (/ 1 P)))))) ;; calculate prob of becoming a cluster head
      0))

(defn become-cluster-head? [node round]
  (<= (rand) (cluster-head-threshold node round))) ;; do threshold test to decide whether we're gonna be a cluster head

(defn shouldnt-cluster? [nodes sink node]
  (let [cluster-head    (find-cluster-head nodes node)
        d-node->cluster (if cluster-head (distance-between-nodes node cluster-head) Double/POSITIVE_INFINITY)
        d-node->sink    (distance-between-nodes node sink)]
    (> (+ (/ e-b e-amp) (* d-node->cluster d-node->cluster))
       (* d-node->sink d-node->sink))))

(defn do-election [node round]
  (if (and (become-cluster-head? node round)
           (<= (:g node) 0))
    (-> node
        (assoc :g (dec (Math/round G))) ;; set self election cool down
        (assoc :type :cluster-head)) ;; we are now a cluster head
    (assoc node :type :cluster-member))) ;; 

(defn do-not-cluster [nodes sink node]
  (if (and (not (is-type? node :cluster-head)) (shouldnt-cluster? nodes sink node))
    (assoc node :type :cluster-not)
    node))

(defn spend-energy [node cost-fn]
  (update node :energy #(- % (cost-fn node))))

(defn do-cluster-energy-calculation [sink {:keys [cluster-head cluster-members]}]
  {:cluster-head    (spend-energy cluster-head #(cluster-head-energy-spent sink cluster-members %))
   :cluster-members (map (fn [cluster-member] (spend-energy cluster-member #(cluster-member-energy-spent cluster-head %))) cluster-members)})

;; reset g if its time (this keeps the distribution of cluster heads even)
(defn do-update-g [state]
  (let [reset-ch? (zero? (mod (:round state) G))] ;; is it time to reset cluster heads?
        (update state :nodes (fn [nodes] (map #(update % :g (fn [g] (if reset-ch? 0 g))) nodes))))) 

(defn do-count-messages-sent [state]
  (let [{:keys [cluster-nots cluster-orphans clusters]} (cluster-network (:nodes state))
        cluster-nots    (count cluster-nots)
        cluster-heads   (count clusters)
        cluster-members (apply + (map #(count (:cluster-members %)) clusters))
        messages-sent   (+ cluster-nots cluster-heads cluster-members)]
    (update state :messages-sent #(+ % messages-sent))))

;;(defn do-count-messages-sent [state]
;;  (let [nodes (:nodes state)
;;        cluster-nots   (count (filter #(is-type? % :cluster-not) nodes))
;;        cluster-heads? (some #(is-type? % :cluster-head) nodes)
;;        clusters       (if cluster-heads? (- (count nodes) cluster-nots) 0)
;;        messages-sent  (+ cluster-nots clusters)]
;;  (update state :messages-sent #(+ % messages-sent))))

;; have nodes decide whether or not to elect themselves
(defn do-elections [state]
  (update state :nodes (fn [nodes] (map #(do-election % (:round state)) nodes)))) 
  
;; have nodes uncluster
(defn do-not-clusters [state]
  (if (:uncluster? state)
    (update state :nodes (fn [nodes] (map #(do-not-cluster nodes (:sink state) %) nodes))) ;; nodes that a close enough to sink shouldnt cluster
    state)) ;; we don't want to uncluster
  

;; remove energy based on work done by node
(defn do-energy-calculation [state]
  (let [sink (:sink state)
        {:keys [cluster-nots cluster-orphans clusters]} (cluster-network (:nodes state))
        cluster-nots*    (map (fn [node] (spend-energy node #(cluster-not-energy-spent sink %))) cluster-nots)
        clusters*        (map (fn [cluster] (do-cluster-energy-calculation sink cluster)) clusters)
        cluster-orphans* (map (fn [node] (spend-energy node #(cluster-orphan-energy-spent %))) cluster-orphans)
        nodes*        (concat cluster-nots*
                              cluster-orphans*
                              (map :cluster-head clusters*)
                              (apply concat (map :cluster-members clusters*)))]
    (assoc state :nodes nodes*)))

;; remove nodes that have less than zero energy
(defn do-remove-dead-nodes [state]
  (update state :nodes (fn [nodes] (filter #(> (:energy %) 0) nodes))))

(defn do-round [state]
  (-> state
      (update :round inc)
      (do-update-g)
      (do-elections)
      (do-not-clusters)
      (do-count-messages-sent)
      (do-energy-calculation)
      (do-remove-dead-nodes)
      (update :nodes vec))) ;; keep nodes as vec

(defn network-lifetime [begining-state]
  (loop [state begining-state]
    (if (empty? (:nodes state)) (select-keys state [:round :messages-sent])
        (recur (do-round state)))))

(def initial-state
  {:round 0
   :messages-sent 0
   :uncluster? false
    :sink {:x (- (/ width  2) (/ sink-size 2))
           :y (- (/ height 2) (/ sink-size 2))}
   :nodes (concat (create-circle-nodes 45 (* width 0.45))
                  (create-circle-nodes 30 (* width 0.30))
                  (create-circle-nodes 15 (* width 0.15)))})
;;  :nodes (create-random-nodes 100)})

(defn setup []
  (q/frame-rate 10)
  (q/color-mode :hsb)
  (do-round initial-state))

(defn update-state [state]
  (do-round state))

(defn key-pressed [state event]
  (if (= (:key event) :c)
    (update state :uncluster? not)))
;;  (if (= (:key event) :space)
;;    (do-round state)
;;    state))

(defn draw-state [state]
  (q/background 240)
  ;; Draw Nodes
  (doseq [node (:nodes state)]
    (q/with-translation [(:x node) (:y node)]
      (q/with-fill (get-node-colour (:nodes state) node)
        (draw-node node))))
  ;; Draw Sink
  (draw-sink (:sink state))
  (q/with-translation [(get-in state [:sink :x]) (get-in state [:sink :y])]
    (q/with-fill [0 0 0]
      (do
        (q/text (str "R " (:round state)) 0 15)
        (q/text (str "N " (:messages-sent state)) 0 30)))))
    ;;(q/text (str "Round " (:round state)) (get-in state [:sink :x]) (get-in state [:sink :y]))))

(q/defsketch sim2
  :title "sim"
  :size [width height]
  :setup setup
  :update update-state
  :key-pressed key-pressed
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
