(ns prj.bond
  (:import
   (javax.swing JFrame JPanel) 
   (java.awt.event KeyListener)
   (java.awt Color Graphics) 
   (java.awt.image BufferedImage)) 
  (:use clojure.contrib.import-static))

(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)

(def screen-x 600)
(def screen-y 600)
(def screen {:x screen-x :y screen-y})
(def growth-per-food-eaten 10)
(def damage-per-pred-hit -1)
(def step-time 30)
(def steps-per-second (/ 1000 step-time))
(def key-push-acceleration 0.5)
(def max-blob-acceleration 0.25)
(def start-size 25)
(def init-blobs-amt 1)
(def blob-size 40)
(def init-seconds-between-new-blobs 5)
(def interval-length 200)
(def rate-increase-per-interval 0.85)
(def chance-new-blob-is-food 0.08)
(def friction 0.012)
(def circle-decay-rate 1)
(def min-size 5)

(defn square [n] (* n n))

(defn pythag [a b] (Math/sqrt (+ (square a) (square b))))

(defn rand-between [min max]
  (+ (rand (- max min)) min))

(defn rand-chance [odds]
  (< (rand) odds))

(defn rand-from [coll] (nth coll (rand (count coll))))

(defn rand-from-map [map]
  (let [total-options (apply + (vals map))
	rnd (rand-int total-options)]
    (first (vals map))))

(defn unix-time []
  (Math/round (Math/floor (/ (System/currentTimeMillis) 100))))

(defn elapsed-time [game]
  (- (unix-time) (game :start-time)))

(defn split-by [pred coll]
  (loop [true-list '() false-list '() tail coll]
    (let [head (first tail)]
      (cond (empty? tail) (list true-list false-list)
	    (pred head) (recur (cons head true-list) false-list (rest tail))
	    :else (recur true-list (cons head false-list) (rest tail))))))


(defn seconds-between-new-blobs [game]
  (* init-seconds-between-new-blobs
     (Math/pow 
      rate-increase-per-interval
      (Math/floor (/ (elapsed-time game) interval-length)))))

(defn new-blobs-per-step [game]
  (/ 1 (* (seconds-between-new-blobs game) steps-per-second)))

(defn new-window [width height]
  (let [frame (JFrame.)]
    (.pack frame)
    (.setSize frame width height)
    (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
    frame))

(defn new-image []
  (BufferedImage. screen-x screen-y (BufferedImage/TYPE_INT_ARGB)))

(defn paint-circle 
  ([g circle] (paint-circle g circle Color/BLUE))
  ([g circle color]
  (let [x (circle :x) y (circle :y) size (circle :size)]
    (doto g
      (.setColor color)
      (.fillOval (- x (/ size 2)) (- y (/ size 2)) size size)))))

(defn paint-info [g game high-score]
  (.setColor g Color/GREEN)
  (.drawString g (str "Size: " (- (int ((game :circle) :size)) min-size)
		      "  Score: " (elapsed-time game)
		      " Hi-score: " high-score)
	       20 20))

(defn blob-colour [blob]
  (if (= (blob :type) :pred) 
    (Color. (blob :colour) 0 0)
    (Color. 0 (blob :colour) 0)))

(defn paint-game [g game high-score]
  (let [circle (game :circle)
	buffer (new-image)
	graphics (.getGraphics buffer)]
    (doto graphics
      (.setColor Color/BLACK)
      (.fillRect 0 0 screen-x screen-y)
      (paint-circle circle)
      (paint-info game high-score))
    (doseq [blob (game :blobs)] 
      (paint-circle graphics blob (blob-colour blob)))
    (.drawImage g buffer 0 0 nil)))

(defn key-pressed [e keys-atom]
  (swap! keys-atom conj (.getKeyCode e)))

(defn key-released [e keys-atom]
  (swap! keys-atom (fn [_] [])))

(defn key-typed [e key-code-atom])

(defn get-key-direction
  "Gets a keyword that describes the direction
   associated with a given key code."
  [key-code]
  (cond 
    (= key-code VK_LEFT) :left
    (= key-code VK_RIGHT) :right
    (= key-code VK_UP) :up
    (= key-code VK_DOWN) :down
    true nil))

(defn new-panel [game-atom keys-atom high-score-atom]
  (let [panel (proxy [JPanel KeyListener] [] 
		(paint [g] (paint-game g @game-atom @high-score-atom))
		(keyPressed [e] (key-pressed e keys-atom))
		(keyReleased [e] (key-released e keys-atom))
		(keyTyped [e] (key-typed e keys-atom)))]
    (doto panel 
      (.setFocusable true) ; won't generate key events without this
      (.addKeyListener panel))))

(defn new-circle []
  {:x (/ screen-x 2) 
   :y (/ screen-y 2) 
   :dx 0 
   :dy 0 
   :size start-size
   :max-size start-size})

(defn new-blob []
  (let [rnd (rand-between 0.2 1)]
    {:x 0
     :y 0
     :dx 0
     :dy 0
     :type (if (rand-chance chance-new-blob-is-food) :food :pred)
     :colour (int (* 255 (- 1.2 rnd)))
     :acceleration (* (- 1.2 rnd) max-blob-acceleration)
     :size (* blob-size rnd)}))

(defn new-blobs [n]
  (map (fn [_] (new-blob)) (range n)))

(defn new-game []
  {:circle (new-circle) 
   :blobs (new-blobs init-blobs-amt) 
   :start-time (unix-time)})

(defn nudge-to-0 [n ratio]
  (if (< (Math/abs n) 0.05) 0
      (* ratio n)))	
      
(defn displace [circle]
  (assoc circle 
    :x (mod (+ (circle :x) (circle :dx)) screen-x) 
    :y (mod (+ (circle :y) (circle :dy)) screen-y)))

(defn circle-acceleration [circle] key-push-acceleration)
;  (/ key-push-acceleration (Math/sqrt (/ (circle :size) 10))))

(defn key-input-circle [circle keys-atom]
  (let [dirs (map get-key-direction @keys-atom)
	dx (circle :dx)
	dy (circle :dy)]
    (assoc circle
      :dy (cond (some #{:up} dirs) (- dy (circle-acceleration circle))
		(some #{:down} dirs) (+ dy (circle-acceleration circle))
		:else dy)
      :dx (cond (some #{:left} dirs) (- dx (circle-acceleration circle))
		(some #{:right} dirs) (+ dx (circle-acceleration circle))
		:else dx))))

(defn deceleration-ratio [obj]
  (- 1 friction))

(defn decelerate [obj]
  (assoc obj 
    :dx (nudge-to-0 (obj :dx) (deceleration-ratio obj))
    :dy (nudge-to-0 (obj :dy) (deceleration-ratio obj))))

(defn update-max-size [obj]
  (let [size (obj :size)]
    (if (> size (obj :max-size))
      (assoc obj :max-size size)
      obj)))

(defn grow-by [object n]
  (assoc object :size (* circle-decay-rate (+ n (object :size)))))

(defn growth-for-eating [blobs]
  (apply + (map #(if (= (% :type) :food) 
		   growth-per-food-eaten 
		   damage-per-pred-hit)
		blobs)))

(defn step-circle [circle keys-atom blobs-eaten]
  (-> circle 
      (displace) 
      (key-input-circle keys-atom) 
      (decelerate)
      (update-max-size)
      (grow-by (growth-for-eating blobs-eaten))))

(defn is-collision? [circle1 circle2]
  (let [dx (- (circle1 :x) (circle2 :x))
	dy (- (circle1 :y) (circle2 :y))
	distance (pythag dy dx)]
    (< distance (/ (+ (circle1 :size) (circle2 :size)) 2))))

(defn screen-distance [obj1 obj2 axis]
  (let [raw-distance (- (obj1 axis) (obj2 axis))
	screen-size (screen axis)
	half-screen (/ screen-size 2)]
    (cond (> raw-distance half-screen) (- raw-distance screen-size)
	  (< raw-distance (- half-screen)) (+ raw-distance screen-size)
	  :else raw-distance)))

(defn accelerate-towards [obj target]
  (let [xdiff (screen-distance obj target :x)
	ydiff (screen-distance obj target :y)
	acc (obj :acceleration)]
    (assoc obj 
      :dx (if (< 0 xdiff) (- (obj :dx) acc) (+ (obj :dx) acc))
      :dy (if (< 0 ydiff) (- (obj :dy) acc) (+ (obj :dy) acc)))))

(defn accelerate-away [obj target]
  (let [xdiff (screen-distance obj target :x)
	ydiff (screen-distance obj target :y)
	acc (obj :acceleration)]
    (assoc obj 
      :dx (if (> 0 xdiff) (- (obj :dx) acc) (+ (obj :dx) acc))
      :dy (if (> 0 ydiff) (- (obj :dy) acc) (+ (obj :dy) acc)))))

(defn step-blob [blob game]
  (let [blob (-> blob (displace) (decelerate))]
    (if (= (blob :type) :food)
      (accelerate-away blob (game :circle))
      (accelerate-towards blob (game :circle)))))

(defn spawn-blobs [blobs game]
  (if (rand-chance (new-blobs-per-step game)) 
    (cons (new-blob) blobs)
    blobs))0

(defn step-blobs [blobs game]
  (if (empty? blobs) (new-blobs init-blobs-amt)
      (map #(step-blob % game) (spawn-blobs blobs game))))

(defn game-complete? [game]
  (<= (int ((game :circle) :size)) min-size))

(defn step [game keys-atom]
  (let [[eaten-blobs blobs] 
	(split-by #(is-collision? % (game :circle)) (game :blobs))]
    (assoc game 
      :circle (step-circle (game :circle) keys-atom eaten-blobs)
      :blobs (step-blobs blobs game))))

(let [game (new-game)
      game-atom (atom game)
      keys-atom (atom [])
      high-score-atom (atom 0)
      window (new-window screen-x screen-y)
      panel (new-panel game-atom keys-atom high-score-atom)
      high-score 0]
  (.add window panel)
  (.show window) 
  (loop []
    (Thread/sleep step-time)
    (swap! game-atom step keys-atom)
    (swap! high-score-atom max (elapsed-time @game-atom))
    (.repaint panel)
    (if (game-complete? @game-atom) 
      (do
	(Thread/sleep 3000)
	(swap! game-atom (fn [_] (new-game)))))
    (recur)))

