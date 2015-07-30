
;; 2.2

(defn x-point [point] (first point))
(defn y-point [point] (second point))
(defn make-point [x y] (list x y))
(defn make-segment [p1 p2] (list p1 p2))
(defn start-segment [segment] (first segment))
(defn end-segment [segment] (second segment))

(defn midpoint-segment [line]
  (make-point 
    (/ (+ (x-point (start-segment line)) (x-point (end-segment line))) 2) 
    (/ (+ (y-point (start-segment line)) (y-point (end-segment line))) 2)))

;; 2.5

(defn exp [x n]
  (if (zero? n) 1
      (* x (exp x (dec n)))))	

(defn find-exp [n b]
  (defn inc-exp [i]
    (if (= 0 (rem n (exp b i))) 
    (inc-exp (inc i))
    (dec i)))
  (inc-exp 1))

(defn cons-int [a b] (*(exp 2 a)(exp 3 b)))
(defn car-int [c] (find-exp c 2))
(defn cdr-int [c] (find-exp c 3))

;; 2.6, use inc to test 

(def zero (fn [f] (fn [x] x)))

(defn add-1 [n] (fn [f] (fn [x] (f ((n f) x)))))

(def one
  (fn [f] (fn [x] (f x))))

(def two
  (fn [f] (fn [x] (f (f x)))))

(defn add-num [m n]
  (fn [f] (fn [x] ((m f) ((n f) x)))))

(def three (add-num one two))
