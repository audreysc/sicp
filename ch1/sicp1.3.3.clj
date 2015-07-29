(set-env! :dependencies '[[org.clojure/math.numeric-tower "0.0.4"]])
 
(require '[clojure.math.numeric-tower :refer [floor ceil round]])
(require '[boot.cli :as cli])

(defn positive? [x]
  (> x 0))

(defn negative? [x]
  (< x 0))

(defn average [x y]
  (/ (+ x y) 2))

(defn search [f neg-point pos-point]  
  (let [midpoint (average neg-point pos-point)]  
    (if (close-enough? neg-point pos-point)  
        midpoint  
        (let [test-value (f midpoint)]  
          (cond (positive? test-value)  
                 (search f neg-point midpoint)  
                (negative? test-value)  
                 (search f midpoint pos-point)  
                :else midpoint)))))

(defn close-enough? [x y]  
  (< (Math/abs (- x y)) 0.001))

(defn fixed-point [f first-guess]
  (let [tolerance 0.00001]

    (defn close-enough? [v1 v2]

      (defn abs [x]
        (if (> 0 x) (- x) x))

      (< (abs (- v1 v2)) tolerance))

    (defn try-it [guess]
      (let [next-guess (f guess)]
        (if (close-enough? guess next-guess)
          next-guess
          (try-it next-guess))))
    
    (try-it first-guess)))


(defn half-interval-method [f a b]  
  (let [a-value (f a)  
        b-value (f b)]  
    (cond (and (negative? a-value) (positive? b-value))  
           (search f a b)  
          (and (negative? b-value) (positive? a-value))  
           (search f b a)  
          :else  
           (throw (Exception. "Values are not of opposite sign")))))

(half-interval-method (fn Math/sin) 2.0 4.0) 

;(fixed-point cos 1.0)

