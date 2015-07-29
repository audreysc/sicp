(defn cube [x]
	(* x x x))

(defn sum-integers [a b]  
  (if (> a b)  
      0  
      (+ a (sum-integers (+ a 1) b))))

(defn sum-cubes [a b]  
  (if (> a b)  
      0  
      (+ (cube a) (sum-cubes (+ a 1) b))))

(defn pi-sum [a b]  
  (if (> a b)  
      0  
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

; sum takes as its arguments
; lower and upper bounds a and b 
; together with the procedures term and next

(defn sum [term a next-term b]
  (if (> a b)
      0  
      (+ (term a)  
         (sum term (next-term a) next-term b))))

; examples using sum from above

(defn inc [n] (+ n 1))  

(defn sum-cubes [a b]  
  (sum cube a inc b))

(defn identity [x] x)  
  
(defn sum-integers [a b]  
  (sum identity a inc b)) 

(defn pi-sum [a b]  
  (defn pi-term [x]
    (/ 1.0 (* x (+ x 2))))
  (defn pi-next [x]
    (+ x 4))  
  (sum pi-term a pi-next b))

(defn integral [f a b dx]
  (defn add-dx [x] (+ x dx))  
  (* (sum f (+ a (/ dx 2.0)) add-dx b)  
     dx))

(defn simpsons [f a b h]
	(println a b h)
	(defn next-y [x]
		(* 4 (+ x h)))
	(* (sum f a next-y b) (/ (* 2 h) 3)))

(defn simpsons-rule [f a b n]
	(simpsons f a b (/ (- b a) n)))


(defn product [term a next-term b]
  (if (> a b)
      1 
      (* (term a)  
         (product term (next-term a) next-term b))))

(defn accumulate [combiner null-value term a next-term b]
	(if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next-term a) next-term b))))

(defn product-cubes [a b] 
	(accumulate * 1 cube a inc b))

(defn factorial [b]
	(product identity 1 inc b))

(defn filtered-accumulate [combiner null-value term a next-term b fi]
	(println a b)
	(if (> a b)
      null-value
      (if (fi a)
      (combiner (term a) 
         (filtered-accumulate combiner null-value term (next-term a) next-term b fi))
      (filtered-accumulate combiner null-value term (next-term a) next-term b fi))))

(defn sum-squares-prime [a b]
	(filtered-accumulate + 0 square a inc b prime?))
