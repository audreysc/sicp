(defn square [x] (* x x))

;;; 1.25

(defn fast-expt [b n]  
  (cond (= n 0) 1  
        (even? n) (square (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (- n 1)))))

(defn expmod [base exp m]
  (println base exp m)
  (cond (= exp 0) 1
        (even? exp) (mod (square (expmod base (/ exp 2) m))
                         m)
        :else (mod (* base (expmod base (- exp 1) m))
                   m)))

(defn expmod2 [base exp m]  
  (rem (fast-expt base exp) m))

;;1.26

(defn expmod3 [base exp m]
  (cond (= exp 0) 1
        (even? exp) (mod (* (expmod base (/ exp 2) m)
                            (expmod base (/ exp 2) m)) m)
        :else (mod (* base (expmod base (- exp 1) m)) m)))

(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (rand-int (- n 1)))))

(defn fermat-test-all [n x]
  (defn try-it [a]
    (println n a)
    (= (expmod a n n) a))
  (try-it x))

(defn fast-prime? [n times]
  (cond (= times 0) true
        (fermat-test-all n times) (fast-prime? n (- times 1))
        :else false))

(defn prime? [n]
  (= n (smallest-divisor n)))

; returns false if Carmichael number

(defn Carmichael [n]
  (= (fast-prime? n (- n 1)) (prime? n)))

; miller-rabin-test-times (exercise 1.28)
; returns true if prime, false if not prime

(defn trivial [r m]
  (if (or (= r 1) (= r (- m 1)))
        r
        (if (= (rem (square r) m) 1)
            0
            r)))

(defn expmod_triv [base exp m]
  (println base exp m)
  (cond (= exp 0) 1
        (even? exp) (mod (square (trivial (expmod base (/ exp 2) m) m))
                         m)
        :else (mod (* base (expmod base (- exp 1) m))
                   m)))

(defn miller-rabin-test [n x]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it x))

(defn miller-rabin-test-times [n times]
  (cond (= times 0) true
    (miller-rabin-test n times)(miller-rabin-test-times n (- times 1))
    :else false))
