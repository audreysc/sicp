;21-26

(defn square [n] (* n n))

(defn divides? [a b]
  (= (mod b a) 0))

(defn next [n]
	(cond (= n 2) 3
		:else (+ n 2)))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (next test-divisor))))

(defn smallest-divisor [n]
  (find-divisor n 2))

;;; PROBLEM 1.22

(defn prime? [n]
	(= n (smallest-divisor n) n))

(defn current-time [] 
	(System/nanoTime))

(defn report-prime [n elapsed-time]
	(println (str n " *** " elapsed-time " nanoseconds")) true)

(defn start-prime-test [n start-time]
	(if (prime? n)
		(report-prime n (- (current-time) start-time))
		false))

(defn timed-prime-test [n]
	(start-prime-test n (current-time)))

(defn search-odds [x]
	(cond (timed-prime-test x) x
		:else (search-odds (+ x 2))))

(defn find-c-primes [above c] 
	(cond (even? above) (find-c-primes (inc above) c)
		:else (find-c-primes (search-odds above) (inc c))))



