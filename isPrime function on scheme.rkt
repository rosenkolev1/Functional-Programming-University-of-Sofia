(define (isPrimeHelper startNumber currentNumber)
  (cond ((= (remainder startNumber currentNumber) 0) "Not prime")
        ((= currentNumber (quotient startNumber 2)) "Prime")
        (else (isPrimeHelper startNumber (+ currentNumber 1)))
        ))

(define (isPrime startNumber)
  (if (or (= startNumber 2) (= startNumber 3)) "Prime" (isPrimeHelper startNumber 2)) )

(isPrime 3)
