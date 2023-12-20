(module srfi_examples_tests
   (library srfi42)
   (include "test_macros.sch")
   (import test_utils)
   (export (srfi-examples-tests)))


(define (read-line port) ; next line (incl. #\newline) of port
   (let ((line
            (string-ec 
               (:until (:port c port read-char)
                  (char=? c #\newline) )
               c )))
      (if (string=? line "")
          (read-char port) ; eof-object
          line )))

(define (read-lines filename) ; list of all lines
   (my-call-with-input-file 
      filename
      (lambda (port)
         (list-ec (:port line port read-line) line) )))


; ==========================================================================
; The examples from the SRFI document
; ==========================================================================

(define (srfi-examples-tests)
   
   ; from Abstract
   (my-check (list-ec (: i 5) (* i i)) => '(0 1 4 9 16))

   (my-check 
      (list-ec (: n 1 4) (: i n) (list n i)) 
      => '((1 0) (2 0) (2 1) (3 0) (3 1) (3 2)) )

   ; from Generators

   (my-check 
      (list-ec (: x (index i) "abc") (list x i)) 
      => '((#\a 0) (#\b 1) (#\c 2)) )

   (my-check
      (list-ec (:string c (index i) "a" "b") (cons c i))
      => '((#\a . 0) (#\b . 1)) )


   ; ==========================================================================
   ; Little Shop of Horrors
   ; ==========================================================================

   (my-check (list-ec (:range x 5) (:range x x) x) => '(0 0 1 0 1 2 0 1 2 3))

   (my-check (list-ec (:list x '(2 "23" (4))) (: y x) y) => '(0 1 #\2 #\3 4))

   (my-check 
      (list-ec (:parallel (:integers x) 
                  (:do ((i 10)) (< x i) ((- i 1))))
         (list x i))
      => '((0 10) (1 9) (2 8) (3 7) (4 6)) )


   ; ==========================================================================
   ; Less artificial examples
   ; ==========================================================================

   (define (factorial n) ; n * (n-1) * .. * 1 for n >= 0
      (product-ec (:range k 2 (+ n 1)) k) )

   (my-check (factorial  0) => 1)
   (my-check (factorial  1) => 1)
   (my-check (factorial  3) => 6)
   (my-check (factorial  5) => 120)


   (define (eratosthenes n) ; primes in {2..n-1} for n >= 1
      (let ((p? (make-string n #\1)))
         (do-ec (:range k 2 n)
            (if (char=? (string-ref p? k) #\1))
            (:range i (* 2 k) n k)
            (string-set! p? i #\0) )
         (list-ec (:range k 2 n) (if (char=? (string-ref p? k) #\1)) k) ))

   (my-check 
      (eratosthenes 50)
      => '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47) )

   (my-check
      (length (eratosthenes 100000))
      => 9592 ) ; we expect 10^5/ln(10^5)


   (define (pythagoras n) ; a, b, c s.t. 1 <= a <= b <= c <= n, a^2 + b^2 = c^2
      (list-ec 
         (:let sqr-n (* n n))
         (:range a 1 (+ n 1))
         ; (begin (display a) (display " "))
         (:let sqr-a (* a a))
         (:range b a (+ n 1)) 
         (:let sqr-c (+ sqr-a (* b b)))
         (if (<= sqr-c sqr-n))
         (:range c b (+ n 1))
         (if (= (* c c) sqr-c))
         (list a b c) ))
   
   (my-check
      (pythagoras 15)
      => '((3 4 5) (5 12 13) (6 8 10) (9 12 15)) )

   (my-check
      (length (pythagoras 200))
      => 127 )


   (define (qsort xs) ; stable
      (if (null? xs)
          '()
          (let ((pivot (car xs)) (xrest (cdr xs)))
             (append
                (qsort (list-ec (:list x xrest) (if (<  x pivot)) x))
                (list pivot)
                (qsort (list-ec (:list x xrest) (if (>= x pivot)) x)) ))))

   (my-check 
      (qsort '(1 5 4 2 4 5 3 2 1 3))
      => '(1 1 2 2 3 3 4 4 5 5) )


   (define (pi-BBP m) ; approx. of pi within 16^-m (Bailey-Borwein-Plouffe)
      (sum-ec 
         (:range n 0 (+ m 1))
         (:let n8 (* 8 n))
         (* (- (/ 4 (+ n8 1))
               (+ (/ 2 (+ n8 4))
                  (/ 1 (+ n8 5))
                  (/ 1 (+ n8 6))))
            (/ 1 (expt 16 n)) )))

   (my-check
      (pi-BBP 5)
      => (/ 40413742330349316707.0 12864093722915635200.0) )

   (my-check
      (begin
         (let ((f (my-open-output-file "tmp1")))
            (do-ec (:range n 10) (begin (write n f) (newline f)))
            (close-output-port f))
         (read-lines "tmp1") )
      => (list-ec (:char-range c #\0 #\9) (string c #\newline)) )
   

   )
