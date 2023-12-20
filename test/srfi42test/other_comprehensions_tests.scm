(module other_comprehensions_tests
   (library srfi42)
   (include "test_macros.sch")
   (import test_utils)
   (export (other-comprehensions-tests)))



; ==========================================================================
; Other comprehensions
; ==========================================================================

(define (other-comprehensions-tests)
   (my-check (append-ec '(a b)) => '(a b))
   (my-check (append-ec (:range i 0) '(a b)) => '())
   (my-check (append-ec (:range i 1) '(a b)) => '(a b))
   (my-check (append-ec (:range i 2) '(a b)) => '(a b a b))

   (my-check (string-ec #\a) => (string #\a))
   (my-check (string-ec (:range i 0) #\a) => "")
   (my-check (string-ec (:range i 1) #\a) => "a")
   (my-check (string-ec (:range i 2) #\a) => "aa")

   (my-check (string-append-ec "ab") => "ab")
   (my-check (string-append-ec (:range i 0) "ab") => "")
   (my-check (string-append-ec (:range i 1) "ab") => "ab")
   (my-check (string-append-ec (:range i 2) "ab") => "abab")

   (my-check (vector-ec 1) => (vector 1))
   (my-check (vector-ec (:range i 0) i) => (vector))
   (my-check (vector-ec (:range i 1) i) => (vector 0))
   (my-check (vector-ec (:range i 2) i) => (vector 0 1))
   
   (my-check (vector-of-length-ec 1 1) => (vector 1))
   (my-check (vector-of-length-ec 0 (:range i 0) i) => (vector))
   (my-check (vector-of-length-ec 1 (:range i 1) i) => (vector 0))
   (my-check (vector-of-length-ec 2 (:range i 2) i) => (vector 0 1))

   (my-check (sum-ec 1) => 1)
   (my-check (sum-ec (:range i 0) i) => 0)
   (my-check (sum-ec (:range i 1) i) => 0)
   (my-check (sum-ec (:range i 2) i) => 1)
   (my-check (sum-ec (:range i 3) i) => 3)

   (my-check (product-ec 1) => 1)
   (my-check (product-ec (:range i 1 0) i) => 1)
   (my-check (product-ec (:range i 1 1) i) => 1)
   (my-check (product-ec (:range i 1 2) i) => 1)
   (my-check (product-ec (:range i 1 3) i) => 2)
   (my-check (product-ec (:range i 1 4) i) => 6)

   (my-check (min-ec 1) => 1)
   (my-check (min-ec (:range i 1) i) => 0)
   (my-check (min-ec (:range i 2) i) => 0)

   (my-check (max-ec 1) => 1)
   (my-check (max-ec (:range i 1) i) => 0)
   (my-check (max-ec (:range i 2) i) => 1)

   (my-check (first-ec #f 1) => 1)
   (my-check (first-ec #f (:range i 0) i) => #f)
   (my-check (first-ec #f (:range i 1) i) => 0)
   (my-check (first-ec #f (:range i 2) i) => 0)

   (my-check 
      (let ((last-i -1))
         (first-ec #f (:range i 10) (begin (set! last-i i)) i)
         last-i )
      => 0 )

   (my-check (last-ec #f 1) => 1)
   (my-check (last-ec #f (:range i 0) i) => #f)
   (my-check (last-ec #f (:range i 1) i) => 0)
   (my-check (last-ec #f (:range i 2) i) => 1)

   (my-check (any?-ec #f) => #f)
   (my-check (any?-ec #t) => #t)
   (my-check (any?-ec (:range i 2 2) (even? i)) => #f)
   (my-check (any?-ec (:range i 2 3) (even? i)) => #t)

   (my-check (every?-ec #f) => #f)
   (my-check (every?-ec #t) => #t)
   (my-check (every?-ec (:range i 2 2) (even? i)) => #t)
   (my-check (every?-ec (:range i 2 3) (even? i)) => #t)
   (my-check (every?-ec (:range i 2 4) (even? i)) => #f)

   (my-check 
      (let ((sum-sqr (lambda (x result) (+ result (* x x)))))
         (fold-ec 0 (:range i 10) i sum-sqr) )
      => 285 )

   (my-check 
      (let ((minus-1 (lambda (x) (- x 1)))
            (sum-sqr (lambda (x result) (+ result (* x x)))))
         (fold3-ec (error "test" "wrong" #unspecified) (:range i 10) i minus-1 sum-sqr) )
      => 284 )

   (my-check 
      (fold3-ec 'infinity (:range i 0) i min min)
      => 'infinity )
   )