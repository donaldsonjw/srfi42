(module do_ec_tests
   (library srfi42)
   (include "test_macros.sch")
   (import test_utils)
   (export (do-ec-tests)))

; ==========================================================================
; do-ec 
; ==========================================================================
(define (do-ec-tests)
   (my-check 
      (let ((x 0)) (do-ec (set! x (+ x 1))) x) 
      => 1)
   
   (my-check 
      (let ((x 0)) (do-ec (:range i 10) (set! x (+ x 1))) x) 
      => 10)
   
   (my-check 
      (let ((x 0)) (do-ec (:range n 10) (:range k n) (set! x (+ x 1))) x) 
      => 45))
