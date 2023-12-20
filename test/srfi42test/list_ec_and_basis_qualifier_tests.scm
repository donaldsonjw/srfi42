(module list_ec_and_basis_qualifier_tests
   (library srfi42)
   (include "test_macros.sch")
   (import test_utils)
   (export (list-ec-and-basic-qualifiers-tests)))



; ==========================================================================
; list-ec and basic qualifiers 
; ==========================================================================
(define (list-ec-and-basic-qualifiers-tests)
   (my-check (list-ec 1) => '(1))
   
   (my-check (list-ec (:range i 4) i) => '(0 1 2 3))
   
   (my-check (list-ec (:range n 3) (:range k (+ n 1)) (list n k)) 
      => '((0 0) (1 0) (1 1) (2 0) (2 1) (2 2)) )
   
   (my-check 
      (list-ec (:range n 5) (if (even? n)) (:range k (+ n 1)) (list n k)) 
      => '((0 0) (2 0) (2 1) (2 2) (4 0) (4 1) (4 2) (4 3) (4 4)) )
   
   (my-check 
      (list-ec (:range n 5) (not (even? n)) (:range k (+ n 1)) (list n k)) 
      => '((1 0) (1 1) (3 0) (3 1) (3 2) (3 3)) )
   
   (my-check
      (list-ec (:range n 5) 
         (and (even? n) (> n 2)) 
         (:range k (+ n 1)) 
         (list n k) )
      => '((4 0) (4 1) (4 2) (4 3) (4 4)) )
   
   (my-check
      (list-ec (:range n 5) 
         (or (even? n) (> n 3)) 
         (:range k (+ n 1)) 
         (list n k) )
      => '((0 0) (2 0) (2 1) (2 2) (4 0) (4 1) (4 2) (4 3) (4 4)) )

   (my-check
      (let ((x 0)) (list-ec (:range n 10) (begin (set! x (+ x 1))) n) x)
      => 10 )
   
   (my-check
      (list-ec (nested (:range n 3) (:range k n)) k)
      => '(0 0 1) )
   )
