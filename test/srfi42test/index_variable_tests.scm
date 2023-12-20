(module index_variable_tests
   (library srfi42)
   (include "test_macros.sch")
   (import test_utils)
   (export (index-variable-tests)))

  
; ==========================================================================
; With index variable
; ==========================================================================
(define (index-variable-tests)
   (my-check (list-ec (:list c (index i) '(a b)) (list c i)) => '((a 0) (b 1)))
   (my-check (list-ec (:string c (index i) "a") (list c i)) => '((#\a 0)))
   (my-check (list-ec (:vector c (index i) (vector 'a)) (list c i)) => '((a 0)))

   (my-check 
      (list-ec (:range i (index j) 0 -3 -1) (list i j)) 
      => '((0 0) (-1 1) (-2 2)) )

   (my-check 
      (list-ec (:real-range i (index j) 0 1 0.2) (list i j)) 
      => '((0. 0) (0.2 1) (0.4 2) (0.6 3) (0.8 4)) )

   (my-check 
      (list-ec (:char-range c (index i) #\a #\c) (list c i)) 
      => '((#\a 0) (#\b 1) (#\c 2)) )

   (my-check 
      (list-ec (: x (index i) '(a b c d)) (list x i))
      => '((a 0) (b 1) (c 2) (d 3)) )

   (my-check 
      (begin
         (let ((f (my-open-output-file "tmp1")))
            (do-ec (:range n 10) (begin (write n f) (newline f)))
            (close-output-port f))
         (my-call-with-input-file "tmp1"
            (lambda (port) (list-ec (: x (index i) port) (list x i))) ))
      => '((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9)) )

   )
