(module type_generators_tests
   (library srfi42)
   (include "test_macros.sch")
   (import test_utils)
   (export (typed-generators-tests)))


; ==========================================================================
; Typed generators
; ==========================================================================
(define (typed-generators-tests)
   (my-check (list-ec (:list x '()) x) => '())
   (my-check (list-ec (:list x '(1)) x) => '(1))
   (my-check (list-ec (:list x '(1 2 3)) x) => '(1 2 3))
   (my-check (list-ec (:list x '(1) '(2)) x) => '(1 2))
   (my-check (list-ec (:list x '(1) '(2) '(3)) x) => '(1 2 3))

   (my-check (list-ec (:string c "") c) => '())
   (my-check (list-ec (:string c "1") c) => '(#\1))
   (my-check (list-ec (:string c "123") c) => '(#\1 #\2 #\3))
   (my-check (list-ec (:string c "1" "2") c) => '(#\1 #\2))
   (my-check (list-ec (:string c "1" "2" "3") c) => '(#\1 #\2 #\3))

   (my-check (list-ec (:vector x (vector)) x) => '())
   (my-check (list-ec (:vector x (vector 1)) x) => '(1))
   (my-check (list-ec (:vector x (vector 1 2 3)) x) => '(1 2 3))
   (my-check (list-ec (:vector x (vector 1) (vector 2)) x) => '(1 2))
   (my-check 
      (list-ec (:vector x (vector 1) (vector 2) (vector 3)) x)
      => '(1 2 3))

   (my-check (list-ec (:range x -2) x) => '())
   (my-check (list-ec (:range x -1) x) => '())
   (my-check (list-ec (:range x  0) x) => '())
   (my-check (list-ec (:range x  1) x) => '(0))
   (my-check (list-ec (:range x  2) x) => '(0 1))

   (my-check (list-ec (:range x  0  3) x) => '(0 1 2))
   (my-check (list-ec (:range x  1  3) x) => '(1 2))
   (my-check (list-ec (:range x -2 -1) x) => '(-2))
   (my-check (list-ec (:range x -2 -2) x) => '())

   (my-check (list-ec (:range x 1 5  2) x) => '(1 3))
   (my-check (list-ec (:range x 1 6  2) x) => '(1 3 5))
   (my-check (list-ec (:range x 5 1 -2) x) => '(5 3))
   (my-check (list-ec (:range x 6 1 -2) x) => '(6 4 2))

   (my-check (list-ec (:real-range x 0.0 3.0)     x) => '(0. 1. 2.))
   (my-check (list-ec (:real-range x 0   3.0)     x) => '(0. 1. 2.))
   (my-check (list-ec (:real-range x 0   3   1.0) x) => '(0. 1. 2.))

   (my-check 
      (string-ec (:char-range c #\a #\z) c) 
      => "abcdefghijklmnopqrstuvwxyz" )

   (my-check 
      (begin
         (let ((f (my-open-output-file "tmp1")))
            (do-ec (:range n 10) (begin (write n f) (newline f)))
            (close-output-port f))
         (my-call-with-input-file "tmp1"
            (lambda (port) (list-ec (:port x port read) x)) ))
      => (list-ec (:range n 10) n) )

   (my-check 
      (begin
         (let ((f (my-open-output-file "tmp1")))
            (do-ec (:range n 10) (begin (write n f) (newline f)))
            (close-output-port f))
         (my-call-with-input-file "tmp1"                 
            (lambda (port) (list-ec (:port x port) x)) ))
      => (list-ec (:range n 10) n) )

   )
