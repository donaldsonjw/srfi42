(module dispatching_generator_tests
   (library srfi42)
   (include "test_macros.sch")
   (import test_utils)
   (export (dispatching-generator-tests)))


; ==========================================================================
; The dispatching generator
; ==========================================================================
(define (dispatching-generator-tests)
   (my-check (list-ec (: c '(a b)) c) => '(a b))
   (my-check (list-ec (: c '(a b) '(c d)) c) => '(a b c d))

   (my-check (list-ec (: c "ab") c) => '(#\a #\b))
   (my-check (list-ec (: c "ab" "cd") c) => '(#\a #\b #\c #\d))

   (my-check (list-ec (: c (vector 'a 'b)) c) => '(a b))
   (my-check (list-ec (: c (vector 'a 'b) (vector 'c)) c) => '(a b c))

   (my-check (list-ec (: i 0) i) => '())
   (my-check (list-ec (: i 1) i) => '(0))
   (my-check (list-ec (: i 10) i) => '(0 1 2 3 4 5 6 7 8 9))
   (my-check (list-ec (: i 1 2) i) => '(1))
   (my-check (list-ec (: i 1 2 3) i) => '(1))
   (my-check (list-ec (: i 1 9 3) i) => '(1 4 7))

   (my-check (list-ec (: i 0.0 1.0 0.2) i) => '(0. 0.2 0.4 0.6 0.8))

   (my-check (list-ec (: c #\a #\c) c) => '(#\a #\b #\c))

   (my-check 
      (begin
         (let ((f (my-open-output-file "tmp1")))
            (do-ec (:range n 10) (begin (write n f) (newline f)))
            (close-output-port f))
         (my-call-with-input-file "tmp1"                 
            (lambda (port) (list-ec (: x port read) x)) ))
      => (list-ec (:range n 10) n) )
   
   (my-check 
      (begin
         (let ((f (my-open-output-file "tmp1")))
            (do-ec (:range n 10) (begin (write n f) (newline f)))
            (close-output-port f))
         (my-call-with-input-file "tmp1"                 
            (lambda (port) (list-ec (: x port) x)) ))
      => (list-ec (:range n 10) n) )


   )
