(module special_generators_tests
   (library srfi42)
   (include "test_macros.sch")
   (import test_utils)
   (export (special-generators-tests)))


; ; ==========================================================================
; ; The special generators :do :let :parallel :while :until
; ; ==========================================================================
(define (special-generators-tests)
   (my-check (list-ec (:do ((i 0)) (< i 4) ((+ i 1))) i) => '(0 1 2 3))

   (my-check 
      (list-ec 
         (:do (let ((x 'x)))
              ((i 0)) 
              (< i 4) 
              (let ((j (- 10 i))))
              #t
              ((+ i 1)) )
         j )
      => '(10 9 8 7))

   (my-check (list-ec (:let x 1) x) => '(1))
   (my-check (list-ec (:let x 1) (:let y (+ x 1)) y) => '(2))
   (my-check (list-ec (:let x 1) (:let x (+ x 1)) x) => '(2))

   (my-check 
      (list-ec (:parallel (:range i 1 10) (:list x '(a b c))) (list i x))
      => '((1 a) (2 b) (3 c)) )

   (my-check 
      (list-ec (:while (:range i 1 10) (< i 5)) i)
      => '(1 2 3 4) )

   (my-check 
      (list-ec (:until (:range i 1 10) (>= i 5)) i)
      => '(1 2 3 4 5) )

   ; with generator that might use inner bindings

   (my-check
      (list-ec (:while (:list i '(1 2 3 4 5 6 7 8 9)) (< i 5)) i)
      => '(1 2 3 4) )
   ; Was broken in original reference implementation as pointed
   ; out by sunnan@handgranat.org on 24-Apr-2005 comp.lang.scheme.
   ; Refer to http://groups-beta.google.com/group/comp.lang.scheme/
   ; browse_thread/thread/f5333220eaeeed66/75926634cf31c038#75926634cf31c038

   (my-check 
      (list-ec (:until (:list i '(1 2 3 4 5 6 7 8 9)) (>= i 5)) i)
      => '(1 2 3 4 5) )

   (my-check
      (list-ec (:while (:vector x (index i) '#(1 2 3 4 5))
                  (< x 10))
         x)
      => '(1 2 3 4 5))
   ; Was broken in reference implementation, even after fix for the
   ; bug reported by Sunnan, as reported by Jens-Axel Soegaard on
   ; 4-Jun-2007.

   ; combine :while/:until and :parallel

   (my-check
      (list-ec (:while (:parallel (:range i 1 10)
                          (:list j '(1 2 3 4 5 6 7 8 9)))
                  (< i 5))
         (list i j))
      => '((1 1) (2 2) (3 3) (4 4)))

   (my-check
      (list-ec (:until (:parallel (:range i 1 10)
                          (:list j '(1 2 3 4 5 6 7 8 9)))
                  (>= i 5))
         (list i j))
      => '((1 1) (2 2) (3 3) (4 4) (5 5)))

   ; check that :while/:until really stop the generator

   (my-check
      (let ((n 0))
         (do-ec (:while (:range i 1 10) (begin (set! n (+ n 1)) (< i 5)))
            (if #f #f))
         n)
      => 5)

   (my-check
      (let ((n 0))
         (do-ec (:until (:range i 1 10) (begin (set! n (+ n 1)) (>= i 5)))
            (if #f #f))
         n)
      => 5)

   (my-check
      (let ((n 0))
         (do-ec (:while (:parallel (:range i 1 10)
                           (:do () (begin (set! n (+ n 1)) #t) ()))
                   (< i 5))
            (if #f #f))
         n)
      => 5)

   (my-check
      (let ((n 0))
         (do-ec (:until (:parallel (:range i 1 10)
                           (:do () (begin (set! n (+ n 1)) #t) ()))
                   (>= i 5))
            (if #f #f))
         n)
      => 5)
   )
