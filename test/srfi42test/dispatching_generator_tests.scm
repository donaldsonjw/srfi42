(module dispatching_generator_tests
   (cond-expand
      ((library srfi196)
       (library srfi196)))
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

   (my-check (list-ec (: c (s8vector #s8:0 #s8:1) (s8vector #s8:2)) c) => '(#s8:0 #s8:1 #s8:2))
   (my-check (list-ec (: c (s8vector #s8:0 #s8:1) (s8vector #s8:2) (s8vector #s8:3)) c)
      => '(#s8:0 #s8:1 #s8:2 #s8:3))
   (my-check (list-ec (: c (s8vector #s8:0 #s8:1) (s8vector #s8:2)
                         (s8vector #s8:3) (s8vector #s8:4)) c)
      => '(#s8:0 #s8:1 #s8:2 #s8:3 #s8:4))
   
   (my-check (list-ec (: c (u8vector #u8:0 #u8:1) (u8vector #u8:2)) c) => '(#u8:0 #u8:1 #u8:2))
   (my-check (list-ec (: c (u8vector #u8:0 #u8:1) (u8vector #u8:2) (u8vector #u8:3)) c)
      => '(#u8:0 #u8:1 #u8:2 #u8:3))
   (my-check (list-ec (: c (u8vector #u8:0 #u8:1) (u8vector #u8:2)
                         (u8vector #u8:3) (u8vector #u8:4)) c)
      => '(#u8:0 #u8:1 #u8:2 #u8:3 #u8:4))

   (my-check (list-ec (: c (s16vector #s16:0 #s16:1) (s16vector #s16:2)) c) => '(#s16:0 #s16:1 #s16:2))
   (my-check (list-ec (: c (s16vector #s16:0 #s16:1) (s16vector #s16:2) (s16vector #s16:3)) c)
      => '(#s16:0 #s16:1 #s16:2 #s16:3))
   (my-check (list-ec (: c (s16vector #s16:0 #s16:1) (s16vector #s16:2)
                         (s16vector #s16:3) (s16vector #s16:4)) c)
      => '(#s16:0 #s16:1 #s16:2 #s16:3 #s16:4))
   
   (my-check (list-ec (: c (u16vector #u16:0 #u16:1) (u16vector #u16:2)) c) => '(#u16:0 #u16:1 #u16:2))
   (my-check (list-ec (: c (u16vector #u16:0 #u16:1) (u16vector #u16:2) (u16vector #u16:3)) c)
      => '(#u16:0 #u16:1 #u16:2 #u16:3))
   (my-check (list-ec (: c (u16vector #u16:0 #u16:1) (u16vector #u16:2)
                         (u16vector #u16:3) (u16vector #u16:4)) c)
      => '(#u16:0 #u16:1 #u16:2 #u16:3 #u16:4))

   (my-check (list-ec (: c (s32vector #s32:0 #s32:1) (s32vector #s32:2)) c) => '(#s32:0 #s32:1 #s32:2))
   (my-check (list-ec (: c (s32vector #s32:0 #s32:1) (s32vector #s32:2) (s32vector #s32:3)) c)
      => '(#s32:0 #s32:1 #s32:2 #s32:3))
   (my-check (list-ec (: c (s32vector #s32:0 #s32:1) (s32vector #s32:2)
                         (s32vector #s32:3) (s32vector #s32:4)) c)
      => '(#s32:0 #s32:1 #s32:2 #s32:3 #s32:4))
   
   (my-check (list-ec (: c (u32vector #u32:0 #u32:1) (u32vector #u32:2)) c) => '(#u32:0 #u32:1 #u32:2))
   (my-check (list-ec (: c (u32vector #u32:0 #u32:1) (u32vector #u32:2) (u32vector #u32:3)) c)
      => '(#u32:0 #u32:1 #u32:2 #u32:3))
   (my-check (list-ec (: c (u32vector #u32:0 #u32:1) (u32vector #u32:2)
                         (u32vector #u32:3) (u32vector #u32:4)) c)
      => '(#u32:0 #u32:1 #u32:2 #u32:3 #u32:4))


   (my-check (list-ec (: c (s64vector #s64:0 #s64:1) (s64vector #s64:2)) c) => '(#s64:0 #s64:1 #s64:2))
   (my-check (list-ec (: c (s64vector #s64:0 #s64:1) (s64vector #s64:2) (s64vector #s64:3)) c)
      => '(#s64:0 #s64:1 #s64:2 #s64:3))
   (my-check (list-ec (: c (s64vector #s64:0 #s64:1) (s64vector #s64:2)
                         (s64vector #s64:3) (s64vector #s64:4)) c)
      => '(#s64:0 #s64:1 #s64:2 #s64:3 #s64:4))
   
   (my-check (list-ec (: c (u64vector #u64:0 #u64:1) (u64vector #u64:2)) c) => '(#u64:0 #u64:1 #u64:2))
   (my-check (list-ec (: c (u64vector #u64:0 #u64:1) (u64vector #u64:2) (u64vector #u64:3)) c)
      => '(#u64:0 #u64:1 #u64:2 #u64:3))
   (my-check (list-ec (: c (u64vector #u64:0 #u64:1) (u64vector #u64:2)
                         (u64vector #u64:3) (u64vector #u64:4)) c)
      => '(#u64:0 #u64:1 #u64:2 #u64:3 #u64:4))

   (my-check (list-ec (: c (f32vector 0. 1.) (f32vector 2.)) c) => '(0. 1. 2.))
   (my-check (list-ec (: c (f32vector 0. 1.) (f32vector 2.) (f32vector 3.)) c)
      => '(0. 1. 2. 3.))
   (my-check (list-ec (: c (f32vector 0. 1.) (f32vector 2.)
                         (f32vector 3.) (f32vector 4.)) c)
      => '(0. 1. 2. 3. 4.))
   
   (my-check (list-ec (: c (f64vector 0. 1.) (f64vector 2.)) c) => '(0. 1. 2.))
   (my-check (list-ec (: c (f64vector 0. 1.) (f64vector 2.) (f64vector 3.)) c)
      => '(0. 1. 2. 3.))
   (my-check (list-ec (: c (f64vector 0. 1.) (f64vector 2.)
                         (f64vector 3.) (f64vector 4.)) c)
      => '(0. 1. 2. 3. 4.))

   (my-check (list-ec (: i 0) i) => '())
   (my-check (list-ec (: i 1) i) => '(0))
   (my-check (list-ec (: i 10) i) => '(0 1 2 3 4 5 6 7 8 9))
   (my-check (list-ec (: i 1 2) i) => '(1))
   (my-check (list-ec (: i 1 2 3) i) => '(1))
   (my-check (list-ec (: i 1 9 3) i) => '(1 4 7))

   (my-check (list-ec (: i 0.0 1.0 0.2) i) => '(0. 0.2 0.4 0.6 0.8))

   (my-check (list-ec (: c #\a #\c) c) => '(#\a #\b #\c))

   (cond-expand
      ((library srfi196)

       (my-check (list-ec (: c (range 6 (lambda (n) (integer->char (+ 65 n))))) c)
          => '(#\A #\B #\C #\D #\E #\F))

       (my-check (list-ec (: c (range-append (range 6 (lambda (n) (integer->char (+ 65 n))))
                                  (range 3 (lambda (n) (integer->char (+ 71 n)))))) c)
          => '(#\A #\B #\C #\D #\E #\F #\G #\H #\I))
       
       ))

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
