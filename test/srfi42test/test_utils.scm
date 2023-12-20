(module test_utils
   (export my-open-output-file
           my-call-with-input-file
           (my-equal? x y)
           my-check-correct
           my-check-wrong))



(define my-open-output-file open-output-file)
(define my-call-with-input-file call-with-input-file)

; Tools for checking results
; ==========================

(define (my-equal? x y)
  (cond
   ((or (boolean? x) 
        (null? x)
        (symbol? x) 
        (char? x) 
        (input-port? x)
        (output-port? x) )
    (eqv? x y) )
   ((string? x)
    (and (string? y) (string=? x y)) )
   ((vector? x)
    (and (vector? y)
         (my-equal? (vector->list x) (vector->list y)) ))
   ((u8vector? x)
    (and (u8vector? y)
         (my-equal? (u8vector->list x) (u8vector->list y)) ))
   ((s8vector? x)
    (and (s8vector? y)
         (my-equal? (s8vector->list x) (s8vector->list y)) ))
   ((u16vector? x)
    (and (u16vector? y)
         (my-equal? (u16vector->list x) (u16vector->list y)) ))
   ((s16vector? x)
    (and (s16vector? y)
         (my-equal? (s16vector->list x) (s16vector->list y)) ))
   ((u32vector? x)
    (and (u32vector? y)
         (my-equal? (u32vector->list x) (u32vector->list y)) ))
   ((s32vector? x)
    (and (s32vector? y)
         (my-equal? (s32vector->list x) (s32vector->list y)) ))
   ((u64vector? x)
    (and (u64vector? y)
         (my-equal? (u64vector->list x) (u64vector->list y)) ))
   ((s64vector? x)
    (and (s64vector? y)
         (my-equal? (s64vector->list x) (s64vector->list y)) ))
   ((f32vector? x)
    (and (f32vector? y)
         (my-equal? (f32vector->list x) (f32vector->list y)) ))
   ((f64vector? x)
    (and (f64vector? y)
         (my-equal? (f64vector->list x) (f64vector->list y)) ))
   ((pair? x)
    (and (pair? y)
         (my-equal? (car x) (car y))
         (my-equal? (cdr x) (cdr y)) ))
   ((integer? x)
    (and (integer? y)
         (= x y)))
   ((real? x)
    (and (real? y)
         (eqv? (exact? x) (exact? y))
         (if (exact? x)
             (= x y)
             (< (abs (- x y)) (/ 1 (expt 10 6))) ))) ; will do here
   (else
    (error "my-equal" "unrecognized type" x) )))

(define my-check-correct 0)
(define my-check-wrong   0)
   

