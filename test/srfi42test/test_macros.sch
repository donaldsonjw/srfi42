; (define-syntax my-check
;   (syntax-rules (=>)
;     ((my-check ec => desired-result)
;      (begin
;         (newline)
;         (write (quote ec))
;         (newline)
;         (let ((actual-result ec))
;            (display "  => ")
;            (write actual-result)
;            (if (my-equal? actual-result desired-result)
;                (begin
;                   (display " ; correct")
;                   (set! my-check-correct (+ my-check-correct 1)) )
;                (begin
;                   (display " ; *** wrong ***, desired result:")
;                   (newline)
;                   (display "  => ")
;                   (write desired-result)
;                   (set! my-check-wrong (+ my-check-wrong 1)) ))
;            (newline) )))))


(define-expander my-check
   (lambda (x e)
      (match-case x
         ((?- ?ec (kwote =>) ?desired-result)
          (let ((actual-result (gensym 'actual-result)))
             (e `(begin
                    (newline)
                    (write (quote ,ec))
                    (newline)
                    (let ((,actual-result ,ec))
                       (display "  => ")
                       (write ,actual-result)
                       (if (my-equal? ,actual-result ,desired-result)
                           (begin
                              (display " ; correct")
                          (set! my-check-correct (+ my-check-correct 1)) )
                           (begin
                              (display " ; *** wrong ***, desired result:")
                              (newline)
                              (display "  => ")
                              (write ,desired-result)
                              (set! my-check-wrong (+ my-check-wrong 1)) ))
                       (newline) )) e))))))

