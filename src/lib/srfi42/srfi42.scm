;; SRFI-42 - eager comprehension
;;
;; Copyright (C) Joseph Donaldson (2023)
;;
;; This is a Bigloo port of Sebastian Egner's reference implementation
;; based on Alex Shinn's port to Gauche.
;;  
;; Copyright (C) Sebastian Egner (2003). All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

; <PLAINTEXT>
; Eager Comprehensions in [outer..inner|expr]-Convention
; ======================================================
;
; sebastian.egner@philips.com, Eindhoven, The Netherlands, 26-Dec-2007
; Scheme R5RS (incl. macros), SRFI-23 (error).
;
; Implementation comments:
;   * All local (not exported) identifiers are named ec-<something>.
;   * This implementation focuses on portability, performance,
;     readability, and simplicity roughly in this order. Design
;     decisions related to performance are taken for Scheme48.
;   * Alternative implementations, Comments and Warnings are
;     mentioned after the definition with a heading.

(module srfi42
   (include "srfi42.sch")
   (export (dispatch-union d1 d2)
           (make-initial-:-dispatch)
           (srfi-42--dispatch-ref)
           (srfi-42--dispatch-set! dispatch)
           (srfi42-error message . rest)
           srfi-42--dispatch
           exact-integer?
           (range-length r)
           (range-ref r i)
           (inline ->flonum::real v)
           (inline ->uint8::uint8 v)
           (inline ->int8::int8 v)
           (inline ->uint16::uint16 v)
           (inline ->int16::int16 v)
           (inline ->uint32::uint32 v)
           (inline ->int32::int32 v)
           (inline ->uint64::uint64 v)
           (inline ->int64::int64 v)
           (ec-:vector-filter *len vecs)))

;; bigloo utility procedures
(define (srfi42-error message . rest)
   (error "srfi42" message rest))

(define exact-integer? integer?)

(define (range-length r)
   (error "range-length" "ranges not supported yet" r))

(define (range-ref r i)
   (error "range-ref" "ranges not supported yet" r))

(define (dispatch-union d1 d2)
  (lambda (args)
    (let ((g1 (d1 args)) (g2 (d2 args)))
      (if g1
          (if g2
              (if (null? args)
                  (append (if (list? g1) g1 (list g1))
                          (if (list? g2) g2 (list g2)) )
                  (srfi42-error "dispatching conflict" args (d1 '()) (d2 '())) )
              g1 )
          (if g2 g2 #f) ))))

;; numeric conversion routines 
(define-inline (->uint8 v)
   (cond ((fixnum? v)
          (fixnum->uint8 v))
         ((elong? v)
          (fixnum->uint8 (elong->fixnum v)))
         ((llong? v)
          (fixnum->uint8 (llong->fixnum v)))
         ((bignum? v)
          (fixnum->uint8 (bignum->fixnum v)))
         ((uint8? v)
          v)
         ((int8? v)
          (int8->uint8 (int8->fixnum v)))
         ((uint16? v)
          (fixnum->uint8 (uint16->fixnum v)))
         ((int16? v)
          (fixnum->uint8 (int16->fixnum v)))
         ((uint32? v)
          (fixnum->uint8 (uint32->fixnum v)))
         ((int32? v)
          (fixnum->uint8 (int32->fixnum v)))
         ((uint64? v)
          (fixnum->uint8 (uint64->fixnum v)))
         ((int64? v)
          (fixnum->uint8 (int64->fixnum v)))
         ((flonum? v)
          (fixnum->uint8 (flonum->fixnum v)))
         (else
          (error "->uint8" "unsupported numeric type" v))))

(define-inline (->int8 v)
   (cond ((fixnum? v)
          (fixnum->int8 v))
         ((elong? v)
          (fixnum->int8 (elong->fixnum v)))
         ((llong? v)
          (fixnum->int8 (llong->fixnum v)))
         ((bignum? v)
          (fixnum->int8 (bignum->fixnum v)))
         ((uint8? v)
          (uint8->int8 v))
         ((int8? v)
          v)
         ((uint16? v)
          (fixnum->int8 (uint16->fixnum v)))
         ((int16? v)
          (fixnum->int8 (int16->fixnum v)))
         ((uint32? v)
          (fixnum->int8 (uint32->fixnum v)))
         ((int32? v)
          (fixnum->int8 (int32->fixnum v)))
         ((uint64? v)
          (fixnum->int8 (uint64->fixnum v)))
         ((int64? v)
          (fixnum->int8 (int64->fixnum v)))
         ((flonum? v)
          (fixnum->int8 (flonum->fixnum v)))
         (else
          (error "->int8" "unsupported numeric type" v))))


(define-inline (->uint16 v)
   (cond ((fixnum? v)
          (fixnum->uint16 v))
         ((elong? v)
          (fixnum->uint16 (elong->fixnum v)))
         ((llong? v)
          (fixnum->uint16 (llong->fixnum v)))
         ((bignum? v)
          (fixnum->uint16 (bignum->fixnum v)))
         ((uint8? v)
          (fixnum->uint16 (uint8->fixnum v)))
         ((int8? v)
          (fixnum->uint16 (int8->fixnum v)))
         ((uint16? v)
          v)
         ((int16? v)
          (fixnum->uint16 (int16->fixnum v)))
         ((uint32? v)
          (fixnum->uint16 (uint32->fixnum v)))
         ((int32? v)
          (fixnum->uint16 (int32->fixnum v)))
         ((uint64? v)
          (fixnum->uint16 (uint64->fixnum v)))
         ((int64? v)
          (fixnum->uint16 (int64->fixnum v)))
         ((flonum? v)
          (fixnum->uint16 (flonum->fixnum v)))
         (else
          (error "->uint16" "unsupported numeric type" v))))

(define-inline (->int16 v)
   (cond ((fixnum? v)
          (fixnum->int16 v))
         ((elong? v)
          (fixnum->int16 (elong->fixnum v)))
         ((llong? v)
          (fixnum->int16 (llong->fixnum v)))
         ((bignum? v)
          (fixnum->int16 (bignum->fixnum v)))
         ((uint8? v)
          (fixnum->int16 (uint8->fixnum v)))
         ((int8? v)
          (fixnum->int16 (int8->fixnum v)))
         ((uint16? v)
          (fixnum->int16 (uint16->fixnum v)))
         ((int16? v)
          v)
         ((uint32? v)
          (fixnum->int16 (uint32->fixnum v)))
         ((int32? v)
          (fixnum->int16 (int32->fixnum v)))
         ((uint64? v)
          (fixnum->int16 (uint64->fixnum v)))
         ((int64? v)
          (fixnum->int16 (int64->fixnum v)))
         ((flonum? v)
          (fixnum->int16 (flonum->fixnum v)))
         (else
          (error "->int16" "unsupported numeric type" v))))


(define-inline (->uint32 v)
   (cond ((fixnum? v)
          (fixnum->uint32 v))
         ((elong? v)
          (elong->uint32 v))
         ((llong? v)
          (llong->uint32 v))
         ((bignum? v)
          (llong->uint32 (bignum->llong v)))
         ((uint8? v)
          (fixnum->uint32 (uint8->fixnum v)))
         ((int8? v)
          (fixnum->uint32 (int8->fixnum v)))
         ((uint16? v)
          (fixnum->uint32 (uint16->fixnum v)))
         ((int16? v)
          (fixnum->uint32 (int16->fixnum v)))
         ((uint32? v)
          v)
         ((int32? v)
          (int32->uint32 v))
         ((uint64? v)
          (uint64->uint32 v))
         ((int64? v)
          (llong->uint32 (int64->llong v)))
         ((flonum? v)
          (fixnum->uint32 (flonum->fixnum v)))
         (else
          (error "->uint32" "unsupported numeric type" v))))

(define-inline (->int32 v)
   (cond ((fixnum? v)
          (fixnum->int32 v))
         ((elong? v)
          (elong->int32 v))
         ((llong? v)
          (llong->int32 v))
         ((bignum? v)
          (elong->int32 (bignum->elong v)))
         ((uint8? v)
          (fixnum->int32 (uint8->fixnum v)))
         ((int8? v)
          (fixnum->int32 (int8->fixnum v)))
         ((uint16? v)
          (fixnum->int32 (uint16->fixnum v)))
         ((int16? v)
          (fixnum->int32 (int16->fixnum v)))
         ((uint32? v)
          (llong->int32 (uint32->llong v)))
         ((int32? v)
          v)
         ((uint64? v)
          (llong->int32 (uint64->llong v)))
         ((int64? v)
          (llong->int32 (int64->llong v)))
         ((flonum? v)
          (fixnum->int32 (flonum->int32 v)))
         (else
          (error "->int32" "unsupported numeric type" v))))

(define-inline (->uint64 v)
   (cond ((fixnum? v)
          (fixnum->uint64 v))
         ((elong? v)
          (uint32->uint64 (elong->uint32 v)))
         ((llong? v)
          (llong->uint64 v))
         ((bignum? v)
          (bignum->uint64 v))
         ((uint8? v)
          (fixnum->uint64 (uint8->fixnum v)))
         ((int8? v)
          (fixnum->uint64 (int8->fixnum v)))
         ((uint16? v)
          (fixnum->uint64 (uint16->fixnum v)))
         ((int16? v)
          (fixnum->uint64 (int16->fixnum v)))
         ((uint32? v)
          (uint32->uint64 v))
         ((int32? v)
          (llong->uint64 (int32->llong v)))
         ((uint64? v)
          v)
         ((int64? v)
          (llong->uint64 (int64->llong v)))
         ((flonum? v)
          (flonum->uint64 v))
         (else
          (error "->uint64" "unsupported numeric type" v))))

(define-inline (->int64 v)
   (cond ((fixnum? v)
          (fixnum->int64 v))
         ((elong? v)
          (llong->int64 v) (elong->llong v))
         ((llong? v)
          (llong->int64 v))
         ((bignum? v)
          (bignum->int64 v))
         ((uint8? v)
          (fixnum->int64 (uint8->fixnum v)))
         ((int8? v)
          (fixnum->int64 (int8->fixnum v)))
         ((uint16? v)
          (fixnum->int64 (uint16->fixnum v)))
         ((int16? v)
          (fixnum->int64 (int16->fixnum v)))
         ((uint32? v)
          (llong->int64 (uint32->llong v)))
         ((int32? v)
          (llong->int64 (int32->llong v)))
         ((uint64? v)
          (uint64->int64 v))
         ((int64? v)
          v)
         ((flonum? v)
          (flonum->int64 v))
         (else
          (error "->int64" "unsupported numeric type" v))))


(define-inline (->flonum v)
   (cond ((fixnum? v)
          (fixnum->flonum v))
         ((elong? v)
          (elong->flonum v))
         ((llong? v)
          (llong->flonum v))
         ((bignum? v)
          (bignum->flonum v))
         ((uint8? v)
          (fixnum->flonum (uint8->fixnum v)))
         ((int8? v)
          (fixnum->flonum (int8->fixnum v)))
         ((uint16? v)
          (fixnum->flonum (uint16->fixnum v)))
         ((int16? v)
          (fixnum->flonum (int16->fixnum v)))
         ((uint32? v)
          (uint32->flonum v))
         ((int32? v)
          (int32->flonum v))
         ((uint64? v)
          (uint64->flonum v))
         ((int64? v)
          (int64->flonum v))
         ((flonum? v)
          v)
         (else
          (error "->flonum" "unsupported numeric type" v))))

; ==========================================================================
; The dispatching generator :
; ==========================================================================

(define (make-initial-:-dispatch)
  (lambda (args)
    (case (length args)
      [(0) 'SRFI42]
      [(1) (let ([a1 (car args)])
             (cond
              [(list? a1)
               (srfi-42-generator-proc (srfi-42-list a1))]
              [(string? a1)
               (srfi-42-generator-proc (srfi-42-string a1))]
              [(vector? a1)
               (srfi-42-generator-proc (srfi-42-vector a1))]
              [(s8vector? a1)
               (srfi-42-generator-proc (srfi-42-s8vector a1))]
              [(u8vector? a1)
               (srfi-42-generator-proc (srfi-42-u8vector a1))]
              [(s16vector? a1)
               (srfi-42-generator-proc (srfi-42-s16vector a1))]
              [(u16vector? a1)
               (srfi-42-generator-proc (srfi-42-u16vector a1))]
              [(s32vector? a1)
               (srfi-42-generator-proc (srfi-42-s32vector a1))]
              [(u32vector? a1)
               (srfi-42-generator-proc (srfi-42-u32vector a1))]
              [(s64vector? a1)
               (srfi-42-generator-proc (srfi-42-s64vector a1))]
              [(u64vector? a1)
               (srfi-42-generator-proc (srfi-42-u64vector a1))]
              [(f32vector? a1)
               (srfi-42-generator-proc (srfi-42-f32vector a1))]
              [(f64vector? a1)
               (srfi-42-generator-proc (srfi-42-f64vector a1))]
              [(and (integer? a1) (exact? a1))
               (srfi-42-generator-proc (srfi-42-range a1))]
              [(real? a1)
               (srfi-42-generator-proc (srfi-42-real-range a1))]
              [(input-port? a1)
               (srfi-42-generator-proc (srfi-42-port a1))]
              ; [(is-a? a1 <collection>)
              ;  (srfi-42-generator-proc (srfi-42-collection a1))]
              ; [(applicable? a1)
              ;  (srfi-42-generator-proc (srfi-42-generator a1))]
              ;; This would trigger autoloading data.range, so we don't
              ;; support it for now.
              ;; [(range? a1)
              ;;  (srfi-42-generator-proc (srfi-42-range a1))]
              [else #f]))]
      [(2) (let ([a1 (car args)] [a2 (cadr args)])
             (cond
              [(and (list? a1) (list? a2))
               (srfi-42-generator-proc (srfi-42-list a1 a2)) ]
              [(and (string? a1) (string? a1))
               (srfi-42-generator-proc (srfi-42-string a1 a2)) ]
              [(and (vector? a1) (vector? a2))
               (srfi-42-generator-proc (srfi-42-vector a1 a2)) ]
              ; [(and (uvector? a1) (uvector? a2))
              ;  (srfi-42-generator-proc (srfi-42-uvector a1 a2)) ]
              [(and (integer? a1) (exact? a1) (integer? a2) (exact? a2))
               (srfi-42-generator-proc (srfi-42-range a1 a2)) ]
              [(and (real? a1) (real? a2))
               (srfi-42-generator-proc (srfi-42-real-range a1 a2)) ]
              [(and (char? a1) (char? a2))
               (srfi-42-generator-proc (srfi-42-char-range a1 a2)) ]
              [(and (input-port? a1) (procedure? a2))
               (srfi-42-generator-proc (srfi-42-port a1 a2)) ]
              [else #f]))]
      [(3) (let ([a1 (car args)] [a2 (cadr args)] [a3 (caddr args)])
             (cond
              [(and (list? a1) (list? a2) (list? a3))
               (srfi-42-generator-proc (srfi-42-list a1 a2 a3)) ]
              [(and (string? a1) (string? a1) (string? a3))
               (srfi-42-generator-proc (srfi-42-string a1 a2 a3)) ]
              [(and (vector? a1) (vector? a2) (vector? a3))
               (srfi-42-generator-proc (srfi-42-vector a1 a2 a3)) ]
              ; [(and (uvector? a1) (uvector? a2) (uvector? a3))
              ;  (srfi-42-generator-proc (srfi-42-uvector a1 a2 a3)) ]
              [(and (integer? a1) (exact? a1)
                    (integer? a2) (exact? a2)
                    (integer? a3) (exact? a3))
               (srfi-42-generator-proc (srfi-42-range a1 a2 a3)) ]
              [(and (real? a1) (real? a2) (real? a3))
               (srfi-42-generator-proc (srfi-42-real-range a1 a2 a3)) ]
              [else #f]))]
      [else (cond
             [(every list? args)
              (srfi-42-generator-proc (srfi-42-list (apply append args))) ]
             [(every string? args)
              (srfi-42-generator-proc (srfi-42-string (apply string-append args)))]
             [(every vector? args)
              (srfi-42-generator-proc (srfi-42-vector (apply vector-append args)))]
             ; [(every uvector? args)
             ;  (srfi-42-generator-proc (srfi-42-list (apply append (map uvector->list args))))]
             [else #f])])))

(define srfi-42--dispatch
  (make-initial-:-dispatch) )

(define (srfi-42--dispatch-ref)
  srfi-42--dispatch )

(define (srfi-42--dispatch-set! dispatch)
  (if (not (procedure? dispatch))
      (srfi42-error "not a procedure" dispatch) )
  (set! srfi-42--dispatch dispatch) )

(define (ec-:vector-filter *len vecs)
  (if (null? vecs)
      '()
      (if (zero? (*len (car vecs)))
          (ec-:vector-filter *len (cdr vecs))
          (cons (car vecs) (ec-:vector-filter *len (cdr vecs))) )))

