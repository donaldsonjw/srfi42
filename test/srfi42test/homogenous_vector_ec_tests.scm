(module homogenous_vector_ec_tests
   (library srfi42)
   (include "test_macros.sch")
   (import test_utils)
   (export (homogenous-vector-ec-tests)))


(define (homogenous-vector-ec-tests)
   (my-check (u8vector-ec 1) => (u8vector 1))
   (my-check (u8vector-ec (:range i 0) i) => (u8vector))
   (my-check (u8vector-ec (:range i 1) i) => (u8vector 0))
   (my-check (u8vector-ec (:range i 2) i) => (u8vector 0 1))

   (my-check (s8vector-ec 1) => (s8vector 1))
   (my-check (s8vector-ec (:range i 0) i) => (s8vector))
   (my-check (s8vector-ec (:range i 1) i) => (s8vector 0))
   (my-check (s8vector-ec (:range i 2) i) => (s8vector 0 1))

   (my-check (u16vector-ec 1) => (u16vector 1))
   (my-check (u16vector-ec (:range i 0) i) => (u16vector))
   (my-check (u16vector-ec (:range i 1) i) => (u16vector 0))
   (my-check (u16vector-ec (:range i 2) i) => (u16vector 0 1))

   (my-check (s16vector-ec 1) => (s16vector 1))
   (my-check (s16vector-ec (:range i 0) i) => (s16vector))
   (my-check (s16vector-ec (:range i 1) i) => (s16vector 0))
   (my-check (s16vector-ec (:range i 2) i) => (s16vector 0 1))

   (my-check (u32vector-ec 1) => (u32vector 1))
   (my-check (u32vector-ec (:range i 0) i) => (u32vector))
   (my-check (u32vector-ec (:range i 1) i) => (u32vector 0))
   (my-check (u32vector-ec (:range i 2) i) => (u32vector 0 1))

   (my-check (s32vector-ec 1) => (s32vector 1))
   (my-check (s32vector-ec (:range i 0) i) => (s32vector))
   (my-check (s32vector-ec (:range i 1) i) => (s32vector 0))
   (my-check (s32vector-ec (:range i 2) i) => (s32vector 0 1))

   (my-check (u64vector-ec 1) => (u64vector 1))
   (my-check (u64vector-ec (:range i 0) i) => (u64vector))
   (my-check (u64vector-ec (:range i 1) i) => (u64vector 0))
   (my-check (u64vector-ec (:range i 2) i) => (u64vector 0 1))

   (my-check (s64vector-ec 1) => (s64vector 1))
   (my-check (s64vector-ec (:range i 0) i) => (s64vector))
   (my-check (s64vector-ec (:range i 1) i) => (s64vector 0))
   (my-check (s64vector-ec (:range i 2) i) => (s64vector 0 1))

   (my-check (f32vector-ec 1) => (f32vector 1.0))
   (my-check (f32vector-ec (:range i 0) i) => (f32vector))
   (my-check (f32vector-ec (:range i 1) i) => (f32vector 0.0))
   (my-check (f32vector-ec (:range i 2) i) => (f32vector 0.0 1.0))

   (my-check (f64vector-ec 1) => (f64vector 1.0))
   (my-check (f64vector-ec (:range i 0) i) => (f64vector))
   (my-check (f64vector-ec (:range i 1) i) => (f64vector 0.0))
   (my-check (f64vector-ec (:range i 2) i) => (f64vector 0.0 1.0))
   )
