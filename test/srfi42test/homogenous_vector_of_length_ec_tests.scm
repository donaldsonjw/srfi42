(module homogenous_vector_of_length_ec_tests
   (library srfi42)
   (include "test_macros.sch")
   (import test_utils)
   (export (homogenous-vector-of-length-ec-tests)))


(define (homogenous-vector-of-length-ec-tests)
   (my-check (u8vector-of-length-ec 1 1) => (u8vector 1))
   (my-check (u8vector-of-length-ec 0 (:range i 0) i) => (u8vector))
   (my-check (u8vector-of-length-ec 1 (:range i 1) i) => (u8vector 0))
   (my-check (u8vector-of-length-ec 2 (:range i 2) i) => (u8vector 0 1))

   (my-check (s8vector-of-length-ec 1 1) => (s8vector 1))
   (my-check (s8vector-of-length-ec 0 (:range i 0) i) => (s8vector))
   (my-check (s8vector-of-length-ec 1 (:range i 1) i) => (s8vector 0))
   (my-check (s8vector-of-length-ec 2 (:range i 2) i) => (s8vector 0 1))

   (my-check (u16vector-of-length-ec 1 1) => (u16vector 1))
   (my-check (u16vector-of-length-ec 0 (:range i 0) i) => (u16vector))
   (my-check (u16vector-of-length-ec 1 (:range i 1) i) => (u16vector 0))
   (my-check (u16vector-of-length-ec 2 (:range i 2) i) => (u16vector 0 1))

   (my-check (s16vector-of-length-ec 1 1) => (s16vector 1))
   (my-check (s16vector-of-length-ec 0 (:range i 0) i) => (s16vector))
   (my-check (s16vector-of-length-ec 1 (:range i 1) i) => (s16vector 0))
   (my-check (s16vector-of-length-ec 2 (:range i 2) i) => (s16vector 0 1))

   (my-check (u32vector-of-length-ec 1 1) => (u32vector 1))
   (my-check (u32vector-of-length-ec 0 (:range i 0) i) => (u32vector))
   (my-check (u32vector-of-length-ec 1 (:range i 1) i) => (u32vector 0))
   (my-check (u32vector-of-length-ec 2 (:range i 2) i) => (u32vector 0 1))

   (my-check (s32vector-of-length-ec 1 1) => (s32vector 1))
   (my-check (s32vector-of-length-ec 0 (:range i 0) i) => (s32vector))
   (my-check (s32vector-of-length-ec 1 (:range i 1) i) => (s32vector 0))
   (my-check (s32vector-of-length-ec 2 (:range i 2) i) => (s32vector 0 1))

   (my-check (u64vector-of-length-ec 1 1) => (u64vector 1))
   (my-check (u64vector-of-length-ec 0 (:range i 0) i) => (u64vector))
   (my-check (u64vector-of-length-ec 1 (:range i 1) i) => (u64vector 0))
   (my-check (u64vector-of-length-ec 2 (:range i 2) i) => (u64vector 0 1))

   (my-check (s64vector-of-length-ec 1 1) => (s64vector 1))
   (my-check (s64vector-of-length-ec 0 (:range i 0) i) => (s64vector))
   (my-check (s64vector-of-length-ec 1 (:range i 1) i) => (s64vector 0))
   (my-check (s64vector-of-length-ec 2 (:range i 2) i) => (s64vector 0 1))

   )
