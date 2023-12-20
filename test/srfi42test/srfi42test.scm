; <PLAINTEXT>
; Examples for Eager Comprehensions in [outer..inner|expr]-Convention
; ===================================================================
;
; sebastian.egner@philips.com, Eindhoven, The Netherlands, 26-Dec-2007.
; Scheme R5RS (incl. macros), SRFI-23 (error).
; 
; Joseph Donaldson, 2023
; split original tests across multiple modules to work around maximum
; method size constraints imposed by the jvm.

(module srfi42test
   (library srfi42)
   (include "test_macros.sch")
   (import test_utils
           do_ec_tests
           list_ec_and_basis_qualifier_tests
           other_comprehensions_tests
           homogenous_vector_ec_tests
           homogenous_vector_of_length_ec_tests
           type_generators_tests
           special_generators_tests
           dispatching_generator_tests
           index_variable_tests
           srfi_examples_tests))


(do-ec-tests)

(list-ec-and-basic-qualifiers-tests)

(other-comprehensions-tests)

(homogenous-vector-ec-tests)

(homogenous-vector-of-length-ec-tests)

(typed-generators-tests)

(special-generators-tests)

(dispatching-generator-tests)

(index-variable-tests)

(srfi-examples-tests)

; ==========================================================================
; Summary
; ==========================================================================

(begin
  (newline)
  (newline)
  (display "correct examples : ")
  (display my-check-correct)
  (newline)
  (display "wrong examples   : ")
  (display my-check-wrong)
  (newline)
  (newline) )
