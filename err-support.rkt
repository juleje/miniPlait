#lang racket

;; =============================================================================
;; Typed Interpreter: err-support.rkt
;; =============================================================================

(struct exn:fail:type exn:fail ())
(struct exn:fail:interp exn:fail ())

(define (raise-type-error msg)
  (raise (exn:fail:type msg (current-continuation-marks))))

(define (raise-interp-error msg)
  (raise (exn:fail:interp msg (current-continuation-marks))))

(provide (struct-out exn:fail:type)
         (struct-out exn:fail:interp)
         raise-type-error
         raise-interp-error)