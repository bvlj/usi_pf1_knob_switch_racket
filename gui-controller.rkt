#lang racket/gui

(require 2htdp/universe)

; Provide

(provide LIST-OF-REGISTERS)
(provide LIST-OF-ALU-OPERATIONS)

(provide world)
(provide build-world)

(provide on-execute)
(provide on-halt)
(provide run-alu-operation)

; Data

(define LIST-OF-REGISTERS (list "R0" "R1" "R2" "R3"))
(define LIST-OF-ALU-OPERATIONS (list "A + B" "A - B" "A or B" "A and B"))


; World

(define STATUS-OFF -1)
(define status STATUS-OFF)

(define-struct world [r0 r1 r2 r3 a b c op status])

(define (build-world r0 r1 r2 r3 a b c op)
  (make-world r0 r1 r2 r3 a b c op status))


; Callbacks

(define (on-execute _)
  (next-status status)
  status)

(define (on-halt _)
  (set! status STATUS-OFF))

(define (run-alu-operation op a b)
  (cond
    [(= op 0) (+ a b)]
    [(= op 1) (- a b)]
    [(= op 2) (bitwise-ior a b)]
    [(= op 3) (bitwise-and a b)]
    [else 0]))

; Status

(define (next-status s)
  (cond
    [(= s 4) (set! status STATUS-OFF)]
    [else (set! status (add1 s))]))
