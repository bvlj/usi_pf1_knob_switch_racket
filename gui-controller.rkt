#lang racket/gui

(require 2htdp/universe)

; Provide

(provide LIST-OF-REGISTERS)
(provide LIST-OF-ALU-OPERATIONS)

(provide world)
(provide world-status)
(provide build-world)

(provide on-execute)
(provide run-alu-operation)

; Data

(define LIST-OF-REGISTERS (list "R0" "R1" "R2" "R3"))
(define LIST-OF-ALU-OPERATIONS (list "A + B" "A - B" "A or B" "A and B"))


; World

(define-struct world [r0 r1 r2 r3 a b c op status])

(define (build-world r0 r1 r2 r3 a b c op)
  (make-world r0 r1 r2 r3 a b c op -2))

; Callbacks

(define (on-execute w)
  (next-status w))

(define (on-halt w)
  (set-status w -2))

(define (run-alu-operation op a b)
  (cond
    [(= op 0) (+ a b)]
    [(= op 1) (- a b)]
    [(= op 2) (bitwise-ior a b)]
    [(= op 3) (bitwise-and a b)]
    [else 0]))

; Private helper functions

(define (next-status w)
  (let [(status (world-status w))]
    (cond
      [(= status 4) (set-status w -2)]
      [else (set-status w (add1 (world-status w)))])))

(define (set-status w s)
  (make-world (world-r0 w)
              (world-r1 w)
              (world-r2 w)
              (world-r3 w)
              (world-a w)
              (world-b w)
              (world-c w)
              (world-op w)
              s))
