#lang racket

(require rackunit)

; Provide

(provide LIST-OF-REGISTERS)
(provide LIST-OF-ALU-OPERATIONS)

(provide world)
(provide build-world)

(provide on-execute)
(provide run-alu-operation)

; Data

; List of available registers
(define LIST-OF-REGISTERS (list "R0" "R1" "R2" "R3"))
; List of available ALU operations
(define LIST-OF-ALU-OPERATIONS (list "A + B" "A - B" "A or B" "A and B"))

; Default status, the computer is off
(define STATUS-OFF -1)
; Current status of execution
; A status is an Number
(define status STATUS-OFF)

; A world is a (make-world Number Number Number Number
;                          String String String Number Number)
; Where r0, r1, r2 and r3 are the values assigned to the given registers
;       a b c are the bus addresses (reference to the register)
;       op is the ALU operation
;       status is the current execution step
(define-struct world [r0 r1 r2 r3 a b c op status])

; Public world builder
; status is handled internally
(define (build-world r0 r1 r2 r3 a b c op)
  (make-world r0 r1 r2 r3 a b c op status))


; Callbacks

; on-execute -> Number
; Execution callback.
; Move to the next status and return it
(define (on-execute _)
  (next-status status)
  status)

; run-alu-operation: Number Number Number -> Number
; Run the ALU operation with the 2 given inputs
(define (run-alu-operation op a b)
  (cond
    [(= op 0) (+ a b)]
    [(= op 1) (- a b)]
    [(= op 2) (bitwise-ior a b)]
    [(= op 3) (bitwise-and a b)]
    [else 0]))

; Status

; Increase the status or eventually reset it
(define (next-status s)
  (cond
    [(= s 4) (set! status STATUS-OFF)]
    [else (set! status (add1 s))]))


; Unit tests

; build-world
(set! status 2)
(check-eq? (world-status (build-world 1 2 3 4 "R0" "R2" "R3" 0))
           2
           "build-world test failed")
; on-execute
(set! status STATUS-OFF)
(check-eq? (on-execute #f)
           0
           "on-execute failed")
; run-alu-operation
(check-eq? (run-alu-operation 0  1  2)
           3
           "run-alu-operation + failed")
(check-eq? (run-alu-operation 1  2  1)
           1
           "run-alu-operation - failed")
(check-eq? (run-alu-operation 2 32 12)
           44
           "run-alu-operation or failed")
(check-eq? (run-alu-operation 3 3 2)
           2
           "run-alu-operation and failed")
(check-eq? (run-alu-operation 4  1  2)
           0
           "run-alu-operation unexpected failed")
; next-status
(next-status 1)
(check-eq? status
           2
           "next-status default failed")
(next-status 4)
(check-eq? status
           -1
           "next-status reset failed")

; Reset the status for real-world execution
(set! status STATUS-OFF)
