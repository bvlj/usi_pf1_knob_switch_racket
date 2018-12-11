#lang racket

(require rackunit)
(require 2htdp/batch-io)

; Provide

(provide LIST-OF-REGISTERS)
(provide LIST-OF-ALU-OPERATIONS)

(provide memory)

(provide memory-reset)
(provide memory-load-microprogram)
(provide memory-dump)
(provide memory-write)

(provide assembly-pre)
(provide assembly-after)
(provide assembly-load)
(provide assembly-store)
(provide assembly-move)
(provide assembly-alu-operation)
(provide assembly-branch)
(provide assembly-bzero)
(provide assembly-bneg)
(provide assembly-halt)

(provide on-execute)
(provide is-off?)
(provide run-alu-operation)
(provide get-current-memory-instruction)

(provide string?->number)
(provide string-prefix?)
(provide get-instr-value)

; Data

; List of available registers
(define LIST-OF-REGISTERS (list "R0" "R1" "R2" "R3"))
; List of available ALU operations
(define LIST-OF-ALU-OPERATIONS (list "A + B" "A - B" "A or B" "A and B" "A"))

; Default status, the computer is off
(define STATUS-OFF -1)
(define STATUS-HALT -2)
; Current status of execution
; A status is an Number
(define status STATUS-OFF)

; Callbacks

; on-execute -> Number
; Execution callback.
; Move to the next status and return it
(define (on-execute)
  (next-status status)
  status)

(define (is-off?)
  (= status STATUS-HALT))

; run-alu-operation: Number Number Number -> Number
; Run the ALU operation with the 2 given inputs
(define (run-alu-operation op a b)
  (cond
    [(= op 0) (+ a b)]
    [(= op 1) (- a b)]
    [(= op 2) (bitwise-ior a b)]
    [(= op 3) (bitwise-and a b)]
    [(= op 4) a]
    [else 0]))

; Memory

; A MemoryValue is a (make-memory-value MemoryExecutable MemoryContent)
; A MemoryExecutable is a Boolean: #t if the memoryContent is an instruction, #f if it contains data
; A MemoryContent is one of
;     - String: Instruction
;     - Number: Data
(define-struct memory-value [executable content])

; Memory size
(define MEMORY-SIZE 32)
(define MEMORY-EMPTY (make-vector MEMORY-SIZE (make-memory-value 0 0)))
(define memory MEMORY-EMPTY)

; For reading the file
(define memory-selector 0)
; PC
(define program-counter 0)

(define (memory-insert instruction executable position)
  (vector-set! memory position (make-memory-value (string?->number executable) instruction)))

(define (memory-reset)
  (set! program-counter 0)
  (set! memory-selector 0)
  (set! memory MEMORY-EMPTY))

(define (memory-write data)
  (cond [(> memory-selector -1) (memory-insert (string?->number data) 0 memory-selector)]))

(define (memory-load-microprogram path)
  (csv-list->memory (read-csv-file path)))

(define (memory-get-content position)
  (string?->number (memory-value-content (vector-ref memory position))))

; return a string containing the memory contents
(define (memory-dump vec str count)
  (if (= (vector-length vec) 0)
      str
      (memory-dump (vector-drop vec 1)
                   (string-append str
                                  (number->string (memory-value-executable (vector-ref vec 0)))
                                  "  "
                                  (number?->string (memory-value-content (vector-ref vec 0)))
                                  "\n")
                   (add1 count))))

; get-current-memory-instruction: -> String
(define (get-current-memory-instruction)
  (if (= status STATUS-HALT)
      ; OFF
      "HALT"
      ; ON
      (if (< program-counter (vector-length memory))
          ; Valid PC
          (let [(mem-val (vector-ref memory program-counter))]
            (if (= (memory-value-executable mem-val) 1)
                ; Executable
                (memory-value-content mem-val)
                ; Data
                "data"))
          ; Invalid PC
          (on-invalid-pc))))

(define (on-invalid-pc)
  (set! status STATUS-HALT)
  "")

(define (csv-line->memory-value line)
  (memory-insert (second line) (first line) memory-selector)
  (set! memory-selector (add1 memory-selector)))

(define (csv-list->memory list)
  (cond
    [(not (empty? list))
     (csv-line->memory-value (first list))
     (csv-list->memory (rest list))]))

; Assembly interpreter

; assembly-pre: ...
;
; Setup the (en|dis)abled circuit parts for the assembly instruction
(define (assembly-pre alu alu-status mem-read mem-read-status mem-write
                      mem-write-status c-bus c-bus-status)
  (send alu set-value alu-status)
  (send mem-read set-value mem-read-status)
  (send mem-write set-value mem-write-status)
  (send c-bus set-value c-bus-status))

; assembly-after: Boolean -> #<void>
; Callback for instruction execution
(define (assembly-after has-branched)
  (cond [(not has-branched) (set! program-counter (add1 program-counter))]))

; Assembly interpreter functions set up the world so that an execution
; results in the required action

; assembly-load: Register Number MemoryBus Number -> #<void>
;    LOAD REG MEM
;
; Set the memory bus to the given value and store it
; onto the given register
(define (assembly-load reg reg-position mem-bus mem-position)
  (send mem-bus set-value (number?->string (memory-get-content mem-position)))
  (send reg set-selection reg-position))

; assembly-store: Bus Number Bus Number Alu Number -> #<void>
;    STORE REG MEM
;
; Store the given bus value to the given memory address
; through its bus
(define (assembly-store reg reg-position alu mem-position)
  (send reg set-selection reg-position)
  (set! memory-selector mem-position)
  (send alu set-selection 4))

; assembly-move: Bus Number Bus Number Alu -> #<void>
;    MOVE REG-A REG-B
;
; Set the destination bus to 0, sum it with the original bus value
; and store the result to the destination bus (results in a copy of the original
; bus value)
(define (assembly-move a a-position c c-position alu)
  (send a set-selection a-position)
  (send c set-selection c-position)
  (send alu set-selection 4))

; assembly-alu-operation ALU AluOperation BusAddr Number BusAddr Number
;    ADD REG-A REG-B REG-C
;    SUM REG-A REG-B REG-C
;    OR  REG-A REG-B REG-C
;    AND REG-A REG-B REG-C
;
; Set the alu operation and the bus so that the specified
; operation is executed and store it to a given register
(define (assembly-alu-operation alu alu-op a a-position b b-position c c-position)
  (send alu set-selection alu-op)
  (send a set-selection a-position)
  (send b set-selection b-position)
  (send c set-selection c-position))

; assembly-branch: MemPosition
;    BRANCH MEM
;
; Set the program counter to the given value
(define (assembly-branch mem-position)
  (set! program-counter mem-position))

; assembly-bzero: Alu Register MemPosition
;    BZERO MEM
;
; Set the program counter to the given value if the alu result was 0
(define (assembly-bzero alu-result mem-position)
  (cond [(= (string?->number alu-result) 0)
         (assembly-branch mem-position)]))

(define (assembly-bneg alu-result mem-position)
  (cond [(< (string?->number alu-result) 0)
         (assembly-branch mem-position)]))

(define (assembly-halt)
  (set! status -2))

; Status

; Increase the status or eventually reset it
(define (next-status s)
  (cond
    [(= s 4) (set! status STATUS-OFF)]
    [(not (= s STATUS-HALT)) (set! status (add1 s))]))

; Utils

(define (string?->number v)
  (if (number? v) v
      (if (string? v) (string->number v) 0)))

(define (number?->string v)
  (if (string? v) v
      (if (number? v) (number->string v) "")))

; get-instr-value: String Number Boolean -> Number
(define (get-instr-value inst position is-register)
  (let [(l (string-split inst))]
    (string->number (if (eqv? is-register #t) (substring (list-ref l position) 1 2)
                        (list-ref l position)))))

; Unit tests

; on-execute
(set! status STATUS-OFF)
(check-eq? (on-execute)
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
           1
           "run-alu-operation a failed")
(check-eq? (run-alu-operation 5  1  2)
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
