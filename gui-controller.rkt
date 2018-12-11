#lang racket

(require rackunit)
(require 2htdp/batch-io)

; Provide

(provide LIST-OF-REGISTERS)
(provide LIST-OF-ALU-OPERATIONS)

(provide memory)

(provide reset)
(provide memory-write)
(provide memory-load-microprogram)
(provide memory-dump)
(provide memory-get-content)
(provide memory-selector-set)
(provide program-counter-set)

(provide halt-execution)
(provide increase-program-counter)

(provide on-execute)
(provide is-off?)
(provide run-alu-operation)
(provide get-current-memory-instruction)

(provide string?->number)
(provide number?->string)

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

; is-off? -> Boolean
; Returns false if the program has halted the execution
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

; Default memory value
(define MEMORY-VALUE-EMPTY (make-memory-value 0 0))

; Default content of the Memory (empty)
(define MEMORY-EMPTY (make-vector MEMORY-SIZE MEMORY-VALUE-EMPTY))

; A Memory is a Vector<MemoryValue>
(define memory MEMORY-EMPTY)

; For reading the file
(define memory-selector 0)

; PC (Program Counter)
(define program-counter 0)

; memory-insert: Instruction Number Number -> #<void>
; Inserts a MemoryValue to the memory vector in a given position
(define (memory-insert instruction executable position)
  (vector-set! memory position (make-memory-value (string?->number executable) instruction)))

; reset -> #<void>
; resets the memory content, program-counter and memory-selector
(define (reset)
  (set! status STATUS-OFF)
  (set! program-counter 0)
  (set! memory-selector 0)
  (vector-fill! memory MEMORY-VALUE-EMPTY))

; memory-write -> #<void>
; Writes a value to the memory to the position given by memory-selector
(define (memory-write data)
  (cond [(> memory-selector -1) (memory-insert (string?->number data) 0 memory-selector)]))

; memory-load-microprogram: String -> #<void>
; Given a file path, reads the .csv file and loads it to the memory
(define (memory-load-microprogram path)
  (csv-list->memory (read-csv-file path)))

; memory-get-content: Number -> Number
; Given a position returns the content of that position in memory
(define (memory-get-content position)
  (string?->number (memory-value-content (vector-ref memory position))))

; memory-dump: Vector<MemoryValue> String -> String
; Return a string containing the memory contents
(define (memory-dump vec str)
  (if (= (vector-length vec) 0)
      str
      (memory-dump (vector-drop vec 1)
                   (string-append str
                                  (number?->string (memory-value-content (vector-ref vec 0)))
                                  "\n"))))

; memory-selector-set: Number -> #<void>
; Public memory-selector setter
(define (memory-selector-set val)
  (set! memory-selector val))

; program-counter-set: Number -> #<void>
; Public program-counter setter
(define (program-counter-set val)
  (set! program-counter val))

; get-current-memory-instruction -> String
; Returns the current memory instruction:
;      - HALT: if the execution has been stopped
;      - NOP: if the content is not an Instruction
;      - else: returns the Instruction
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
                "NOP"))
          ; Invalid PC
          (on-invalid-pc))))

; on-invalid-pc -> String
; Set the status to HALT when the program-counter is invalid
(define (on-invalid-pc)
  (set! status STATUS-HALT)
  "")

; csv-line->memory-value: List<String> -> #<void>
; Insterts to the memory the content of the csv line
(define (csv-line->memory-value line)
  (memory-insert (second line) (first line) memory-selector)
  (set! memory-selector (add1 memory-selector)))

; csv-list->memory: List<List<String>> -> #<void>
; Is a recursive function which helps reading the .csv file
(define (csv-list->memory list)
  (cond
    [(not (empty? list))
     (csv-line->memory-value (first list))
     (csv-list->memory (rest list))]))

; increase-program-counter -> #<void>
; Increase the program-counter after a cycle execution
(define (increase-program-counter)
  (set! program-counter (add1 program-counter)))

(define (halt-execution)
  (set! status STATUS-HALT))

; Status

; next-status: Number -> #<void>
; Increase the status or eventually reset it
(define (next-status s)
  (cond
    [(= s 4) (set! status STATUS-OFF)]
    [(not (= s STATUS-HALT)) (set! status (add1 s))]))

; Utils

; string?->number: String? -> Number
; where String? is one of:
;     - String
;     - Number
; Converts a potentially String to a Number
(define (string?->number v)
  (if (number? v) v
      (if (string? v) (string->number v) 0)))

; number?->string: Number? -> String
; where Number? is one of:
;     - String
;     - Number
; Converts a potentially Number to a String
(define (number?->string v)
  (if (string? v) v
      (if (number? v) (number->string v) "")))

; get-instr-value: String Number Boolean -> Number
; Returs the register number if the instruction contains a register
; otherwise returns the value
(define (get-instr-value inst position is-register)
  (let [(l (string-split inst))]
    (string->number (if (eqv? is-register #t) (substring (list-ref l position) 1 2)
                        (list-ref l position)))))

; Unit tests
(module+ test
  (require rackunit)

  ; on-execute
  (reset)
  (check-eq? (on-execute)
             0
             "on-execute failed")
  ; is-off?
  (halt-execution)
  (check-eq? (is-off?)
             #t
             "is-off? true failed")
  (reset)
  (check-eq? (is-off?)
             #f
             "is-off? false failed")
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
  ; memory-insert
  (memory-insert "HELLO" 1 1)
  (check-eq? (memory-value-content (vector-ref memory 1))
             "HELLO"
             "memory-insert failed")
  ; reset
  (memory-insert "HELLO" 1 3)
  (reset)
  (check-eq? (memory-value-content (vector-ref memory 1))
          0
          "reset failed")
  ; memory-write
  (reset)
  (memory-selector-set 2)
  (memory-write 5)
  (check-eq? (memory-value-content (vector-ref memory memory-selector))
             5
             "memory-write failed")
  ; memory-selector-set
  (reset)
  (memory-selector-set 13)
  (check-eq? memory-selector
             13
             "memory-selector-set failed")
  ; program-counter-set
  (reset)
  (program-counter-set 18)
  (check-eq? program-counter
             18
             "program-counter-set failed")
  ; get-current-memory-instruction
  (reset)
  (memory-insert "LOAD R0 1" 1 0)
  (memory-insert 12 0 1)
  (program-counter-set 0)
  (check-eq? (get-current-memory-instruction)
             "LOAD R0 1"
             "get-current-memory-instruction instruction [0] failed")
  (program-counter-set 1)
  (check-eq? (get-current-memory-instruction)
             "NOP"
             "get-current-memory-instruction data [1] failed")
  (program-counter-set 100)
  (check-eq? (get-current-memory-instruction)
             ""
             "get-current-memory-instruction out of bounds failed [1/2]")
  (check-eq? status
             STATUS-HALT
             "get-current-memory-instruction out of bounds failed [2/2]")
  ; increase-program-counter
  (reset)
  (program-counter-set 18)
  (increase-program-counter)
  (check-eq? program-counter
             19
             "increase-program-counter failed")
  ; halt-execution
  (reset)
  (halt-execution)
  (check-eq? status
             STATUS-HALT
             "halt-execution failed")
  ; next-status
  (next-status 1)
  (check-eq? status
             2
             "next-status default failed")
  (next-status 4)
  (check-eq? status
             -1
             "next-status reset failed")
  ; string?->number
  (check-eq? (string?->number "123")
             123
             "string?->number string failed")
  (check-eq? (string?->number 123)
             123
             "string?->number number failed")
  number?->string
  (check-equal? (number?->string 4)
                "4"
                "number?->string number failed")
  (check-eq? (number?->string "123")
             "123"
             "number?->string string failed")
  ; get-instr-value
  (check-eq? (get-instr-value "LOAD R0 20" 1 #t)
             0
             "get-instr-value register failed")
  (check-eq? (get-instr-value "LOAD R0 20" 2 #f)
             20
             "get-instr-value memory failed")
  ; Reset for real-world execution
  (reset))
