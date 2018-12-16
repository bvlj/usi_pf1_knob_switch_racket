#lang racket
; Knob and switch computer: Controller
; This file contains the back-end of the knob and switch computer,
; it also contains unit tests for the logic part of the program
;
; Authors: Bevilacqua Joey
;          Brunner Nicola

(require rackunit)
(require 2htdp/batch-io)

; Provide

(provide (struct-out world)
         world-get
         world-set
         world-r-set
         world-in-bus-set
         world-out-bus-set
         world-mem-bus-set
         world-alu-set
         world-en-set
         world-status-set
         world-pc-set)

(provide LIST-OF-REGISTERS
         LIST-OF-ALU-OPERATIONS)

(provide memory
         reset
         memory-dump
         memory-get-content
         memory-load-microprogram
         memory-selector-set
         memory-write)

(provide halt-execution
         increase-program-counter)

(provide on-execute
         is-off?
         run-alu-operation
         get-current-memory-instruction)

(provide string?->number
         number?->string
         string-prefix?
         get-instr-value)

; Data

; List of available registers
(define LIST-OF-REGISTERS (list "R0" "R1" "R2" "R3"))
; List of available ALU operations
(define LIST-OF-ALU-OPERATIONS (list "A + B" "A - B" "A or B" "A and B" "A"))

; Default status, the computer is off
(define STATUS-OFF -1)
(define STATUS-HALT -2)

; World abstraction

; A world is a (make-world Register Register Register Register
;                          BusAddr BusAddr BusAddr MemBusAddr
;                          AluOp
;                          Boolean Boolean Boolean Boolean
;                          Status ProgramCounter)
; Where
; - A Register is a Number that indicates the value contained in a register
; - A BusAddr is the address of a register set to a bus
; - A MemBusAddr is the memory bus address
; - A AluOp determines which operation is being executed on the ALU
; - A Status is a Number that determines the execution progress
; - A ProgramCounter is a Number that contains the to-be-read/executed memory address
(define-struct world [r0 r1 r2 r3
                      a-bus b-bus c-bus mem-bus
                      alu
                      en-alu en-read en-write en-c
                      status pc])

; Abstraction world, used by the GUI components to read and save non-memory data
(define w (make-world 0 0 0 0 0 0 0 0 0 #t #f #f #t STATUS-OFF 0))

; world-get -> World
; Get the current world
(define (world-get) w)

; world-set: World -> #<void>
; Update the internal world holder
(define (world-set new-world)
  (set! w new-world))

; world-r-set: Register Register Register Register -> World
; Set the world registers values
(define (world-r-set a b c d)
  (make-world a b c d
              (world-a-bus w) (world-b-bus w) (world-c-bus w) (world-mem-bus w)
              (world-alu w) (world-en-alu w) (world-en-read w)
              (world-en-write w) (world-en-c w) (world-status w)
              (world-pc w)))

; world-in-bus-set: BusAddr BusAddr -> World
; Set the world input bus addresses
(define (world-in-bus-set a b)
  (make-world (world-r0 w) (world-r1 w) (world-r2 w) (world-r3 w)
              a b (world-c-bus w) (world-mem-bus w)
              (world-alu w) (world-en-alu w) (world-en-read w)
              (world-en-write w) (world-en-c w) (world-status w)
              (world-pc w)))

; world-out-bus-set: BusAddr -> World
; Set the world bus addresses
(define (world-out-bus-set c)
  (make-world (world-r0 w) (world-r1 w) (world-r2 w) (world-r3 w)
              (world-a-bus w) (world-b-bus w) c (world-mem-bus w)
              (world-alu w) (world-en-alu w) (world-en-read w)
              (world-en-write w) (world-en-c w) (world-status w)
              (world-pc w)))

; world-mem-bus-set: MemBusAddr -> World
; Set the world memory bus addresses
(define (world-mem-bus-set m)
  (make-world (world-r0 w) (world-r1 w) (world-r2 w) (world-r3 w)
              (world-a-bus w) (world-b-bus w) (world-c-bus w) m
              (world-alu w) (world-en-alu w) (world-en-read w)
              (world-en-write w) (world-en-c w) (world-status w)
              (world-pc w)))

; world-alu-set: AluOp -> World
; Set the world alu operation
(define (world-alu-set op)
  (make-world (world-r0 w) (world-r1 w) (world-r2 w) (world-r3 w)
              (world-a-bus w) (world-b-bus w) (world-c-bus w) (world-mem-bus w)
              op (world-en-alu w) (world-en-read w)
              (world-en-write w) (world-en-c w) (world-status w)
              (world-pc w)))

; world-en-set: Boolean Boolean Boolean Boolean -> World
; Set the world enablers status
(define (world-en-set alu-status read-status write-status c-status)
  (make-world (world-r0 w) (world-r1 w) (world-r2 w) (world-r3 w)
              (world-a-bus w) (world-b-bus w) (world-c-bus w) (world-mem-bus w)
              (world-alu w) alu-status read-status
              write-status c-status (world-status w)
              (world-pc w)))

; world-status-set: Status -> World
; Set the world status
(define (world-status-set s)
  (make-world (world-r0 w) (world-r1 w) (world-r2 w) (world-r3 w)
              (world-a-bus w) (world-b-bus w) (world-c-bus w) (world-mem-bus w)
              (world-alu w) (world-en-alu w) (world-en-read w)
              (world-en-write w) (world-en-c w) s
              (world-pc w)))

; world-pc-set ProgramCounter -> World
; Set the world program counter
(define (world-pc-set pc)
  (make-world (world-r0 w) (world-r1 w) (world-r2 w) (world-r3 w)
              (world-a-bus w) (world-b-bus w) (world-c-bus w) (world-mem-bus w)
              (world-alu w) (world-en-alu w) (world-en-read w)
              (world-en-write w) (world-en-c w) (world-status w)
              pc))

; Callbacks

; on-execute -> Number
; Execution callback.
; Move to the next status and return it
(define (on-execute)
  (next-status)
  (world-status w))

; is-off? -> Boolean
; Returns false if the program has halted the execution
(define (is-off?)
  (= (world-status w) STATUS-HALT))

; run-alu-operation: Number Number -> Number
; Run the ALU operation with the 2 given inputs
(define (run-alu-operation a b)
  (case (world-alu w)
    [(0) (+ a b)]
    [(1) (- a b)]
    [(2) (bitwise-ior a b)]
    [(3) (bitwise-and a b)]
    [(4) a]
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

; memory-insert: Instruction Number Number -> #<void>
; Inserts a MemoryValue to the memory vector in a given position
(define (memory-insert instruction executable position)
  (vector-set! memory position (make-memory-value (string?->number executable) instruction)))

; reset -> #<void>
; resets the memory content, program-counter and memory-selector
(define (reset)
  (world-set (world-status-set STATUS-OFF))
  (world-set (world-pc-set 0))
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

; get-current-memory-instruction -> String
; Returns the current memory instruction:
;      - HALT: if the execution has been stopped
;      - NOP: if the content is not an Instruction
;      - else: returns the Instruction
(define (get-current-memory-instruction)
  (if (= (world-status w) STATUS-HALT)
      ; OFF
      "HALT"
      ; ON
      (if (< (world-pc w) (vector-length memory))
          ; Valid PC
          (let [(mem-val (vector-ref memory (world-pc w)))]
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
  (halt-execution)
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
  (world-set (world-pc-set (add1 (world-pc w)))))

(define (halt-execution)
  (world-set (world-status-set STATUS-HALT)))

; Status

; next-status: Number -> #<void>
; Increase the status or eventually reset it
(define (next-status)
  (cond
    [(= (world-status w) 4)
        (world-set (world-status-set STATUS-OFF))]
    [(not (= (world-status w) STATUS-HALT))
        (world-set (world-status-set (add1 (world-status w))))]))

; Utils

; string?->number: String? -> Number
; where String? is one of:
;     - String
;     - Number
; Converts a potential String to a Number
(define (string?->number v)
  (if (number? v) v
      (if (string? v) (string->number v) 0)))

; number?->string: Number? -> String
; where Number? is one of:
;     - String
;     - Number
; Converts a potential Number to a String
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
  (world-set (world-alu-set 0))
  (check-eq? (run-alu-operation 1 2)
             3
             "run-alu-operation + failed")
  (world-set (world-alu-set 1))
  (check-eq? (run-alu-operation 2 1)
             1
             "run-alu-operation - failed")
  (world-set (world-alu-set 2))
  (check-eq? (run-alu-operation 32 12)
             44
             "run-alu-operation or failed")
  (world-set (world-alu-set 3))
  (check-eq? (run-alu-operation 3 2)
             2
             "run-alu-operation and failed")
  (world-set (world-alu-set 4))
  (check-eq? (run-alu-operation 1 2)
             1
             "run-alu-operation a failed")
  (world-set (world-alu-set 5))
  (check-eq? (run-alu-operation 1 2)
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
  ; get-current-memory-instruction
  (reset)
  (memory-insert "LOAD R0 1" 1 0)
  (memory-insert 12 0 1)
  (world-set (world-pc-set 0))
  (check-eq? (get-current-memory-instruction)
             "LOAD R0 1"
             "get-current-memory-instruction instruction [0] failed")
  (world-set (world-pc-set 1))
  (check-eq? (get-current-memory-instruction)
             "NOP"
             "get-current-memory-instruction data [1] failed")
  (world-set (world-pc-set 100))
  (check-eq? (get-current-memory-instruction)
             ""
             "get-current-memory-instruction out of bounds failed [1/2]")
  (check-eq? (world-status w)
             STATUS-HALT
             "get-current-memory-instruction out of bounds failed [2/2]")
  ; increase-program-counter
  (reset)
  (world-set (world-pc-set 18))
  (increase-program-counter)
  (check-eq? (world-pc w)
             19
             "increase-program-counter failed")
  ; halt-execution
  (reset)
  (halt-execution)
  (check-eq? (world-status w)
             STATUS-HALT
             "halt-execution failed")
  ; next-status
  (world-set (world-status-set 1))
  (next-status)
  (check-eq? (world-status w)
             2
             "next-status default failed")
  (world-set (world-status-set 4))
  (next-status)
  (check-eq? (world-status w)
             -1
             "next-status reset failed")
  ; string?->number
  (check-eq? (string?->number "123")
             123
             "string?->number string failed")
  (check-eq? (string?->number 123)
             123
             "string?->number number failed")
  ; number?->string
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
