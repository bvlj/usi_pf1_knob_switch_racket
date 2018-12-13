#lang racket/gui

(require "gui-controller.rkt")

(provide gui-main)

(define (gui-main)
  ; Root frame
  (define root (new frame%
                    [label "Knob and Switch computer"]
                    [width 800]
                    [height 400]))
  ; Root panels
  (define main-panel (new horizontal-panel%
                          [parent root]))
  ; Panel for the main UI
  (define working-panel (new vertical-panel%
                             [parent main-panel]))
  ; Panel for displaying the memory contents
  (define memory-display-panel (new panel%
                                    [parent main-panel]))
  ; Main UI panels
  (define top-panel (new horizontal-panel%
                         [parent working-panel]))
  (define middle-panel (new horizontal-panel%
                            [parent working-panel]))
  (define bottom-panel (new horizontal-panel%
                            [parent working-panel]))
  ; Top panels
  (define c-bus-addr-panel (new group-box-panel%
                                [parent top-panel]
                                [label "Out Bus Address"]))
  (define registers-panel (new group-box-panel%
                               [parent top-panel]
                               [label "Register bank"]))
  (define ab-bus-addr-panel (new group-box-panel%
                                 [parent top-panel]
                                 [label "In Bus Address"]))
  ; Middle panels
  (define c-bus-panel (new vertical-panel%
                           [parent middle-panel]))
  (define alu-panel (new group-box-panel%
                         [parent middle-panel]
                         [label "ALU"]))
  (define ab-bus-panel (new vertical-panel%
                            [parent middle-panel]))
  ; Bottom panels
  (define btns-panel (new horizontal-panel%
                          [parent bottom-panel]))
  ; Register bank
  (define r0-input (new text-field%
                        [parent registers-panel]
                        [label "R0"]
                        [init-value "0"]))
  (define r1-input (new text-field%
                        [parent registers-panel]
                        [label "R1"]
                        [init-value "0"]))
  (define r2-input (new text-field%
                        [parent registers-panel]
                        [label "R2"]
                        [init-value "0"]))
  (define r3-input (new text-field%
                        [parent registers-panel]
                        [label "R3"]
                        [init-value "0"]))
  ; AB Bus Address
  (define a-bus-addr (new choice%
                          [parent ab-bus-addr-panel]
                          [label "A Bus Address"]
                          [choices LIST-OF-REGISTERS]))
  (define b-bus-addr (new choice%
                          [parent ab-bus-addr-panel]
                          [label "B Bus Address"]
                          [choices LIST-OF-REGISTERS]))
  ; C Bus Address
  (define c-bus-addr (new choice%
                          [parent c-bus-addr-panel]
                          [label ""]
                          [choices LIST-OF-REGISTERS]))

  ; C Bus Enabler
  (define c-bus-enabler (new check-box%
                             [parent c-bus-addr-panel]
                             [label "Enable C Bus"]
                             [value #t]))
  ; C Bus
  (define c-bus (new text-field%
                     [parent c-bus-panel]
                     [label "C Bus"]
                     [init-value "0"]))
  ; Memory
  (define memory-panel (new group-box-panel%
                            [parent c-bus-panel]
                            [label "Memory"]))
  (define memory-write-enabler (new check-box%
                                    [parent memory-panel]
                                    [label "Write memory"]
                                    [value #f]))
  (define memory-read-enabler (new check-box%
                                   [parent memory-panel]
                                   [label "Read memory"]
                                   [value #f]))
  (define memory-bus (new text-field%
                          [parent memory-panel]
                          [label "Memory Bus"]
                          [init-value "0"]))
  (define memory-display (new text-field%
                              [parent memory-display-panel]
                              [label "Memory content"]
                              [init-value "(empty)"]
                              [style (cons 'multiple (cons 'vertical-label '()))]))
  ; ALU panels
  (define alu-ab-panel (new horizontal-panel%
                            [parent alu-panel]))
  (define alu-op-panel (new panel%
                            [parent alu-panel]))
  (define alu-c-panel (new panel%
                           [parent alu-panel]))
  (define alu-enabler (new check-box%
                           [parent alu-panel]
                           [label "Enable ALU"]
                           [value #t]))
  ; ALU AB values
  (define alu-a-value (new text-field%
                           [parent alu-ab-panel]
                           [label "A"]
                           [init-value "0"]))
  (define alu-b-value (new text-field%
                           [parent alu-ab-panel]
                           [label "B"]
                           [init-value "0"]))
  ; ALU operation
  (define alu-op (new choice%
                      [parent alu-op-panel]
                      [label "Operation"]
                      [choices LIST-OF-ALU-OPERATIONS]))
  ; ALU C value
  (define alu-c-value (new text-field%
                           [parent alu-c-panel]
                           [label "C"]
                           [init-value "0"]))
  ; AB Bus
  (define a-bus (new text-field%
                     [parent ab-bus-panel]
                     [label "A Bus"]
                     [init-value "0"]))
  (define b-bus (new text-field%
                     [parent ab-bus-panel]
                     [label "B Bus"]
                     [init-value "0"]))
  ; Execute
  (define exec-btn (new button%
                        [parent btns-panel]
                        [label "Execute"]
                        [callback (lambda (b e) (on-exec))]))

  ; Stepper
  (define stepper-btn (new button%
                           [parent btns-panel]
                           [label "Next step"]
                           [callback (lambda (b e) (on-step))]))

  ; Load
  (define load-btn (new button%
                        [parent btns-panel]
                        [label "Load program"]
                        [callback (lambda (b e) (load-program))]))

  ;
  (define (on-exec)
    (after-sync)
    (exec-animate))

  ;
  (define (on-step)
    (after-sync)
    (exec-step))

  ; World-builder function
  (define (exec-step)
    (case (on-execute)
      [(0) (set-status-0)]
      [(1) (set-status-1)]
      [(2) (set-status-2)]
      [(3) (set-status-3)]
      [(4) (set-status-4)]))

  (define (exec-animate)
    ; Disable the btns
    (send btns-panel enable #f)
    ; Start the animation
    (sleep 0.2)
    (set-status-0)
    (sleep 0.2)
    (set-status-1)
    (sleep 0.2)
    (set-status-2)
    (sleep 0.2)
    (set-status-3)
    (sleep 0.2)
    (set-status-4)
    (sleep 0.2)
    ; Re-enable the btns
    (send btns-panel enable #t))

  (define (load-program)
    (reset)
    (memory-load-microprogram "test.csv")
    (print-memory)
    (exec-program))

  (define (exec-program)
    (cond [(not (is-off?))
           (parse-microinstruction (get-current-memory-instruction))
           (exec-program)]))

  (define (print-memory)
    (send memory-display set-value (memory-dump memory "")))

  (define (set-status-0)
    (pre-sync)
    (send a-bus set-value (get-register-value (send a-bus-addr get-selection)))
    (send b-bus set-value (get-register-value (send b-bus-addr get-selection))))

  (define (set-status-1)
    (send alu-a-value set-value (send a-bus get-value))
    (send alu-b-value set-value (send b-bus get-value)))

  (define (set-status-2)
    (cond
      [(send alu-enabler get-value)
       (send alu-c-value set-value
             (number->string (run-alu-operation (string?->number (send alu-a-value get-value))
                                                (string?->number (send alu-b-value get-value)))))]))

  (define (set-status-3)
    (cond
      [(send memory-read-enabler get-value) (send c-bus set-value (send memory-bus get-value))]
      [(send alu-enabler get-value) (send c-bus set-value (send alu-c-value get-value))]
      [(send memory-write-enabler get-value) (memory-write (send alu-c-value get-value))]))

  (define (set-status-4)
    (cond [(send c-bus-enabler get-value)
           (send (get-register (send c-bus-addr get-selection))
                 set-value (send c-bus get-value))])
    (after-sync))

  ; Registers utils
  (define (get-register-value v)
    (let [(register (get-register v))]
      (if (boolean? register) 0 (send register get-value))))

  (define (get-register v)
    (case v
      [(0) r0-input]
      [(1) r1-input]
      [(2) r2-input]
      [(3) r3-input]
      [else #f]))

  (define (parse-microinstruction instr)
    (cond
      [(string-prefix? instr "LOAD ") (run-load instr)]
      [(string-prefix? instr "STORE ") (run-store instr)]
      [(string-prefix? instr "MOVE ") (run-move instr)]
      [(string-prefix? instr "ADD ") (run-alu-op instr 0)]
      [(string-prefix? instr "SUB ") (run-alu-op instr 1)]
      [(string-prefix? instr "OR ") (run-alu-op instr 2)]
      [(string-prefix? instr "AND ") (run-alu-op instr 3)]
      [(string-prefix? instr "BRANCH ") (run-branch instr)]
      [(string-prefix? instr "BZERO ") (run-bzero instr)]
      [(string-prefix? instr "BNEG ") (run-bneg instr)]
      [(string-prefix? instr "HALT") (run-halt)]
      [else (run-nop)]))

  ; run-load: String -> #<void>
  ;    LOAD REG MEM
  ;
  ; Set the memory bus to the given value and store it
  ; onto the given register
  (define (run-load instr)
    (world-set (world-en-set #f #t #f #t))

    (world-set (world-out-bus-set (get-instr-value instr 1 #t)))
    (world-set (world-mem-bus-set (number?->string (memory-get-content
        (get-instr-value instr 2 #f)))))

    (increase-program-counter)
    (exec-animate))

  ; run-store: String -> #<void>
  ;    STORE REG MEM
  ;
  ; Store the given bus value to the given memory address
  ; through its bus
  (define (run-store instr)
    (world-set (world-en-set #t #f #t #f))

    (world-set (world-out-bus-set (get-instr-value instr 1 #t)))
    (world-set (world-alu-set 4))
    (memory-selector-set (get-instr-value instr 2 #f))

    (increase-program-counter)
    (exec-animate)

    ; Update the memory
    (memory-write (get-register-value (get-instr-value instr 1 #t)))
    (print-memory))

  ; run-move: String -> #<void>
  ;    MOVE REG-A REG-B
  ;
  ; Copy the value of a register to another
  (define (run-move instr)
    (world-set (world-en-set #t #f #f #t))

    (world-set (world-in-bus-set (get-instr-value instr 1 #t)
                                 (world-b-bus (world-get))))
    (world-set (world-out-bus-set (get-instr-value instr 2 #t)))
    (world-set (world-alu-set 4))

    (increase-program-counter)
    (exec-animate))

  ; run-alu-op: String Number -> #<void>
  ;
  ;    ADD REG-A REG-B REG-C
  ;    SUM REG-A REG-B REG-C
  ;    OR  REG-A REG-B REG-C
  ;    AND REG-A REG-B REG-C
  ;
  ; Set the alu operation and the bus so that the specified
  ; operation is executed and store it to a given register
  (define (run-alu-op instr op)
    (world-set (world-en-set #t #f #f #t))

    (world-set (world-in-bus-set (get-instr-value instr 1 #t)
                                 (get-instr-value instr 2 #t)))
    (world-set (world-out-bus-set (get-instr-value instr 3 #t)))
    (world-set (world-alu-set op))

    (increase-program-counter)
    (exec-animate))

  ; run-branch: String -> #<void>
  ;    BRANCH MEM
  ;
  ; Set the program counter to the given value
  (define (run-branch instr)
    (world-set (world-pc-set (get-instr-value instr 1 #f))))

  ; run-bzero: String -> #<void>
  ;    BZERO MEM
  ;
  ; Set the program counter to the given value if the alu result was 0
  (define (run-bzero instr)
    (cond [(= (string?->number (send alu-c-value get-value)) 0)
             (world-set (world-pc-set (get-instr-value instr 1 #f)))])
    (exec-animate))

  ; run-bneg: String -> #<void>
  ;    BNEG MEM
  ;
  ; Set the program counter to the given value if the alu result was less than 0
  (define (run-bneg instr)
    (cond [(< (string?->number (send alu-c-value get-value)) 0)
             (world-set (world-pc-set (get-instr-value instr 1 #f)))])
    (exec-animate))

  ; run-nop -> #<void>
  ;    NOP
  ;
  ; Do nothing
  (define (run-nop)
    (increase-program-counter))

  ; run-halt -> #<void>
  ;    HALT
  ;
  ; Halt the execution
  (define (run-halt)
    (halt-execution))

  ; pre-sync -> #<void>
  ; Sync the GUI components with the world before cycle execution
  (define (pre-sync)
    (let [(w (world-get))]
      ; Input registers
      (send r0-input set-value (number->string (world-r0 w)))
      (send r1-input set-value (number->string (world-r1 w)))
      (send r2-input set-value (number->string (world-r2 w)))
      (send r3-input set-value (number->string (world-r3 w)))
      ; Bus addresses
      (send a-bus-addr set-selection (world-a-bus w))
      (send b-bus-addr set-selection (world-b-bus w))
      (send c-bus-addr set-selection (world-c-bus w))
      (send memory-bus set-value (number?->string (world-mem-bus w)))
      ; Alu
      (send alu-op set-selection (world-alu w))
      ; Enablers
      (send alu-enabler set-value (world-en-alu w))
      (send memory-read-enabler set-value (world-en-read w))
      (send memory-write-enabler set-value (world-en-write w))
      (send c-bus-enabler set-value (world-en-c w))))

  ; after-sync -> #<void>
  ; Sync the world with the GUI components after cycle execution
  (define (after-sync)
    (world-set (world-r-set (string?->number (send r0-input get-value))
                            (string?->number (send r1-input get-value))
                            (string?->number (send r2-input get-value))
                            (string?->number (send r3-input get-value))))
    (world-set (world-in-bus-set (send a-bus-addr get-selection)
                                 (send b-bus-addr get-selection)))
    (world-set (world-out-bus-set (send c-bus-addr get-selection)))
    (world-set (world-mem-bus-set (string->number (send memory-bus get-value))))
    (world-set (world-en-set (send alu-enabler get-value)
                             (send memory-read-enabler get-value)
                             (send memory-write-enabler get-value)
                             (send c-bus-enabler get-value))))


  ; Show the window
  (send root show #t))
