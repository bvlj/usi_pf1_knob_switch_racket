#lang racket/gui

(require "gui-controller.rkt")

(provide gui-main)

(define (gui-main)
  ; Root frame
  (define root (new frame%
                    [label "Knob and Switch computer"]
                    [width 400]
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
                              [label "Memory"]
                              [init-value "(empty)"]
                              [style (cons 'multiple '())]))
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
                        [callback (lambda (b e) (exec-animate))]))

  ; Stepper
  (define stepper-btn (new button%
                           [parent btns-panel]
                           [label "Next step"]
                           [callback (lambda (b e) (exec-step))]))

  ; Load
  (define load-btn (new button%
                        [parent btns-panel]
                        [label "Load program"]
                        [callback (lambda (b e) (load-program))]))

  ; World-builder function
  (define (exec-step)
    (let [(status (on-execute))]
      (cond
        [(= status 0) (set-status-0)]
        [(= status 1) (set-status-1)]
        [(= status 2) (set-status-2)]
        [(= status 3) (set-status-3)]
        [(= status 4) (set-status-4)])))

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
    (send a-bus set-value (get-register-value (send a-bus-addr get-selection)))
    (send b-bus set-value (get-register-value (send b-bus-addr get-selection))))

  (define (set-status-1)
    (send alu-a-value set-value (send a-bus get-value))
    (send alu-b-value set-value (send b-bus get-value)))

  (define (set-status-2)
    (cond
      [(send alu-enabler get-value)
       (send alu-c-value set-value
             (number->string (run-alu-operation (send alu-op get-selection)
                                                (string?->number (send alu-a-value get-value))
                                                (string?->number (send alu-b-value get-value)))))]))

  (define (set-status-3)
    (cond
      [(send memory-read-enabler get-value) (send c-bus set-value (send memory-bus get-value))]
      [(send alu-enabler get-value) (send c-bus set-value (send alu-c-value get-value))]
      [(send memory-write-enabler get-value) (memory-write (send alu-c-value get-value))]))

  (define (set-status-4)
    (cond [(send c-bus-enabler get-value)
           (send (get-register (send c-bus-addr get-selection))
                 set-value (send c-bus get-value))]))

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
    (run-pre #f #t #f #t)

    (send c-bus-addr set-selection (get-instr-value instr 1 #t))
    (send memory-bus set-value (number?->string (memory-get-content (get-instr-value instr 2 #f))))

    (increase-program-counter)
    (exec-animate))

  ; run-store: String -> #<void>
  ;    STORE REG MEM
  ;
  ; Store the given bus value to the given memory address
  ; through its bus
  (define (run-store instr)
    (run-pre #t #f #t #f)

    (send c-bus-addr set-selection (get-instr-value instr 1 #t))
    (send alu-op set-selection 4)
    (memory-selector-set (get-instr-value instr 2 #f))

    (increase-program-counter)
    (exec-animate)
    (memory-write (get-register-value (get-instr-value instr 1 #t)))
    (print-memory))

  ; run-move: String -> #<void>
  ;    MOVE REG-A REG-B
  ;
  ; Copy the value of a register to another
  (define (run-move instr)
    (run-pre #t #f #f #t)

    (send a-bus-addr set-selection (get-instr-value instr 1 #t))
    (send c-bus-addr set-selection (get-instr-value instr 2 #t))
    (send alu-op set-selection 4)

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
    (run-pre #t #f #f #t)

    (send alu-op set-selection op)
    (send a-bus-addr set-selection (get-instr-value instr 1 #t))
    (send b-bus-addr set-selection (get-instr-value instr 2 #t))
    (send c-bus-addr set-selection (get-instr-value instr 3 #t))

    (increase-program-counter)
    (exec-animate))

  ; run-branch: String -> #<void>
  ;    BRANCH MEM
  ;
  ; Set the program counter to the given value
  (define (run-branch instr)
    (program-counter-set (get-instr-value instr 1 #f)))

  ; run-bzero: String -> #<void>
  ;    BZERO MEM
  ;
  ; Set the program counter to the given value if the alu result was 0
  (define (run-bzero instr)
    (cond [(= (string?->number (send alu-c-value get-value)) 0)
             (program-counter-set (get-instr-value instr 1 #f))])
    (exec-animate))

  ; run-bneg: String -> #<void>
  ;    BNEG MEM
  ;
  ; Set the program counter to the given value if the alu result was less than 0
  (define (run-bneg instr)
    (cond [(< (string?->number (send alu-c-value get-value)) 0)
             (program-counter-set (get-instr-value instr 1 #f))])
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

  ; run-pre: Boolean Boolean Boolean Boolean -> #<void>
  ;
  ; Setup the circuits status before executing a cycle
  (define (run-pre alu-status read-status write-status c-status)
    (send alu-enabler set-value alu-status)
    (send memory-read-enabler set-value read-status)
    (send memory-write-enabler set-value write-status)
    (send c-bus-enabler set-value c-status))

  ; Show the window
  (send root show #t))
