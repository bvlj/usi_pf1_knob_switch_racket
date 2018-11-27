#lang racket/gui

(define LIST-OF-REGISTERS (list "R0" "R1" "R2" "R3"))
(define LIST-OF-ALU-OPERATIONS (list "A + B" "A - B" "A or B" "A and B"))

(define (gui-main)
    ; Root frame
    (define root (new frame%
                      [label "Knob and Switch computer"]
                      [width 400]
                      [height 400]))
    ; Root panels
    (define top-panel (new horizontal-panel%
                           [parent root]))
    (define middle-panel (new horizontal-panel%
                              [parent root]))
    (define bottom-panel (new horizontal-panel%
                              [parent root]))
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
    ; C Bus
    (define c-bus (new text-field%
                       [parent c-bus-panel]
                       [label "C Bus"]))
    ; ALU panels
    (define alu-ab-panel (new horizontal-panel%
                              [parent alu-panel]))
    (define alu-op-panel (new panel%
                              [parent alu-panel]))
    (define alu-c-panel (new panel%
                             [parent alu-panel]))
    ; ALU AB values
    (define alu-a-value (new text-field%
                             [parent alu-ab-panel]
                             [label "A"]))
    (define alu-b-value (new text-field%
                             [parent alu-ab-panel]
                             [label "B"]))
    ; ALU operation
    (define alu-addr (new choice%
                          [parent alu-op-panel]
                          [label "Operation"]
                          [choices LIST-OF-ALU-OPERATIONS]))
   ; ALU C value
    (define alu-c-value (new text-field%
                             [parent alu-c-panel]
                             [label "C"]))
    ; AB Bus
    (define a-bus (new text-field%
                       [parent ab-bus-panel]
                       [label "A Bus"]))
    (define b-bus (new text-field%
                       [parent ab-bus-panel]
                       [label "B Bus"]))
    ; Execute
    (define exec-btn (new button%
                          [parent btns-panel]
                          [label "Execute"]))
    ; Show the window
    (send root show #t))

(module+ main (gui-main))
