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
  (define alu-op (new choice%
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
                        [label "Execute"]
                        [callback (lambda (b e) (exec-animate #f))]))

  ; Halth
  (define stepper-btn (new button%
                           [parent btns-panel]
                           [label "Next step"]
                           [callback (lambda (b e) (exec-step #f))]))

  ; World-builder function
  (define (exec-step _)
    (let [(status (on-execute 0))]
       (cond
         [(= status 0) (set-status-0 0)]
         [(= status 1) (set-status-1 0)]
         [(= status 2) (set-status-2 0)]
         [(= status 3) (set-status-3 0)]
         [(= status 4) (set-status-4 0)])))

  (define (exec-animate _)
    ; Disable the btns
    (send btns-panel enable #f)
    ; Start the animation
    (set-status-0 0)
    (sleep 1)
    (set-status-1 0)
    (sleep 1)
    (set-status-2 0)
    (sleep 1)
    (set-status-3 0)
    (sleep 1)
    (set-status-4 0)
    (sleep 1)
    ; Re-enable the btns
    (send btns-panel enable #t))


  (define (set-status-0 world)
    (send a-bus set-value (get-register-value (send a-bus-addr get-selection)))
    (send b-bus set-value (get-register-value (send b-bus-addr get-selection))))

  (define (set-status-1 world)
    (send alu-a-value set-value (send a-bus get-value))
    (send alu-b-value set-value (send b-bus get-value)))

  (define (set-status-2 world)
    (send alu-c-value set-value (number->string (run-alu-operation
                                                 (send alu-op get-selection)
                                                  (string?->number (send alu-a-value get-value))
                                                  (string?->number (send alu-b-value get-value))))))

  (define (set-status-3 world)
    (send c-bus set-value (send alu-c-value get-value)))

  (define (set-status-4 world)
    (let [(register (get-register (send c-bus-addr get-selection)))]
     (send register set-value (send c-bus get-value))))

  ; Registers utils
  (define (get-register-value v)
    (let [(register (get-register v))]
      (if (boolean? register) 0 (send register get-value))))

  (define (get-register v)
    (cond
      [(= v 0) r0-input]
      [(= v 1) r1-input]
      [(= v 2) r2-input]
      [(= v 3) r3-input]
      [else #f]))

  ; Show the window
  (send root show #t))

(define (string?->number v)
  (if (number? v) v
      (if (string? v) (string->number v) 0)))
