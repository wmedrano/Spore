(define %exit? (box false))

;; Exit.
;;
;; Note that exiting does not happen immediately.
(define (exit!)
  (box-set! %exit? true))

;; Handle `event` globally.
(define (handle-event! event)
  (when (= event "<c-q>")
    (exit!)
    (return))
  (window-handle-event! (active-window) event))
