(define %exit? (box false))

;; Exit.
;;
;; Note that exiting does not happen immediately.
(define (exit!)
  (box-set! %exit? true))

(define %modal-window (box false))
(define (toggle-modal!)
  (define modal-window (unbox %modal-window))
  (when modal-window
      (window-remove-window! modal-window)
      (box-set! %modal-window false)
      (return))
  (define new-modal (window-new-modal "TEST MODAL"))
  (box-set! %modal-window new-modal)
  (window-add-window! new-modal))

;; Handle `event` globally.
(define (handle-event! event)
  (when (= event "<c-q>")
    (exit!)
    (return))
  (when (= event "<esc>")
    (toggle-modal!)
    (return))
  (window-handle-event! (active-window) event))
