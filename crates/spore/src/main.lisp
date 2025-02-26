(define %exit? (box false))
(define text-window (struct :text (new-rope "") :cursor 0))
(define windows (list text-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Public" functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exit.
;;
;; Note that exiting does not happen immediately.
(define (exit!)
    (box-set! %exit? true))

;; The currently active window.
(define (active-window)
    text-window)

;; Move the cursor on `window` by the given `amount`. `amount` may be
;; positive or negative to move forwards or backwards, respectively.
(define (window-move-cursor! window amount)
    (define new-cursor (rope-normalize-cursor (struct-get window :text)
                                              (+ amount
                                                 (struct-get window :cursor))))
    (struct-set! window :cursor new-cursor))

;; Insert `text` into the `window`. Text is inserted at the cursor.
(define (window-insert-text! window text)
    (define insert-len (rope-insert! (struct-get window :text)
                                     (struct-get window :cursor) text))
    (window-move-cursor! window
                         insert-len))

;; Handle `event` on `window`.
(define (window-handle-event! window event)
    (when (= event "<backspace>")
      (when (< 0 (struct-get window :cursor))
        (struct-set! window :cursor (- (struct-get window :cursor) 1))
        (rope-delete! (struct-get window :text)
                      (struct-get window :cursor)))
      (return))
  (when (= event "<left>")
    (window-move-cursor! window -1)
    (return))
  (when (= event "<right>")
    (window-move-cursor! window 1)
    (return))
  (when (= event "<enter>")
    (window-insert-text! window "\n")
    (return))
  (window-insert-text! window event))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "User" config.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Handle `event` globally.
(define (handle-event! event)
    (when (= event "<esc>")
      (exit!)
      (return))
  (window-handle-event! (active-window) event))
