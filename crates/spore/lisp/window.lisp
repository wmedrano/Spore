(define %text-window (struct :text (new-rope "") :cursor 0 :modal? false))
(define %windows (box (list %text-window)))

;; The currently active window.
(define (active-window)
  (define modals (filter (lambda (w) (struct-get w :modal?)) (unbox %windows)))
  (if (< (list-empty? modals))
      %text-window
      (nth modals 0)))

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

(define (window-delete-char! window)
  (when (< 0 (struct-get window :cursor))
    (window-move-cursor! window -1)
    (rope-delete! (struct-get window :text)
                  (struct-get window :cursor))))

;; Returns true if `x` is a string that contains a single character.
(define (char? x)
  (= (string-len x) 1))

(define (open-modal! text)
  (box-set! %windows
            (list-concat (list (struct :text (new-rope text) :cursor 0 :modal? true))
                         (unbox %windows))))

;; Handle `event` for `window`.
(define (window-handle-event! window event)
  (when (char? event)
    (window-insert-text! window event)
    (return))
  (when (= event "<backspace>")
    (window-delete-char! window))
  (when (= event "<left>")
    (window-move-cursor! window -1))
  (when (= event "<right>")
    (window-move-cursor! window 1))
  (when (= event "<enter>")
    (window-insert-text! window "\n")))

