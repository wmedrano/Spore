(define exit? (box false))
(define text-window (struct :text (new-rope "") :cursor 0))
(define windows (list text-window))
(define newline-str "
")

(define (handle-event! event)
    (when (= event "<esc>")
      (box-set! exit? true)
      (return))
  (window-handle-event! text-window event))

(define (window-move-cursor! window amount)
    (struct-set! window
                 :cursor
                 (rope-normalize-cursor (struct-get window :text)
                                        (+ amount
                                           (struct-get window :cursor)))))

(define (window-insert-text! window text)
    (window-move-cursor! window
                         (rope-insert! (struct-get window :text)
                                       (struct-get window :cursor) text)))

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
    (window-insert-text! window newline-str)
    (return))
  (window-insert-text! window event))
