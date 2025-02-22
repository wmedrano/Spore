(define exit? false)
(define text-window (struct :text (new-rope "") :cursor 0))
(define windows (list text-window))

(define (handle-event! event)
    (when (= event "<esc>")
      (define exit? true)
      (return))
  (handle-event-for-window! text-window event))

(define (handle-event-for-window! window event)
    (when (= event "<backspace>")
      (when (< 0 (struct-get window :cursor))
        (struct-set! window :cursor (- (struct-get window :cursor) 1))
        (rope-delete! (struct-get window :text)
                      (struct-get window :cursor)))
      (return))
  (when (= event "<left>")
    (struct-set! window :cursor (rope-normalize-cursor (struct-get window :text)
                                                       (- (struct-get window :cursor) 1)))
    (return))
  (when (= event "<right>")
    (struct-set! window :cursor (rope-normalize-cursor (struct-get window :text)
                                                       (+ (struct-get window :cursor) 1)))
    (return))
  (when (= event "<enter>")
    (struct-set! window
                 :cursor
                 (+ (struct-get window :cursor)
                    (rope-insert! (struct-get window :text)
                                  (struct-get window :cursor)
                                  "
")))
    (return))
  (struct-set! window
               :cursor
               (+ (struct-get window :cursor)
                  (rope-insert! (struct-get window :text)
                                (struct-get window :cursor) event))))
