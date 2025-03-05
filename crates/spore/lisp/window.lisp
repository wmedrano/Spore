(define (window-new-modal title)
  (struct :buffer (struct :text (new-rope "") :cursor 0)
          :border true
          :title title))

(define %text-window
  (struct :buffer (struct :text (new-rope "") :cursor 0)))
(define %windows
    (box (list %text-window)))


;; The currently active window.
(define (active-window)
    (nth (unbox %windows) 0))

(define (window-add-window! window)
  (box-set! %windows (list-concat (unbox %windows)
                                  (list window))))
(define (window-remove-window! window)
  (box-set! %windows
            (filter (lambda (w) (not (= w window)))
                    (unbox %windows))))

(define (window-buffer window)
  (struct-get window :buffer))

;; Returns true if `x` is a string that contains a single character.
(define (char? x)
  (= (string-len x) 1))

;; Handle `event` for `buffer`.
(define (window-handle-event! window event)
  (define buffer (window-buffer window))
  (when (char? event)
    (buffer-insert-text! buffer event)
    (return))
  (when (= event "<backspace>")
    (buffer-delete-char! buffer))
  (when (= event "<left>")
    (buffer-move-cursor! buffer -1))
  (when (= event "<right>")
    (buffer-move-cursor! buffer 1))
  (when (= event "<enter>")
    (buffer-insert-text! buffer "\n")))
