(define exit? false)
(define text (new-buffer (shell-command! "rg" "--files")))
(define cursor (buffer-len text))

(define (handle-event! event)
    (if (= event "<esc>")
        (do
         (define exit? true)
         (return)))
  (if (= event "<backspace>")
      (if (< 0 cursor)
          (do
           (define cursor (- cursor 1))
           (buffer-delete! text cursor)
            (return))))
  (if (= event "<backspace>") (return))
  (if (= event "<left>")
      (do
       (define cursor (buffer-normalize-cursor text (- cursor 1)))
       (return)))
  (if (= event "<right>")
      (do
       (define cursor (buffer-normalize-cursor text (+ cursor 1)))
       (return)))
  (if (= event "<enter>")
      (do
       (define cursor (+ cursor (buffer-insert! text cursor "
")))
       (return)))
  (do
   (define cursor (+ cursor (buffer-insert! text cursor event)))))
