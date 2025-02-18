(define exit? false)
(define text-buffer (new-buffer (file-read "crates/spore/Cargo.toml")))
(define modal-buffer (new-buffer (shell-command! "rg" "--files")))
(define cursor (buffer-len text-buffer))

(define screen-text (struct 'buffer text-buffer))
(define screen-modal (struct 'buffer modal-buffer))
(define screen (list screen-text))

(define (toggle-modal!)
    (if (= (list-len screen) 1)
        (define screen (list screen-text screen-modal))
        (define screen (list screen-text))))

(define (handle-event! event)
    (if (= event "<esc>")
        (do
         (define exit? true)
         (return)))
  (if (= event "<tab>")
      (do
       (toggle-modal!)
       (return)))
  (if (= event "<backspace>")
      (if (< 0 cursor)
          (do
           (define cursor (- cursor 1))
           (buffer-delete! text-buffer cursor)
            (return))))
  (if (= event "<backspace>") (return))
  (if (= event "<left>")
      (do
       (define cursor (buffer-normalize-cursor text-buffer (- cursor 1)))
       (return)))
  (if (= event "<right>")
      (do
       (define cursor (buffer-normalize-cursor text-buffer (+ cursor 1)))
       (return)))
  (if (= event "<enter>")
      (do
       (define cursor (+ cursor (buffer-insert! text-buffer cursor "
")))
       (return)))
  (do
   (define cursor (+ cursor (buffer-insert! text-buffer cursor event)))))
