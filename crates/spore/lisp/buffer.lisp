;; Move the cursor on `buffer` by the given `amount`. `amount` may be
;; positive or negative to move forwards or backwards, respectively.
(define (buffer-move-cursor! buffer amount)
  (define new-cursor (rope-normalize-cursor (struct-get buffer :text)
                                            (+ amount
                                               (struct-get buffer :cursor))))
  (struct-set! buffer :cursor new-cursor))

;; Insert `text` into the `buffer`. Text is inserted at the cursor.
(define (buffer-insert-text! buffer text)
  (define insert-len (rope-insert! (struct-get buffer :text)
                                   (struct-get buffer :cursor) text))
  (buffer-move-cursor! buffer
                       insert-len))

;; Delete the character at the cursor on `buffer`.
(define (buffer-delete-char! buffer)
  (when (< 0 (struct-get buffer :cursor))
    (buffer-move-cursor! buffer -1)
    (rope-delete! (struct-get buffer :text)
                  (struct-get buffer :cursor))))
