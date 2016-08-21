(define (read-json file-name)
  (letrec ((read-rec (lambda(buf)
                       (let ((c (read-char)))
                         (if (eof-object? c)
                           buf
                           (read-rec (cons c buf)))))))
    (with-input-from-file file-name
                          (lambda ()
                            (let* ((chrs (read-rec '()))
                                   (txt (list->string (reverse chrs))))
                              txt)))))
