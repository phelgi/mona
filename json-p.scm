(load "mona.scm")

(define space (sat char-whitespace?))
(define komma (char #\,))

(define pair-sep (sep-by-1 (bind (many space)
                                 (lambda(_)
                                   (bind komma
                                         (lambda(x)
                                           (bind (many space)
                                                 (lambda(_)
                                                   (result x)))))))))
(define value-sep pair-sep)

(define (single-sep p)
  (bind (many space)
        (lambda(_)
          (bind p
                (lambda(x)
                  (bind (many space)
                        (lambda(_)
                          (result x))))))))

(define kv-sep (single-sep (char #\:)))
(define curly-open (single-sep (char #\{)))
(define curly-close (single-sep (char #\})))
(define bracket-open (single-sep (char #\[)))
(define bracket-close (single-sep (char #\])))

(define non-komma
  (sat (lambda (ch) (not (char=? ch #\,)))))

(define non-quote
  (sat (lambda (ch) (not (char=? ch #\")))))

(define quoted (bind (char #\")
                     (lambda (_)
                       (bind (many non-quote)
                             (lambda (x)
                               (bind (char #\")
                                     (lambda (_)
                                       (result x))))))))

(define true (string-p "true"))
(define false (string-p "false"))
(define null (string-p "null"))
(define number double)
(define object '())
(define array '())
(define value '())

(define (eval-p x)
  (cond
    ((string? x) (cond
                   ((string=? x "true") #t)
                   ((string=? x "false") #f)
                   ((string=? x "null") '(()))))
    ((and (list? x) (every? char? x) (list->string x)))
    (else x)))

(define pair (bind quoted
                   (lambda (k)
                     (bind kv-sep
                           (lambda(_)
                             (bind value
                                   (lambda (v)
                                     (result (cons (string->symbol (list->string k))
                                                   (eval-p v))))))))))
(define object
  (bind curly-open
        (lambda (_)
          (bind (plus (pair-sep pair) (result '(())))
                (lambda (v)
                  (bind curly-close
                        (lambda(_)
                          (result v))))))))

(define array
  (bind bracket-open
        (lambda (_)
          (bind (plus (value-sep value) (result '(#())))
                (lambda (v)
                  (bind bracket-close
                        (lambda(_)
                          (result (list->vector (map eval-p v))))))))))

(define value
  (reduce plus-1 null (list true
                            false
                            quoted
                            number
                            object
                            array)))
