;; monadic parser in scheme based on the paper
;; >>Monadic Parser Combinators<< by Graham Hutton and Erik Meijer
;; R4RS compliant

(load "essentials.scm")

;; primitives
(define result (lambda (v)
                 (lambda (inp) (list (cons v inp)))))

(define zero (lambda (inp) '()))

(define item (lambda (inp)
               (if (equal? inp "")
                 '()
                 (let ((f (string-ref inp 0))
                       (r (substring inp 1 (string-length inp))))
                   (list (cons f r))))))

;; combinators
(define (bind p f) (lambda (inp)
                     (let ((vs (p inp)))
                       (if (null? vs)
                         vs
                         ((f (caar vs)) (cdar vs))))))

(define (sat pred) (bind item
                         (lambda (x)
                           (if (pred x)
                             (result x)
                             zero))))

(define char (lambda (x) (sat (lambda(y) (equal? x y)))))

(define digit (sat (lambda(x) (char-numeric? x))))

(define lower (sat (lambda(x) (char-lower-case? x))))

(define upper (sat (lambda(x) (char-upper-case? x))))

(define (plus p q)
  (lambda (inp)
    (concat (p inp) (q inp))))

(define letter (plus lower upper))
(define alphanum (plus letter digit))

(define (string-p s)
  (if (equal? s "")
    (result s)
    (let ((x (string-ref s 0))
          (xs (substring s 1 (string-length s))))
      (bind (char x)
            (lambda (_)
              (bind (string-p xs)
                    (lambda (_)
                      (result (string-append (string x) xs)))))))))

(define non-empty-many
  (lambda (p)
    (bind p
          (lambda (x)
            (bind (many p)
                  (lambda (xs)
                    (result (append (list x) xs))))))))

(define many (lambda (p) (plus (non-empty-many p) (result '()))))
(define many-1 (lambda (p) (non-empty-many p)))

(define first (lambda (p)
                (lambda (inp)
                  (let ((x (p inp)))
                    (if (null? x)
                      x
                      (list (car x)))))))

(define (plus-1 p q)
  (first (plus p q)))

(define d-chars (reduce plus zero (list alphanum (char #\.) (char #\-) (char #\E))))

(define double (bind (first (many-1 d-chars))
                       (lambda(x)
                         (result (string->number (list->string x))))))

(define integer (bind (first (many-1 (plus digit (char #\-))))
                        (lambda(x)
                          (result (string->number (list->string x))))))
