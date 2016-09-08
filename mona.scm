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
(define stack-max 0)
(define (stack-len)
  (let ((s (stack-length (make-stack #t))))
    (if (> s stack-max)
      (set! stack-max s))))

(define (reset-stack-len)
  (set! stack-max 0))

(define (bind p f) (lambda (inp)
                     (stack-len)
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
  (let ((ps (map char (string->list s))))
    (lambda (inp)
      (if (< (string-length inp) (length ps))
        '()
        (let* ((mc (substring inp 0 (length ps)))
               (rst (substring inp (length ps) (string-length inp)))
               (m (map (lambda (cp c) (cp (string c))) ps (string->list mc))))
          (if (every? (lambda(x) (not (null? x))) m)
            (list (cons mc rst))
            '()))))))

(define (non-empty-many p)
  (letrec ((many-rec (lambda (acc inp)
                       (let ((mv (p inp)))
                         (if (null? mv)
                           (if (null? acc)
                             '()
                             (list (cons acc inp)))
                           (many-rec (concat acc (list (caar mv))) (cdar mv)))))))
    (lambda(inp)
      (if (equal? inp "")
        '()
        (many-rec '() inp)))))

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
