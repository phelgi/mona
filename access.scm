(load "mona.scm")

(define (path-add head tail)
  (string->symbol (format #f "~a.~a" (symbol->string head) (symbol->string tail))))

(define (path-concat pth . subpths)
  (reduce (lambda(h t) (path-add h t)) pth subpths))

(define (head-tail lst chr)
  (letrec ((r (lambda(head rst) 
                (if (or (null? rst) (char=? chr (car rst)))
                  (cons head rst)
                  (r (append head (list (car rst))) (cdr rst))))))
    (r '() lst)))

(define (string-split str chr)
  (letrec ((string-split-r (lambda(chrs chr)
                             (let ((ht (head-tail chrs chr)))
                               (if (null? (cdr ht))
                                 (list (car ht))
                                 (cons (car ht) (string-split-r (cddr ht) chr)))))))
    (map list->string (string-split-r (string->list str) chr))))

(define (get-objects obj)
  (if (list? obj)
    (filter symbol? (map (lambda(x)
                           (if (and (list? x) (not (null? x)))
                             (car x)
                             '())) obj))
    '()))

(define (get-fields obj)
  (if (list? obj)
    (map car (filter (lambda(x) (and (pair? x) (not (list? x)))) obj))
    '()))

(define (get-obj obj symb-or-number)
  (if (number? symb-or-number)
    (vector-ref obj symb-or-number)
    (cdr (assq symb-or-number obj))))

(define (get-value obj pth)
  (let* ((pth-str (symbol->string pth))
         (chunks (string-split pth-str #\.))
         (symb-number (lambda (x)
                        (let ((n (string->number x)))
                          (if n
                            n
                            (string->symbol x))))))
    (reduce get-obj obj (map symb-number chunks))))

(define (object? obj pth)
  (list? (get-value obj pth)))

(define (array? obj pth)
  (vector? (get-value obj pth)))

(define (field? obj pth)
  (let ((v (get-value obj pth)))
    (and (not (list? v)) (pair? v))))

(define (leaf? obj pth)
  (let ((v (get-value obj pth)))
    (cond
      ((vector? v) #t)
      ((pair? v) #f)
      (else #t))))
