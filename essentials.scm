(define (reduce fn base-val lst)
  (letrec ((reduce-rec (lambda (acc rest)
                         (if (null? rest)
                           acc
                           (reduce-rec (fn acc (car rest)) (cdr rest))))))
    (if (null? lst)
      base-val
      (reduce-rec base-val lst))))

(define (concat . colls)
  (reduce append '() colls))

(define (comp . fncs)
  (reduce (lambda (f g) (lambda (. args) (f (apply g args)))) (lambda(x) x) fncs))

(define (every? pred coll)
  (reduce (lambda (x y) (and x y)) #t (map pred coll)))

(define (but-last lst)
  (letrec ((r (lambda(acc rst)
                (if (null? (cdr rst))
                  acc
                  (r (cons (car rst) acc) (cdr rst))))))
    (if (null? lst)
      lst
      (reverse (r '() lst)))))

(define (distinct coll)
  (reduce (lambda(acc x)
            (if (member x acc)
              acc
              (append acc (list x))))
          '()
          coll))

(define (mapcat fn . colls)
  (apply concat (apply map fn colls)))

(define (repeat n x)
  (letrec ((r (lambda(acc m)
                (if (zero? m)
                  acc
                  (r (cons x acc) (- m 1))))))
    (r '() n)))
