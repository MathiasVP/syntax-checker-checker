; Exercise 0
; Scheme syntax evaluator
; Mathias Vorreiter Pedersen, 20112238
; ------------------------------------------
; Helper functions
; ------------------------------------------

(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (- n 1))))))

(define list-at-least-as-long-as?
  (lambda (v n)
    (letrec ([visit (lambda (v i)
                      (or (= i 0)
                           (and (pair? v)
                                (visit (cdr v)
                                       (- i 1)))))])
      (if (>= n 0)
          (visit v n)
          (errorf 'list-at-least-as-long-as? "negative length: ~s" n)))))

(define propify
  (lambda (x)
    (letrec ([visit
              (lambda (xs ys)
                (cond
                  [(not (pair? xs))
                   (append ys (list xs))]
                  [else
                   (visit (cdr xs) (append ys (list (car xs))))]))])
      (visit x '()))))

; ------------------------------------------
; Predicates
; ------------------------------------------
; Helper functions
(define generic-is
  (lambda (x keyword length length-eval-func)
    (if(pair? x)
       (and (length-eval-func x length)
            (equal? (car x) keyword))
       #f)))

(define generic-exact-is
  (lambda (x keyword length)
    (generic-is x keyword length
                proper-list-of-given-length?)))

(define generic-at-least-is
  (lambda (x keyword length)
    (generic-is x keyword length
                list-at-least-as-long-as?)))

; Actual predicates
(define is-time?
  (lambda (x)
    (generic-exact-is x 'time 2)))

(define is-if?
  (lambda (x)
    (generic-exact-is x 'if 4)))

(define is-cond-clause?
  (lambda (x)
    (or (proper-list-of-given-length? x 1)
        (proper-list-of-given-length? x 2)
        (and (proper-list-of-given-length? x 3)
             (equal? (list-ref x 1) '=>)))))

(define is-cond?
  (lambda (x)
   (generic-at-least-is x 'cond 2)))

(define is-case?
  (lambda (x)
    (generic-at-least-is x 'case 3)))

(define is-definition?
  (lambda (x)
    (generic-exact-is x 'define 3)))

(define is-or?
  (lambda (x)
    (generic-at-least-is x 'or 1)))

(define is-and?
  (lambda (x)
    (generic-at-least-is x 'and 1)))

(define is-quotation?
  (lambda (x)
    (or (number? x)
        (boolean? x)
        (char? x)
        (string? x)
        (symbol? x)
        (equal? x '())
        (and (pair? x)
             (is-quotation? (car x))
             (is-quotation? (cdr x))))))

(define is-let?
  (lambda (x)
    (generic-exact-is x 'let 3)))

(define is-let-star?
  (lambda (x)
      (generic-exact-is x 'let* 3)))

(define is-let-rec?
  (lambda (x)
    (generic-exact-is x 'letrec 3)))

(define is-begin?
  (lambda (x)
    (generic-at-least-is x 'begin 2)))

(define is-quote?
  (lambda (x)
    (generic-exact-is x 'quote 2)))

(define is-var?
  (lambda (x)
    (and (symbol? x)
         (not (member x '(define
                             time
                             if
                             cond
                             else
                             case
                             and
                             or
                             let
                             let*
                             letrec
                             begin
                             quote
                             quasiquote
                             unquote
                             unquote-splicing
                             lambda
                             trace-lambda))))))

(define is-application?
  (lambda (x)
    (and
     (list-at-least-as-long-as? x 1))))

(define is-lambda-abs?
  (lambda (x)
    (or
     (generic-exact-is x 'lambda 3)
     (generic-exact-is x 'trace-lambda 4))))

(define is-quasiquote?
  (lambda (x)
    (generic-exact-is x 'quasiquote 2)))

(define is-unquote?
  (lambda (x)
    (equal? (car x) 'unquote)))

(define is-unquote-splicing?
  (lambda (x)
    (equal? (car x) 'unquote-splicing)))

; ------------------------------------------
; Accessors
; ------------------------------------------
(define ref-0
  (lambda (x)
    (list-ref x 0)))

(define ref-1
  (lambda (x)
    (list-ref x 1)))

(define ref-2
  (lambda (x)
    (list-ref x 2)))

(define ref-3
  (lambda (x)
    (list-ref x 3)))

(define ref-=>
  (lambda (x)
    (if(equal? (ref-1 x) '=>)
       (ref-2 x)
       (ref-1 x))))

; ------------------------------------------
; Syntax checkers
; ------------------------------------------
; Helper functions
(define check-distinct
  (lambda (x)
    (letrec ([visit
              (lambda (y ys)
                (cond
                  [(null? ys)
                   #t]
                  [(member y ys)
                   #f]
                  [else
                   (visit (car ys) (cdr ys))]))])
      (or
       (null? x)
       (visit (car x) (cdr x))))))

(define check-zero-or-more
  (lambda (x checker distinct)
    (and (list? x)
         (if distinct
             (and (check-distinct x)
                  (andmap checker x))
             (andmap checker x)))))

(define check-one-or-more
  (lambda (x checker distinct)
    (and (if distinct
             (check-distinct x)
             #t)
         (checker (car x))
         (check-zero-or-more (cdr x) checker #f))))

; Actual checkers
(define check-time
  (lambda (x)
    (check-expression (ref-1 x))))

(define check-if
  (lambda (x)
    (and (check-expression (ref-1 x))
         (check-expression (ref-2 x))
         (check-expression (ref-3 x)))))

(define check-application
  (lambda (x)
    (check-one-or-more x check-expression #f)))

(define check-cond
  (lambda (x)
    (letrec ([visit
              (lambda (y n)
                (cond
                  [(or (null? y)
                       (null? (car y))) #f]
                  [(equal? (caar y) 'else)
                   (and (equal? (length x) (1+ n))
                        (check-expression (cdar y)))]
                  [else
                   (and (check-cond-clause (car y))
                        (visit (cdr y) (1+ n)))]))])
      (and
       (not (null? x))
       (pair? x)
       (visit (cdr x) 1)))))

(define check-cond-clause
  (lambda (x)
    (let ([n (length x)])
      (cond
        [(equal? n 1)
         (check-expression (ref-0 x))]
        [else
         (and (check-expression (ref-0 x))
              (check-expression (ref-=> x)))]))))

(define check-begin
  (lambda (x)
    (check-one-or-more (cdr x) check-expression #f)))

(define check-quotation
  (lambda (x)
    (or
     (number? x)
     (boolean? x)
     (char? x)
     (string? x)
     (symbol? x)
     (equal? '() x)
     (and (check-quotation (car x))
          (check-quotation (cdr x))))))

(define check-expression
  (lambda (x)
    (cond
      [(number? x) #t]
      [(boolean? x) #t]
      [(char? x) #t]
      [(string? x) #t]
      [(is-var? x) #t]
      [(is-time? x)
       (check-time x)]
      [(is-if? x)
       (check-if x)]
      [(is-cond? x)
       (check-cond x)]
      [(is-case? x)
       (check-case x)]
      [(is-and? x)
       (check-and x)]
      [(is-or? x)
       (check-or x)]
      [(is-let? x)
       (check-let x)]
      [(is-let-star? x)
       (check-let-star x)]
      [(is-let-rec? x)
       (check-let-rec x)]
      [(is-begin? x)
       (check-begin x)]
      [(is-quote? x)
       (check-quotation (ref-1 x))]
      [(is-quasiquote? x)
       (check-quasiquote (ref-1 x))]
      [(is-lambda-abs? x)
       (check-lambda-abs x)]
      [(is-application? x)
       (check-application x)]
      [else #f])))

(define check-case
  (lambda (x)
    (letrec ([check-case-body
              (lambda (y n)
                (cond
                  [(or (null? y)
                       (null? (car y))) #f]
                  [(and (not (null? (caar y))) (equal? (caar y) 'else))
                   (and (equal? n (length x))
                        (pair? (cdar y))
                        (equal? (length (cdar y)) 1)
                        (check-expression (cadar y)))]
                  [else
                   (and
                    (check-zero-or-more (caar y) check-quotation #f)
                    (pair? (cdar y))
                    (equal? (length (cdar y)) 1)
                    (check-expression (cadar y))
                    (check-case-body (cdr y) (1+ n)))]))])

      (and (check-expression (list-ref x 1))
           (check-case-body (cddr x) 3)))))

(define check-and
  (lambda (x)
    (check-zero-or-more (cdr x) check-expression #f)))

(define check-or
  (lambda (x)
    (check-zero-or-more (cdr x) check-expression #f)))

(define check-definition
  (lambda (x)
    (and (is-var? (ref-1 x))
         (check-expression (ref-2 x)))))

(define check-lambda-formals
  (lambda (x)
    (cond
      [(list? x)
       (check-zero-or-more x is-var? #t)]
      [(pair? x)
       (check-one-or-more (propify x) is-var? #t)]
      [else
       (is-var? x)])))

(define check-lambda-abs
  (lambda (x)
    (cond
      [(not (is-lambda-abs? x)) #f]
      [(equal? (car x) 'lambda)
       (and (check-lambda-formals (list-ref x 1))
            (check-expression (list-ref x 2)))]
      [(equal? (car x) 'trace-lambda)
       (and (symbol? (list-ref x 1))
            (check-lambda-formals (list-ref x 2))
            (check-expression (list-ref x 3)))]
      [else #f])))

(define check-generic-let
  (lambda (x checker var-handler)
    (letrec ([visit
              (lambda (y vars)
                (cond
                  [(null? y) #t]
                  [(not (pair? (car y))) #f]
                  [(is-var? (caar y))
                   (and (equal? (length (cdar y)) 1)
                        (checker (cdar y))
                        (not (member (caar y) vars))
                        (visit (cdr y) (var-handler (caar y) vars)))]
                  [else #f]))])
      (and (list? (cadr x))
           (visit (cadr x) '())
           (check-expression (caddr x))))))

(define check-let
  (lambda (x)
    (check-generic-let x check-expression cons)))

(define check-let-star
  (lambda (x)
    (check-generic-let x check-expression (lambda (y vars) '()))))

(define check-let-rec
  (lambda (x)
    (check-generic-let x (lambda (x)
                           (check-lambda-abs (car x))) cons)))

(define check-quasiquote
  (trace-lambda check-quasiquote (x)
    (letrec ([visit
              (trace-lambda visit (y n)
                (cond
                  [(< n 0) #f]
                  [(number? y) #t]
                  [(boolean? y) #t]
                  [(char? y) #t]
                  [(string? y) #t]
                  [(symbol? y) #t]
                  [(null? y) #t]
                  [(is-quasiquote? y)
                   (visit (cdr y) (1+ n))]
                  [(or (is-unquote? y)
                       (is-unquote-splicing? y))
                   (if(equal? n 0)
                      (and (equal? (length (cdr y)) 1)
                           (check-expression (cdr y)))
                      (visit (cdr y) (1- n)))]
                  [(pair? y)
                   (and (visit (car y) n)
                        (visit (cdr y) n))]
                  [else (errorf 'check-quasiquote
                                "Invalid expression: ~s"
                                y)]))])
      (visit x 0))))

; ------------------------------------------
; Top level interface
; ------------------------------------------
(define read-file
  (lambda (filename)
    (call-with-input-file filename
      (lambda (p)
        (letrec ([visit (lambda ()
                          (let ([in (read p)])
                            (if (eof-object? in)
                                '()
                                (cons in (visit)))))])
          (visit))))))

(define check-program
  (lambda (v)
    (cond
      [(null? v)
       #t]
      [(pair? v)
       (and (check-toplevel-form (car v))
            (check-program (cdr v)))]
      [else
       #f])))

(define check-toplevel-form
  (lambda (v)
    (cond
      [(is-definition? v)
       (check-definition v)]
      [else
       (check-expression v)])))

(define check-file
  (lambda (filename)
    (if (string? filename)
        (check-program (read-file filename))
        (errorf 'check-file "not a string: ~s" filename))))
