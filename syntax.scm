;;;;;;;;;;

(define check-silently
  #t)

;;;;;;;;;;

(define check-program
  (lambda (v)
    (cond
      [(null? v)
       #t]
      [(pair? v)
       (and (check-toplevel-form (car v))
            (check-program (cdr v)))]
      [else
       (begin
         (unless check-silently
           (printf "check-program -- unrecognized input: ~s~n" v))
         #f)])))

;;;;;;;;;;

(define check-toplevel-form
  (lambda (v)
    (cond
      [(is-definition? v)
       (check-definition (define-1 v) (define-2 v))]
      [else
       (check-expression v)])))

;;;;;;;;;;

;;;;;;;;;;
;;; basic predicates and accessors for definitions:
;;;;;;;;;;

;;; predicate:
(define is-definition?
  (lambda (v)
    (if (pair? v) (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'define))(begin (unless check-silently (printf "is-definition -- unrecognized input: ~s~n" v)) #f))))

;;; 1st accessor:
(define define-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define define-2
  (lambda (v)
    (list-ref v 2)))

;;;;;;;;;;
;;; the syntax checker proper for definitions:
;;;;;;;;;;

(define check-definition
  (lambda (name definiens)
    (and (check-variable name)
         (check-expression definiens))))

;;;;;;;;;;

;;;;;;;;;;
;;; basic predicates and accessors for expressions:
;;;;;;;;;;

;;; predicate:
(define is-time?
  (lambda (v)
    (if (pair? v)(and (proper-list-of-given-length? v 2)
         (equal? (car v) 'time))(begin (unless check-silently (printf "check-let-bindings -- unrecognized input: ~s~n" v)) #f))))

;;; 1st accessor:
(define time-1
  (lambda (v)
    (list-ref v 1)))

;;;;;

;;; predicate:
(define is-if?
  (lambda (v)
    (if (pair? v)(and (proper-list-of-given-length? v 4)
         (equal? (car v) 'if))(begin (unless check-silently (printf "check-let-bindings -- unrecognized input: ~s~n" v)) #f))))

;;; 1st accessor:
(define if-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define if-2
  (lambda (v)
    (list-ref v 2)))

;;; 3rd accessor:
(define if-3
  (lambda (v)
    (list-ref v 3)))

;;;;;

;;; predicate:
(define is-and?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'and))))

;;;;;

;;; predicate:
(define is-not?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'not))))

;;;;;


;;; predicate:
(define is-or?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'or))))

;;;;;

;;; predicate:
(define is-cond?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'cond))))

;;;;;

;;; predicate:
(define is-case?
  (lambda (v)
    (and (list-strictly-longer-than? v 2)
         (equal? (car v) 'case))))

;;;;;

;;; predicate:
(define is-let?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
		 (list? (list-ref v 1))
         (equal? (car v) 'let))))



;;;;;

;;; predicate:
(define is-letstar?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'let*))))

;;;;;

;;; predicate:
(define is-letrec?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'letrec)
		 (list? (list-ref v 1)))))

;;;;;

;;; predicate:
(define is-begin?
  (lambda (v)
    (and (list-strictly-longer-than? v 1)
         (equal? (car v) 'begin))))



;;;;;
;;; predicate:
(define is-trace-lambda?
  (lambda (v)
    (and (proper-list-of-given-length? v 4)
         (equal? (car v) 'trace-lambda))))
		 
;;; predicate:
(define is-lambda?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'lambda))))
		 
;;; 1st accessor:
(define lambda-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define lambda-2
  (lambda (v)
    (list-ref v 2)))

;;; predicate:
(define is-unless?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'unless))))

;;; 1st accessor:
(define unless-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define unless-2
  (lambda (v)
    (list-ref v 2)))

;;;;;

;;; predicate:
(define is-quote?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'quote))))

;;; 1st accessor:
(define quote-1
  (lambda (v)
    (list-ref v 1)))

		 
;;; predicate:
(define is-application?
  (lambda (v)
    (and (pair? v)
         (let ([w (car v)])
           (if (symbol? w)
               (keyword? w)
               #t)))))

;;; 1st accessor:
(define application-operator
  car)

;;; 2nd accessor:
(define application-operands
  cdr)
 
;;;;;;;;;;
;;; the syntax checker proper for expressions:
;;;;;;;;;;
(define check-expressions
  (lambda (v)
	(cond 
		[(null? v) #t]
		[(pair? v)
		(and (check-expression (car v))
			(check-expressions (cdr v)))]
		[else
			(begin (unless check-silently (printf "check-let-bindings -- unrecognized input: ~s~n" v)) #f)])))
  
(define check-expression
  (lambda (v)
    (cond
	[(null? v)#f]
      [(number? v)
       #t]
      [(boolean? v)
       #t]
      [(char? v)
       #t]
      [(string? v)
       #t]
      [(symbol? v)
       (check-variable v)]
      [(is-time? v)
       (check-time-expression (time-1 v))]
      [(is-if? v)
       (check-if-expression (if-1 v) (if-2 v) (if-3 v))]
	  [(is-not? v)
	   (check-expressions (cdr v))]
	  [(is-and? v)
       (check-expressions (cdr v))]
	  [(is-or? v)
       (check-expressions (cdr v))]
	  [(is-cond? v)
       (check-cond-expression v)]
	  [(is-case? v)
       (and (check-expression (cdr (car v)))(check-case-clauses (cdr (cdr v))))]
	  [(is-begin? v)
       (check-begin-expression v)]
      [(is-unless? v)
       (check-unless-expression (unless-1 v) (unless-2 v))]
      [(is-quote? v)
       (check-quote-expression (quote-1 v))]
      [(is-lambda? v)
       (check-lambda-abstraction (cdr v))]
      [(is-trace-lambda? v)
       (and (check-variable (cdr (car v))) 
		(check-lambda-abstraction (cdr (cdr v))))]
      [(is-application? v)
       (check-application (application-operator v) (application-operands v))]
	  [(is-let? v)
       (check-let-expression v)]
	  [(is-letstar? v)
       (check-letstar-expression v)]
	  [(is-letrec? v)
       (check-letrec-expression v)]
      [else
       (begin
         (unless check-silently
           (printf "check-expression -- unrecognized input: ~s~n" v))
         #f)])))


(define check-variable
  (lambda (v)
    (if (keyword? v) #t ((begin
         (unless check-silently
           (printf "check-variable -- unrecognized input: ~s~n" v))
         #f)))))

(define check-time-expression
  (lambda (v)
    (check-expression (time-1 v))))

(define check-if-expression
  (lambda (test consequent alternative)
    (and (check-expression test)
         (check-expression consequent)
         (check-expression alternative))))
		 
		 
		 
(define check-cond-expression
	(lambda (v)
		(check-cond-clauses (cdr v))))

(define check-cond-clauses
	(lambda (v)
		(cond 
			[(not (proper-list-of-given-length? v 1))
				(and (check-cond-clause (car v))
					 (check-cond-clauses (cdr v)))]
			[(proper-list-of-given-length? (car v) 2)
				(check-else-clause v)]
			[else
				 (begin
					(unless check-silently
						(printf "check-cond-clauses  -- unrecognized input: ~s~n" v))
					#f)]
			)))

			
(define check-cond-clause 
	(lambda (v)
		(cond 
			[(proper-list-of-given-length? v 1) (check-expression (car v))]
			[(proper-list-of-given-length? v 2)
				(and (check-expression(cdr v))
					(check-expression (car v)))]
			[(proper-list-of-given-length? v 3)
				(and 	(check-expression (car v))
						(equal? (list-ref v 1) '=> )
						(check-expression (list-ref v 2)))]
			[else (begin
					(unless check-silently
						(printf "check-cond-clause -- not a else clause in the end ~s ~n" v))
					#f)])))

(define check-else-clause
	(lambda (v)
		(if (pair? v) ((equal? (car v) 'else)
				(check-expression (cdr v)))(begin (unless check-silently (printf "check-let-bindings -- unrecognized input: ~s~n" v)) #f))))
				
(define check-begin-expression
  (lambda (v)
    (if (null? v) #t 
		(and (check-expression (car v))
			 (check-begin-expression (cdr v))))))

(define check-unless-expression
  (lambda (test consequent)
    (and (check-expression test)
         (check-expression consequent))))

(define check-quote-expression
  (lambda (v)
    (errorf 'check-quote-expression "not implemented yet")))

(define check-application
  (lambda (v vs)
    (and 	(check-expression v)
			(check-expressions vs))))


(define check-lambda-abstraction
	(lambda (v)
		(cond	[(null? v) #f]
				[(proper-list-of-given-length? v 2)
					(and	(check-lambda-formals (car v))
							(check-expression (cdr v)))]
				[(not (proper-list-of-given-length? v 1))
				(and 	(check-lambda-formals (car v))
						(check-lambda-abstraction (cdr v)))]
				[else
				 (begin
					(unless check-silently
						(printf "check-expression -- unrecognized input: ~s~n" v))
					#f)])))

	
(define check-lambda-formals
	(lambda (f)
		(cond 	[(null? f)
				 #t]
				[(symbol? f)
				 (check-variable f)]
				[(pair? f) 
				 (and	(check-variable (car f))
						(check-lambda-formals (cdr f))
						(not (member (car f) (cdr f))))]
				[else
				 (begin
					(unless check-silently
						(printf "check-lambda-formals -- unrecognized input: ~s~n" v))
					#f)])))

(define check-case-expression
 (lambda (v)
	(if (pair? v) (and (check-expression (cdr (car v)))
		(check-case-clauses (cdr (cdr c))))(begin (unless check-silently (printf "check-case-expression -- unrecognized input: ~s~n" v)) #f))))

(define check-case-clauses
	(lambda (v)
		(if (pair? v)
			(or (and 	(check-case-clause (car v))
						(check-case-clauses (cdr v)))
				(check-else-clause v))
		(begin (unless check-silently (printf "check-case-clauses -- unrecognized input: ~s~n" v)) #f))))
			

(define check-case-clause
			(lambda (v)
				(if (pair? v)(and (check-quotation-expression (car v))(check-expression (cdr v)))(begin (unless check-silently (printf "check-case-clause -- unrecognized input: ~s~n" v)) #f))))

(define check-quotations
	(lambda (v)
		(cond 	[(null? v) #t]
				[(pair? v)(and (check-quote-expression (car v))(check-quotations (cdr v)))]
				[else (begin
					(unless check-silently
						(printf "check-quotations -- not a else clause in the end ~s ~n" v))
					#f)])))
				

				
(define check-let-expression
	(lambda (v)
		(letrec ([visit (lambda (v var)
			(if (null? v)
				(check-expression (list-ref v 2))
				(and 	(not (member (list-ref (car l) 0) vars))
						(check-let-binding (car v))
						(visit (cdr v) (cons (list-ref (car v) 0) vars)))
			))])(visit (cdr (car v)) '()))))

(define check-let-binding
	(lambda (v)
		(if (pair? v)
			(and (check-variable (car v))
				(check-expression (cdr v)))
		(begin
					(unless check-silently
						(printf "check-let-binding -- unrecognized input: ~s~n" v))
					#f))))


(define check-letstar-expression
	(lambda (v)
		(letrec ([visit (lambda (v)
			(if (null? v)
				(check-expression (list-ref v 2))
				(and	(check-letstar-binding (car v))
						(visit (cdr v)))
			))])(visit (cdr (car v))))))
	
(define check-letstar-binding
	(lambda (v)
		(if (pair? v)
			(and (check-variable (car v))
				(check-expression (cdr v)))
		(begin
					(unless check-silently
						(printf "check-letstar-binding -- unrecognized input: ~s~n" v))
					#f))))



(define check-letrec-expression
	(lambda (v)
		(letrec ([visit (lambda (x var)
			(if (null? x)
				(check-expression (list-ref v 2))
				(and 	(not (member (list-ref (car x) 0) vars))
						(check-letrec-binding (car x))
						(visit (cdr x) (cons (list-ref (car x) 0) vars)))
			))])(visit (cdr (car v)) '()))))

(define check-letrec-binding
	(lambda (v)
		(if (pair? v)
			(and (check-variable (car v))
				(check-lambda-abstraction (cdr v)))
		(begin
					(unless check-silently
						(printf "check-letrec-binding -- unrecognized input: ~s~n" v))
					#f))))


(define check-quote-expression
	(lambda (v)
		(cond [(null? v) #t]
				[(number? v) #t]
				[(boolean? v) #t]
				[(char? v) #t]
				[(string? v) #t]
				[(symbol? v) #t]
				[(pair? v)(and(check-quote-expression (car v))(check-quote-expression (cdr v)))]
		
		[else (begin
					(unless check-silently
						(printf "check-quote-- unrecognized input: ~s~n" v))
					#f)])))
					
;;;;;;;;;;
;;; auxiliaries:
;;;;;;;;;;

(define keyword?
  (lambda (w)
    (not (member w (list 'define 'time 'if 'cond 'else 'case 'and 'or 
				'let 'let* 'letrec 'begin 'unless 'quote 'lambda 'trace-lambda)
	))))

(define list-strictly-longer-than?
  (lambda (v n)
    (letrec ([visit (lambda (v i)
                      (and (pair? v)
                           (or (= i 0)
                               (visit (cdr v)
                                      (- i 1)))))])
      (if (>= n 0)
          (visit v n)
          (errorf 'list-strictly-longer-than? "negative length: ~s" n)))))
		  

(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (- n 1))))))
;;; reads an entire file as a list of Scheme data
;;; use: (read-file "filename.scm")
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

;;; interface:
(define check-file
  (lambda (filename)
    (if (string? filename)
        (check-program (read-file filename))
        (errorf 'check-file "not a string: ~s" filename))))

(check-file "syntax.scm")