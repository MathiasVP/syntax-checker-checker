# syntax-checker-checker
Testing syntax checkers for the Programming Language course at Aarhus University

## Usage
```
Mathiass-MacBook-Pro:TestSyntaxChecker mathias$ ./main
main: Usage:         path/to/syntax/checker.scm pos|neg n [restrics]
       	pos:         Run positive tests
       	neg:   		 Run negative tests
       	n:     		 Number of tests
       	size > 0:    Maximum size for largest testcase
       	[restricts]: Do not generate these syntactic constructs.
       	The possible values are:
       		-toplevelform
       		-toplevel-definition
       		-toplevel-expression
       		-expression
       		-number
       		-boolean
       		-char
       		-string
       		-time
       		-if
       		-and
       		-or
       		-cond
       		-case
       		-let
       		-let*
       		-letrec
       		-begin
       		-unless
       		-quote
       		-application
       		-variable
       		-cond-clause-immediate
       		-cond-clause-guarded
       		-cond-clause-guarded-impl
       		-case-clause
       		-let-binding
       		-let*-binding
       		-letrec-binding
       		-trace-lambda
       		-lambda
       		-quote-number
       		-quote-boolean
       		-quote-char
       		-quote-string
       		-quote-symbol
       		-quote-empty
       		-quote-pair
```

## Note
Negative testcase generation is currently bugged :(
