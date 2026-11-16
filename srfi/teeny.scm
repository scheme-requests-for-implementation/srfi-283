;; SPDX-FileCopyrightText: 2026 Artyom Bologov
;; SPDX-License-Identifier: MIT

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

(define (check->sexp check)
  (cond
   ((or (opcode? check)
        (procedure? check))
    (procedure-name check))
   (else
    #f)))

(define (encoding->type [encoding-str "string"])
  (case (string-ref encoding-str 0)
    [(#\* #\< #\> #\l) #f]
    [(#\p) 'port]
    [(#\.) 'pair]
    [(#\n) 'number]
    [(#\+ #\i) 'integer]
    [(#\/ #\f #\^) 'procedure]
    [(#\s) 'string]
    [(#\c) 'char]
    [(#\y) 'symbol]
    [(#\?) 'continuation]
    [(#\@) 'foreign]
    [(#\v) 'vector]
    [(#\`) 'syntax]
    [(#\!) 'promise]
    [(#\8) 'bytevector]
    [(#\$) 'struct]
    [(#\:) 'parameter]
    [(#\b) 'boolean]
    [(#\0) 'null]
    [(#\x) 'error-object]
    [(#\&) 'pointer]))

(define (type->check type)
  (case type
    [(string) string?]
    [(symbol) symbol?]
    [(opcode) opcode?]
    [(pair) pair?]
    ;; Yes, this loses some precision
    [(procedure continuation foreign syntax) procedure?]
    [(char) char?]
    ;; Input / output?
    [(port) port?]
    [(vector) vector?]
    [(promise) promise?]
    [(environment) environment?]
    [(bytevector) bytevector?]
    [(parameter) parameter?]
    [(boolean) boolean?]
    [(null) null?]
    [(error-object) error-object?]
    [(pointer) pointer?]
    [(integer) integer?]
    [(number) number?]
    [else #f]))

(define (check-of datum)
  (cond
   ;; Special constant, the only-ish in Teeny
   ((eof-object? datum) eof-object?)
   (else (type->check (type-of datum)))))

(define (procedure-dummy-types [proc "func"])
  (let-values ([(min max) (procedure-arity proc)])
    `([(,@(make-list min #f)
        . ,(if (equal? max min) '() #f))
       #f])))

;; This is crude, because Teeny actually has information on optional
;; arguments too. But this is enough for a compliant implementation.
(define (opcode-types-of [opcode "opcode"])
  (let*-values ([(min max) (procedure-arity opcode)]
                [(variadic?) (not (equal? min max))]
                [(checks) (opcode-checks opcode)])
    (if checks
        `([,(let loop ([idx 0]
                       [checks (string->list checks)])
              (cond
               [(>= idx min)
                (cond
                 [(and variadic?
                       (not (null? checks)))
                  (encoding->type (string (car checks)))]
                 [variadic? #f]
                 [else '()])]
               [else
                (cons (encoding->type (string (car checks)))
                      (loop (+ idx 1) (cdr checks)))]))
           #f])
        (procedure-dummy-types opcode))))

(define (closure/macro-types-of [clomac "func"])
  (let* ([code (procedure-code clomac)])
    `([,(let loop ([args (cadr code)])
          (cond
           [(null? args)
            '()]
           [(symbol? args)
            #f]
           [(pair? (car args))
            (cons (encoding->type (cadr (car args)))
                  (loop (cdr args)))]
           [else
            (cons #f (loop (cdr args)))]))
       #f])))

(define (procedure-types-of [proc "func"])
  (cond
   [(opcode? proc)
    (opcode-types-of proc)]
   [(or (closure? proc) (macro? proc))
    (closure/macro-types-of proc)]
   [else
    #f]))

(define (%recursive-type->check x)
  (cond
   [(null? x)
    x]
   [(pair? x)
    (cons (%recursive-type->check (car x))
          (%recursive-type->check (cdr x)))]
   [else
    (type->check x)]))

(define (procedure-checks-of [proc "func"])
  (%recursive-type->check (procedure-types-of proc)))
