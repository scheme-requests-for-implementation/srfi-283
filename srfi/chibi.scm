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
   ((opcode? check)
    (string->symbol (opcode-name check)))
   ((procedure? check)
    (procedure-name check))
   (else
    #f)))

(define (type->check type)
  (if (not (type? type))
      #f
      (case (string->symbol (type-name type))
        ((Number) number?)
        ((Bignum Integer) integer?)
        ((Flonum) inexact?)
        ((Ratio) rational?)
        ((Complex) complex?)
        ((Symbol) symbol?)
        ((Char) char?)
        ((Boolean) boolean?)
        ((String) string?)
        ((Byte-Vector) bytevector?)
        ((Pair) pair?)
        ((Vector) vector?)
        ((Input-Port) input-port?)
        ((Output-Port) output-port?)
        ((Opcode Procedure) procedure?)
        (else #f))))

(define (check-of datum)
  (cond
   ((eof-object? datum) eof-object?)
   ((null? datum) null?)
   ((error-object? datum) error-object?)
   (else (type->check (type-of datum)))))

(define (%opcode-types-of opcode)
  (let ((params (let loop ((idx 0))
                  (if (= (opcode-num-params opcode) idx)
                      '()
                      (cons (opcode-param-type opcode idx)
                            (loop (+ 1 idx)))))))
    `((,@(cond
          ((and (opcode-variadic? opcode)
                (null? params))
           (list (opcode-param-type opcode 0)))
          ((opcode-variadic? opcode)
           (list `(,@params . (opcode-param-type opcode (length params)))))
          (else
           (list params)))
       (,(opcode-return-type opcode))))))

(define (procedure-types-of proc)
  (cond
   ((opcode? proc)
    (%opcode-types-of proc))
   ((and (procedure? proc)
         (procedure-arity proc))
    ;; This is a really convoluted structure, so you have to trust me
    ;; it’s right. I wouldn’t trust me on that.
    `(((,@(make-list (procedure-arity proc) #f)
        . ,(if (procedure-variadic? proc)
               #f
               '()))
       #f)))
   (else #f)))

(define (%recursive-type->check x)
  (cond
   ((null? x)
    x)
   ((pair? x)
    (cons (%recursive-type->check (car x))
          (%recursive-type->check (cdr x))))
   (else
    (type->check x))))

(define (procedure-checks-of proc)
  (%recursive-type->check (procedure-types-of proc)))
