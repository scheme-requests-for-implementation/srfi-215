;; © 2020 Göran Weinholt

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice (including the
;; next paragraph) shall be included in all copies or substantial
;; portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(import (scheme base)
        (scheme write)
        (srfi xxx))

(current-log-fields '(FACILITY 1))

(define (find proc list)
  (if (null? list)
      #f
      (if (proc (car list))
          #t
          (find proc (cdr list)))))

;; This log message should be buffered until the callback is set
(send-log DEBUG "first message")
(let ((msgs '()))
  (parameterize ((current-log-callback
                  (lambda (msg)
                    (set! msgs (cons msg msgs)))))
    ;; This should should also be handled by the callback
    (send-log INFO "second message" 'FOO 'bar)
    (unless (find (lambda (msg)
                    (and (eqv? 7 (cdr (assq 'SEVERITY msg)))
                         (string=? "first message"
                                   (cdr (assq 'MESSAGE msg)))))
                  msgs)
      (error "missing the first message" msgs))
    (unless (find (lambda (msg)
                    (and (eqv? 6 (cdr (assq 'SEVERITY msg)))
                         (string=? "second message"
                                   (cdr (assq 'MESSAGE msg)))
                         (equal? "bar"
                                 (cond ((assq 'FOO msg) => cdr)
                                       (else #f)))))
                  msgs)
      (error "missing the second message" msgs))))

;; Check that the example from the document works
(current-log-callback
 (lambda (msg)
   (let ((p (current-error-port)))
     (display "<" p)
     (display (cdr (assq 'SEVERITY msg)) p)
     (display ">" p)
     (display (cdr (assq 'MESSAGE msg)) p)
     (newline p))))

(send-log DEBUG "Log callback configured")
