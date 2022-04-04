#lang racket

(provide commatize)


(define commatize
  (lambda (n)
    (string-join (split n) ",")))

(define split
  (lambda (n [result '()])
    (let* ([tail (modulo n 1000)]
           [result
             (cons
              (cond
                [(< n 1000) (~r tail)]
                [else (~r tail #:min-width 3 #:pad-string "0")])
              result)])
      (cond
        [(< n 1000) result]
        [else (split (floor (/ n 1000)) result)]))))

