#lang racket/base

(require racket/port)
(require racket/string)
(require racket/list)

(define (reduce f start lst)
  (if (null? lst)
      start
      (for/fold ([result null]) ([current lst] [next (cdr lst)])
        (if next
            (cons (f current next) result)
            start))))
        
(define (parse filename)
  (define lines (port->lines (open-input-file filename) #:close? #t))
  (for/list ([line lines])
    (map string->number (string-split line " "))))

(define (safe-report? report)
  (define deltas (reduce - null report))
  (define absolute-deltas (map abs deltas))
  (and
   (or (andmap positive? deltas)
       (andmap negative? deltas))
   (>= (apply min absolute-deltas) 1)
   (<= (apply max absolute-deltas) 3)))

(define (sublists-mod-i lst)
  (for/list ([i (range (length lst))])
    (for/list ([x lst] [j (range (length lst))]
                       #:unless (equal? i j))
      x)))

(define (solve-part-1 reports)
  (length (filter safe-report? reports)))

(define (solve-part-2 reports)
  (define-values (safe-reports unsafe-reports) (partition safe-report? reports))
  (define dampener-tolerated-reports
    (for/list ([report unsafe-reports]
               #:when (ormap safe-report? (sublists-mod-i report)))
      report))
  (+ (length safe-reports) (length dampener-tolerated-reports)))
 
(define reports (parse "input.txt"))
(solve-part-1 reports)
(solve-part-2 reports)
