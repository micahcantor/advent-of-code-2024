#lang racket/base

(require racket/port)
(require racket/string)
(require racket/list)
(require racket/hash)

(define (parse filename)
  (define lines (port->lines (open-input-file filename) #:close? #t))
  (define input-pairs
    (for/list ([line lines])
      (map string->number (string-split line "   "))))
  (define left (map car input-pairs))
  (define right (map cadr input-pairs))
  (values left right))

(define (solve-part-1 left right)
  (define left-sorted (sort left <=))
  (define right-sorted (sort right <=))
  (apply + (map abs (map - left-sorted right-sorted))))

(define (solve-part-2 left right)
  (define right-appearances (make-hash))
  (for ([x right])
    (hash-update! right-appearances x add1 0))
  (define similarity-scores
    (for/list ([x left])
      (* x (hash-ref right-appearances x 0))))
  (apply + similarity-scores))

(define-values (left right) (parse "input.txt"))
(solve-part-1 left right)
(solve-part-2 left right)
