#lang racket

(require
  racket/file
  threading
  rackunit)

(define (bsp->seat-id bsp)
  (let* ([row-id (substring bsp 0 7)]
         [col-id (substring bsp 7 10)]
         [row-num (bsp->row-num row-id)]
         [col-num (bsp->col-num col-id)])
    (~> row-num
        (* 8)
        (+ col-num))))

(define (bsp->row-num row-bsp [r (make-range 0 127)])
  (let ([bsp-char (string-first row-bsp)])
  (cond
    [(eq? bsp-char null)
     (range-lower-bound r)]
    [(eq? bsp-char #\F)
     (bsp->row-num (string-rest row-bsp) (split-range r 'lower))]
    [(eq? bsp-char #\B)
     (bsp->row-num (string-rest row-bsp) (split-range r 'upper))])))

(define (bsp->col-num col-bsp [r (make-range 0 7)])
  (let ([bsp-char (string-first col-bsp)])
  (cond
    [(eq? bsp-char null)
     (range-lower-bound r)]
    [(eq? bsp-char #\L)
     (bsp->col-num (string-rest col-bsp) (split-range r 'lower))]
    [(eq? bsp-char #\R)
     (bsp->col-num (string-rest col-bsp) (split-range r 'upper))])))

(define (make-range l u) (cons l u))

(define range-lower-bound car)

(define range-upper-bound cdr)

(define (split-range r direction)
  (let* ([l (range-lower-bound r)]
         [u (range-upper-bound r)]
         [split-point (~> u (- l) (/ 2) ceiling)])
    (cond
      [(eq? direction 'upper)
         (make-range (+ l split-point) u)]
      [(eq? direction 'lower)
       (make-range l (- u split-point))]
      )))

(define (string-first s)
  (let ([chars (string->list s)])
  (cond
    [(eq? chars null) null]
    [#t (car chars)])))

(define (string-rest s)
  (~> s string->list cdr list->string))

(define seat-ids
    (~> "puzzle_inputs/day05.txt"
       file->lines
       (map bsp->seat-id _)))

;; 5.1

(define highest-seat-id (apply max seat-ids))

(display (~a "highest seat id: " highest-seat-id "\n"))
(check-equal? highest-seat-id 896)

;; 5.2

(define (find-first-missing-in-range nums)
  (let* ([first-num (car nums)]
        [second-num (cadr nums)]
        [diff (- second-num first-num)])
    (if (= diff 1)
        (find-first-missing-in-range (cdr nums))
        (+ first-num 1))))

(define my-seat-id
  (~> seat-ids
      (sort _ <)
      find-first-missing-in-range))

(display (~a "my seat id: " my-seat-id "\n"))
(check-equal? my-seat-id 659)
