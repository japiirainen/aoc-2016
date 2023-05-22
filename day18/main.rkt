#lang racket

(require string-interpolation)

(define (char= a b)
  (= (char->integer a) (char->integer b)))

(define (rule-90 x _ y)
  (if (not (char= x y)) #\^ #\.))

(define (next ss)
  (letrec ([go (lambda (a b ss)
                 (if (= 0 (length ss))
                   (list (rule-90 a b #\.))
                   (cons (rule-90 a b (car ss)) (go b (car ss) (cdr ss)))))])
    (go #\. (car ss) (cdr ss))))

(define (count c xs)
  (foldl (lambda (d acc) (if (char= c d) (+ acc 1) acc)) 0 xs))

(define (iterate-til n f init)
  (let ([r0 (f init)])
    (if (<= n 1) (list r0) (cons r0 (iterate-til (- n 1) f r0)))))

(define (solve input n)
  (count #\. (apply append (iterate-til n next (string->list input)))))

(define (solve-file fp)
  (let ([input (string-trim (file->string fp))])
    (display "Solving for file : @{fp}\n")
    (display "Part 1 : @{(solve input 40)}\n")
    (display "Part 1 : @{(solve input 400000)}\n")))

(for ([fp (current-command-line-arguments)])
     (solve-file fp))
