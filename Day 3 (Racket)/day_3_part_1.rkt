#lang racket

;; https://github.com/alan-turing-institute/advent-of-code-2021/blob/main/day-01/racket_lannelin/day_1.rkt
(require 2htdp/batch-io)
(define INPATH "./input.txt")

(define lines (read-lines INPATH))
;; count lines in file
(define line-count (length lines))

;; get length of first item in lines
(define first-line-length (string-length (first lines)))
;; create a list of that length
(define arr (make-list first-line-length 0))

;; https://stackoverflow.com/a/50332810
(define (nth lst counter)
    (cond ((null? lst) (error 'nth "index out of bounds"))
        ((= counter 0) (first lst))
        (else (nth (rest lst) (- counter 1)))
    )
)

;; sets the value of the array at the given index to the given value
(define (set-nth lst index val)
    (define newlst (list))
    (for ([i (length lst)])
        (set! newlst (append newlst (if (= i index) (list val) (list (nth lst i)))))
    )

    newlst)

;; converts a delimited string to a number
(define (to-num str)
    (string->number (string str))
)

;; conerts list of numbers to a string
(define (to-string lst)
    (string-join (map number->string lst) "")
)

;; converts binary number list to decimal number
(define (to-dec lst)
    ;; converts the created string prepended with #b to a decimal number
    (string->number 
        ;; prepend #b to the string, meaning it is a binary number
        (string-append "#b" (to-string lst))
    )
)

;; displays text and number nicely
(define (print-num-desc desc num)
    (displayln (string-append desc (number->string num)))
)

;; counts the number of times a one appears in each position
(for ([i line-count])
    (for ([j first-line-length])
        (set! arr (set-nth arr j (+ (to-num (string-ref (nth lines i) j)) (list-ref arr j))))
    )
)

;; sets the index to 1 if the value is greater than 50% of line-count
(for ([i first-line-length])
    (if (> (list-ref arr i) (/ line-count 2))
        (set! arr (set-nth arr i 1))
        (set! arr (set-nth arr i 0))
    )
)
(define gamma arr)
(define gamma-dec (to-dec gamma))

;; inverts the value of arr
(for ([i first-line-length])
    (if (= (list-ref arr i) 0)
        (set! arr (set-nth arr i 1))
        (set! arr (set-nth arr i 0))
    )
)
(define epsilon arr)
(define epsilon-dec (to-dec epsilon))


(displayln (string-append "Gamma (bin): " (to-string epsilon)))
(print-num-desc "Gamma (dec): " gamma-dec)

(newline)

(displayln (string-append "Epsilon (bin): " (to-string epsilon)))
(print-num-desc "Epsilon (dec): " epsilon-dec)

;; final answer
(newline)
(print-num-desc "Answer (part 1): " (* gamma-dec epsilon-dec))
