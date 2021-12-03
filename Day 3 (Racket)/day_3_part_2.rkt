#lang racket

;; https://github.com/alan-turing-institute/advent-of-code-2021/blob/main/day-01/racket_lannelin/day_1.rkt
(require 2htdp/batch-io)
(define INPATH "./input.txt")

(define lines (read-lines INPATH))
(define first-line-length (string-length (first lines)))

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

;; counts the number of ones in a given position
(define (one-count lst pos)
    (define num 0)
    (for ([i (length lst)])
        (set! num (+ (to-num (string-ref (nth lst i) pos)) num))
    )
    num)

;; returns the value of the most common numer in a position, favours 1
(define (get-most-common lst pos)
    (if (< (one-count lst pos) (/ (length lst) 2))
        0
        1)
)

;; returns the list of only the numbers with the common value
(define (discard-least-common lst pos)
    (define num (get-most-common lst pos))
    (define newlst (list))
    (for ([i (length lst)])
        (set! newlst (append newlst
            ;; if the common number is in the position, keep it
            (if (= num (to-num (string-ref (nth lst i) pos)))
                (list (nth lst i))
                (list)
            )
        ))
    )

    (if (= (length newlst) 0) lst newlst)
)

;; returns the list of only the numbers without the common value
(define (discard-most-common lst pos)
    (define num (get-most-common lst pos))
    (define newlst (list))
    (for ([i (length lst)])
        (set! newlst (append newlst
            ;; if the common number is in the position, discard it
            (if (= num (to-num (string-ref (nth lst i) pos)))
                (list)
                (list (nth lst i))
            )
        ))
    )

    (if (= (length newlst) 0) lst newlst)
)

;; converts binary number to decimal number
(define (to-dec bin)
    ;; converts the created string prepended with #b to a decimal number
    (string->number 
        ;; prepend #b to the string, meaning it is a binary number
        (string-append "#b" bin)
    )
)

;; not equal to
(define (neq a b)
    (not (= a b))
)

(define least-common lines)
(define most-common lines)

;; loop through the lines and find the least common and most common numbers for each index
;; reducing the list of lines as we go
(for ([i first-line-length])
    (when (neq (length least-common) 1)
        (set! least-common (discard-most-common least-common i))
    )

    (when (neq (length most-common) 1)
        (set! most-common (discard-least-common most-common i))
    )
)

;; getting out of lists
(set! least-common (first least-common))
(set! most-common (first most-common))

(display "CO2 (bin): ")
(displayln least-common)
(define least-common-dec (to-dec least-common))
(display "CO2 (dec): ")
(displayln least-common-dec)

(newline)

(display "O2 (bin): ")
(displayln most-common)
(define most-common-dec (to-dec most-common))
(display "O2 (dec): ")
(displayln most-common-dec)

;; final answer
(newline)
(display "Answer (part 2): ")
(displayln (* least-common-dec most-common-dec))