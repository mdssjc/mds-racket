;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 1-Arithmetic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 1-Arithmetic.rkt

;; I - Fixed-Size Data
;; 1 - Arithmetic

;; 1.1 - The Arithmetic of Numbers
;; Exercise 1

(define x 3)
(define y 4)

(define (distance x y)
  (sqrt (+ (sqr x) (sqr y))))

(check-expect (distance x y)  5)
(check-expect (distance 12 5) 13)

;; 1.2 - The Arithmetic of Strings
;; Exercise 2

(define prefix "hello")
(define suffix "world")

(define (glue prefix suffix)
  (string-append prefix "_" suffix))

(check-expect (glue prefix suffix) "hello_world")
