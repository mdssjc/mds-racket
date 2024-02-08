;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 1-Arithmetic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
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

;; 1.3 Mixing It Up
;; Exercise 3

(define str "helloworld")
(define ind "0123456789")
(define i 5)

(define (insert str i)
  (string-append (substring str 0 i) "_" (substring str i)))

(check-expect (insert str i) "hello_world")

;; Exercise 4

(define (delete str i)
  (string-append (substring str 0 i)
                 (substring str (add1 i))))

(check-expect (delete str i) "helloorld")
(check-expect (delete str 0) "elloworld")
;;(check-expect (delete str 10) "helloorld")

;; 1.4 The Arithmetic of Images

;; Exercise 5

;; Simple tree
(define (my-scale value) (* value 1))
(define leaf   (circle (my-scale 10) "solid" "green"))
(define trunk  (rectangle (my-scale 10) (my-scale 20) "solid" "brown"))
(define sheets (overlay/offset leaf 0 (my-scale 5) (overlay/offset leaf (my-scale 10) 0 leaf)))

(define tree (overlay/offset sheets 0 (my-scale 15) trunk))

tree

;; Exercise 6

(define cat (circle 11 "solid" "brown"))
(define pixels (* (image-width cat) (image-height cat)))

(check-expect pixels (* (* 11 2) (* 11 2)))
