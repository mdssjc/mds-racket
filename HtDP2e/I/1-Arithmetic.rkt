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

(check-expect (sqrt (+ (sqr x) (sqr y)))  5)
(check-expect (sqrt (+ (sqr 12) (sqr 5))) 13)

;; 1.2 - The Arithmetic of Strings
;; Exercise 2

(define prefix "hello")
(define suffix "world")

(define ex2 (string-append prefix "_" suffix))

(check-expect ex2 "hello_world")

;; 1.3 - Mixing It Up
;; Exercise 3

(define str "helloworld")
(define ind "0123456789")
(define i 5)

(define ex3 (string-append (substring str 0 i) "_" (substring str i)))

(check-expect ex3 "hello_world")

;; Exercise 4

(define ex4a (string-append (substring str 0 i) (substring str (add1 i))))
(define ex4b (string-append (substring str 0 0) (substring str (add1 0))))
;;(define ex4c (string-append (substring str 0 10) (substring str (add1 10))))

(check-expect ex4a "helloorld")
(check-expect ex4b "elloworld")
;;(check-expect ex4c "helloorld")

;; 1.4 - The Arithmetic of Images

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

;; 1.5 - The Arithmetic of Booleans

;; Exercise 7

(define sunny  #true)
(define friday #false)

(check-expect (or (not sunny) friday) #false)

;; 1.6 - Mixing It Up with Booleans

;; Exercise 8

(define rh (rectangle 10 20 "solid" "black"))
(define rw (rectangle 20 10 "solid" "black"))
(define rs (rectangle 20 20 "solid" "black"))

(define (tall-or-wide? img)
  (cond [(> (image-height img) (image-width  img)) "tall"]
        [(> (image-width  img) (image-height img)) "wide"]
        [else "square"]))

(check-expect (tall-or-wide? cat) "square")
(check-expect (tall-or-wide? rh)  "tall")
(check-expect (tall-or-wide? rw)  "wide")
(check-expect (tall-or-wide? rs)  "square")

;; 1.7 - Predicates: Know Thy Data

;; Exercise 9

(define in -123)

(define (convert x)
  (cond [(string? x)  (string-length x)]
        [(image? x)   (* (image-width x) (image-height x))]
        [(number? x)  (if (> x 0) x (* x -1))]
        [(boolean? x) (if x 10 20)]))

(check-expect (convert in) 123)
(check-expect (convert "hello") 5)
(check-expect (convert (rectangle 10 10 "solid" "black")) 100)
(check-expect (convert -1) 1)
(check-expect (convert 0)  0)
(check-expect (convert 1)  1)
(check-expect (convert #true)  10)
(check-expect (convert #false) 20)

;; Exercise 10

"Now relax, eat, sleep, and then tackle the next chapter."

;; 2 - Functions and Programs

;; 2.1 Functions

;; Exercise 11

(define (distance x y)
  (sqrt (+ (sqr x) (sqr y))))

(check-expect (distance x y)  5)
(check-expect (distance 12 5) 13)

;; Exercise 12

(define (cvolume side)
  (* side side side))

(check-expect (cvolume 3) 27)
(check-expect (cvolume 4) 64)

(define (csurface side square)
  (* side square))

(check-expect (csurface 3 9) 27)
(check-expect (csurface 4 16) 64)

;; Exercise 13

; A 1String is a String of length 1,
; including
; – "\\" (the backslash),
; – " " (the space bar),
; – "\t" (tab),
; – "\r" (return), and
; – "\b" (backspace).
; interpretation represents keys on the keyboard

(define (string-first sf)
  (substring sf 0 1))

(check-expect (string-first "hello") "h")

;; Exercise 14

(define (string-last sl)
  (substring sl (sub1 (string-length sl))))

(check-expect (string-last "hello") "o")

;; Exercise 15

(define (implication sunny friday)
  (or (not sunny) friday))

(check-expect (implication #true #true) #true)
(check-expect (implication #true #false) #false)
(check-expect (implication #false #true) #true)
(check-expect (implication #false #false) #true)

;; Exercise 16

(define (image-area img)
  (* (image-width img) (image-height img)))

(check-expect (image-area cat) pixels)

;; Exercise 17

(define (image-classify img)
  (cond
    [(> (image-height img) (image-width img)) "tall"]
    [(= (image-height img) (image-width img)) "square"]
    [(< (image-height img) (image-width img)) "wide"]))

(check-expect (image-classify cat) "square")
(check-expect (image-classify rh)  "tall")
(check-expect (image-classify rw)  "wide")
(check-expect (image-classify rs)  "square")

;; Exercise 18

(define (string-join str1 str2)
  (string-append str1 "_" str2))

(check-expect (string-join prefix suffix) "hello_world")

;; Exercise 19

(define (string-insert s i)
  (string-append (substring s 0 i)
                 "_"
                 (substring s i)))

(check-expect (string-insert "helloworld" 5) "hello_world")
(check-expect (string-insert "helloworld" 0) "_helloworld")
(check-expect (string-insert "helloworld" 10) "helloworld_")

;; Exercise 20
(define (string-delete str i)
  (string-append (substring str 0 i)
                 (substring str (add1 i))))

(check-expect (string-delete str 0) "elloworld")
(check-expect (string-delete str 5) "helloorld")
(check-expect (string-delete str 9) "helloworl")

;; Exercise 21

(define (ff a)
  (* 10 a))

(check-expect (ff (ff 1)) 100)
(check-expect (+ (ff 1) (ff 1)) 20)

;; Exercise 22

(define (distance-to-origin x y)
  (sqrt (+ (sqr x) (sqr y))))

(check-expect (distance-to-origin 3 4) 5)

;; Exercise 23

(check-expect (string-first "hello world") "h")

;; Exercise 24

(check-expect (==> #true #false) #false)

;; Exercise 25

;; replace >= to >, and <= to <

;; Exercise 26

(check-expect (string-insert "helloworld" 6) "hellow_orld")
