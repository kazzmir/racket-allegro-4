#lang scheme

(require (prefix-in image- "../image.ss"))
(require "../util.ss")
(require "../keyboard.ss")

(easy-init 640 480 16 'WINDOWED)

;; after this point its safe to use any graphics function

;; width and height of each test's image
(define WIDTH 120)
(define HEIGHT 20)

(define white (image-color 255 255 255))

;; print the title of the test
(define (title image string)
  (image-print image 0 0 white -1 string))

(define (test-putpixel image)
  (title image "Putpixel")
  (image-putpixel image 10 15 white))

(define (test-translucent-putpixel image)
  (title image "Trans putpixel")
  (image-rectangle-fill image 0 10 (sub1 WIDTH) (sub1 HEIGHT)
			(image-color 200 0 0))
  (image-putpixel/translucent image 0 0 0 128 10 15 white))

(define (test-rectangle image)
  (title image "Rectangle")
  (image-rectangle image 5 12 20 17 white))

(define (test-rectangle-translucent image)
  (title image "Trans rectangle")
  (image-rectangle-fill image 0 10 (sub1 WIDTH) (sub1 HEIGHT)
			(image-color 200 0 0))
  (image-rectangle/translucent image 0 0 0 128 5 12 20 17
			       white))

(define (test-circle image)
  (title image "Circle")
  (image-circle image 10 14 5 white))

(define (test-circle-translucent image)
  (title image "Trans Circle")
  (image-rectangle-fill image 0 10 (sub1 WIDTH) (sub1 HEIGHT)
			(image-color 200 0 0))
  (image-circle/translucent image 0 0 0 128 10 14 5 white))

(define (test-triangle image)
  (title image "Triangle")
  (image-triangle image 5 11 10 18 18 13 white))

(define (test-triangle-translucent image)
  (title image "Trans Triangle")
  (image-rectangle-fill image 0 10 (sub1 WIDTH) (sub1 HEIGHT)
			(image-color 200 0 0))
  (image-triangle/translucent image 0 0 0 128 5 11 10 18 18 13 white))

(define (test-floodfill image)
  (title image "Floodfill")
  (image-rectangle image 3 11 15 19 (image-color 255 0 0))
  (image-floodfill image 5 13 white))

(define (test-floodfill-translucent image)
  (title image "Trans Floodfill")
  (image-rectangle-fill image 0 10 (sub1 WIDTH) (sub1 HEIGHT)
			(image-color 0 0 200))
  (image-rectangle image 3 11 15 19 (image-color 255 0 0))
  (image-floodfill/translucent image 0 0 0 128 5 13 white))

(define (test-ellipse image)
  (title image "Ellipse")
  (image-ellipse image 12 14 8 3 white))

(define (test-ellipse-translucent image)
  (title image "Trans Ellipse")
  (image-rectangle-fill image 0 10 (sub1 WIDTH) (sub1 HEIGHT)
			(image-color 200 0 0))
  (image-ellipse/translucent image 0 0 0 128 12 14 8 3 white))

(define (test-ellipse-fill image)
  (title image "Ellipse Fill")
  (image-ellipse-fill image 12 14 8 3 white))

(define (test-ellipse-fill-translucent image)
  (title image "Trans Ellipse Fill")
  (image-rectangle-fill image 0 10 (sub1 WIDTH) (sub1 HEIGHT)
			(image-color 200 0 0))
  (image-ellipse-fill/translucent image 0 0 0 128 12 14 8 3 white))


;; get a sub-bitmap of screen so each test can draw directly to it using
;; absolute coordinates, that is 0,0 is the upper left hand corner
(define (get-image x y)
  (let ((i (image-create-sub-screen (* x WIDTH) (* y HEIGHT) WIDTH HEIGHT)))
    ;; give it a nice border
    (image-rectangle i 0 0 (sub1 WIDTH) (sub1 HEIGHT) (image-color 64 64 64))
    i))

;; this can probably be done with continuations
(let* ((next (let ((x -1)
		  (y 0))
	      (lambda ()
		(set! x (add1 x))
		(when (>= x 5)
		  (set! y (add1 y))
		  (set! x 0))
		(get-image x y))))
       (run (lambda (test)
	      (let ((sub (next)))
		(test sub)
		(image-destroy sub)))))
  (run test-putpixel)
  (run test-translucent-putpixel)
  (run test-rectangle)
  (run test-rectangle-translucent)
  (run test-circle)
  (run test-circle-translucent)
  (run test-triangle)
  (run test-triangle-translucent)
  (run test-floodfill)
  (run test-floodfill-translucent)
  (run test-ellipse)
  (run test-ellipse-translucent)
  (run test-ellipse-fill)
  (run test-ellipse-fill-translucent)
  )

;; wait for key press
(readkey)

;; kill graphics window
(easy-exit)
(exit)
