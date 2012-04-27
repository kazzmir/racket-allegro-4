;; Illusion of traveling through a colored wormhole
;; This program has been through the following languages
;; pascal -> c -> c++ -> c( with x11 ) -> and now badly written scheme..

(module wormhole mzscheme
  
(require (prefix image- "../image.ss")
	 "../util.ss"
	 (prefix keyboard- "../keyboard.ss"))

(require (lib "list.ss"))

(define (real->int i)
  (inexact->exact (round i)))

#;
(define-values (foo bar)
  (define x 1)
  (define foo 2)
  (define bar 3)
  (values foo bar))


(define-values (star-z star-x star-y
                star-alive? make-star recalc-star
		update-star)
  (let ()
    (define si 1024)

    (define-struct star (x y z cx cy screen-x screen-y))
    (define (make-star-user x y z cx cy)
      (let ((s (make-star x y z cx cy 0 0)))
	(recalc-star s)
	s))

    (define (star-alive? star)
      (and (> (star-z star) 0)
	   (> (star-screen-x star) 0)
	   (> (star-screen-y star) 0)
	   (< (star-screen-x star) 640)
	   (< (star-screen-y star) 480)))

    (define (recalc-star s)
      (set-star-screen-x! s
			  (+ (quotient (* si (star-x s)) (star-z s)) (star-cx s)))
      (set-star-screen-y! s
			  (+ (quotient (* si (star-y s)) (star-z s)) (star-cy s))))

    (define (update-star s)
      (set-star-z! s (- (star-z s) 9)))

    (values star-z star-screen-x star-screen-y
	    star-alive? make-star-user recalc-star
	    update-star)))

(define-values (make-starline starline-alive? update-starline draw-starline)

  (let ()
    (define-struct starline (start end))

    (define MAX-Z 500)
    (define M-PI 3.14159265358979323846)

    (define (Cosine x) (cos (/ (* x M-PI) 180)))
    (define (Sine x) (sin (- (/ (* x M-PI) 180))))

    (define (starline-alive? star)
      (and (star-alive? (starline-start star))
	   (star-alive? (starline-end star))))

    (define (make-starline-real scale mx my)
      (let* ((ang (random 360))
	     (x (real->int (* (Cosine ang) scale)))
	     (y (real->int (* (Sine ang) scale))))
	(make-starline (make-star x y (- MAX-Z (random 9) 4) mx my)
		       (make-star x y MAX-Z mx my))))

    (define (update-starline s)
      (update-star (starline-start s))
      (update-star (starline-end s)))

    (define (draw-starline buffer s colors max-colors)
      (let* ((start (starline-start s))
	     (end   (starline-end s))
	     (ncolor (quotient (* (star-z start) (sub1 max-colors)) MAX-Z)))
	(recalc-star start)
	(recalc-star end)
	#; (image-putpixel buffer (star-x start) (star-y start) (list-ref colors ncolor))
	(image-putpixel buffer (star-x start) (star-y start) (image-color 255 255 255))
	(image-line buffer
		    (star-x start) (star-y start)
		    (star-x end) (star-y end)
		    (list-ref colors ncolor))))

    (values make-starline-real starline-alive? update-starline draw-starline)))

(define-values (make-wormhole update-wormhole draw-wormhole)

  (let ()
    (define-struct rgb (r g b))

  ;; how many stars to add each iteration. larger numbers usually
  ;; result in a prettier effect, but take more cpu time
  (define STARS-TO-ADD 20)

  (define (set-rgb! rgb r g b)
    (set-rgb-r! rgb r)
    (set-rgb-g! rgb g)
    (set-rgb-b! rgb b))

  (define-struct wormhole
    (zsize msize x y ang stars colors s1-rgb s2-rgb e1-rgb e2-rgb))
  (define WORM-MOVE 9)

  (define (move-color! rgb1 rgb2)
    (define (change-color! setter! getter)
      (when (< (getter rgb1) (getter rgb2))
        (setter! rgb1 (add1 (getter rgb1))))
      (when (> (getter rgb1) (getter rgb2))
        (setter! rgb1 (sub1 (getter rgb1)))))
    (for-each 
      (lambda (x) (change-color! (car x) (cadr x)))
      (list (list set-rgb-r! rgb-r)
	    (list set-rgb-g! rgb-g)
	    (list set-rgb-b! rgb-b)))
    (when (and (= (rgb-r rgb1) (rgb-r rgb2))
               (= (rgb-g rgb1) (rgb-g rgb2))
               (= (rgb-b rgb1) (rgb-b rgb2)))
      (set-rgb! rgb2 (random 255) (random 255) (random 255))))

  (define (move-colors! worm)
    (move-color! (wormhole-s1-rgb worm) (wormhole-s2-rgb worm))
    (move-color! (wormhole-e1-rgb worm) (wormhole-e2-rgb worm))
    (set-wormhole-colors!
      worm
      (blend-palette
	(let ((s1 (wormhole-s1-rgb worm)))
	  (makeColor (rgb-r s1)
		     (rgb-g s1)
		     (rgb-b s1)))
	(let ((e1 (wormhole-e1-rgb worm)))
	  (makeColor (rgb-r e1)
		     (rgb-g e1)
		     (rgb-b e1)))
	50)))

  (define (make-wormhole-user)
    (let ((worm (make-wormhole 20 20 320 240 0 null null #f #f #f #f)))
      (set-wormhole-e1-rgb! worm (make-rgb (random 255)
                                           (random 255)
                                           (random 255)))
      (set-wormhole-e2-rgb! worm (make-rgb (random 255)
                                           (random 255)
                                           (random 255)))
      (set-wormhole-s1-rgb! worm (make-rgb (random 255)
                                           (random 255)
                                           (random 255)))
      (set-wormhole-s2-rgb! worm (make-rgb (random 255)
                                           (random 255)
                                           (random 255)))
      (move-colors! worm)
      worm))

  (define (add-stars! worm num)
    (set-wormhole-stars! worm
      (let ((zsize (wormhole-zsize worm))
            (wx (real->int (wormhole-x worm)))
            (wy (real->int (wormhole-y worm))))
        (let loop ((stars (wormhole-stars worm))
                   (q num))
          (if (positive? q)
            (loop (cons (make-starline zsize wx wy) stars)
                  (sub1 q))
            stars)))))

  (define (update-wormhole worm)
    (if (or (< (wormhole-x worm) 20)
            (< (wormhole-y worm) 20)
            (> (wormhole-x worm) 620)
            (> (wormhole-y worm) 460))
      (set-wormhole-ang!
       worm
       (real->int (calculate-normal-angle
                   (wormhole-x worm)
                   (wormhole-y worm)
                   320 240)))
      (begin
        (set-wormhole-ang!
         worm
         (let ((new-ang (+ (wormhole-ang worm)
                           (- (random 15)
                              (random 7)))))
           (modulo (+ new-ang 360) 360)))
        (when (= (random 60) (random 60))
          (set-wormhole-ang! worm (random 360)))))
    (set-wormhole-x! worm (+ (wormhole-x worm)
                             (* WORM-MOVE
                                (Cosine (wormhole-ang worm)))))
    (set-wormhole-y! worm (+ (wormhole-y worm)
                             (* WORM-MOVE
                                (Sine (wormhole-ang worm)))))
    (when (= (random 20) (random 20))
      (set-wormhole-msize! worm (+ (random 40) 5)))
    (when (< (wormhole-zsize worm) (wormhole-msize worm))
      (set-wormhole-zsize! worm (add1 (wormhole-zsize worm))))
    (when (> (wormhole-zsize worm) (wormhole-msize worm))
      (set-wormhole-zsize! worm (sub1 (wormhole-zsize worm))))
    (add-stars! worm STARS-TO-ADD)
    (for-each (lambda (s) (update-starline s)) (wormhole-stars worm))
    (set-wormhole-stars! worm (filter starline-alive? (wormhole-stars worm)))
    (keyboard-keypressed? 'ESC))

  (define foo1 0)
  (define foo2 0)

  (define (draw-wormhole buffer worm)
    (move-colors! worm)
    (set! foo1 (current-milliseconds))
    (let ((num-colors (length (wormhole-colors worm))))
      (for-each (lambda (s)
                  (draw-starline buffer s (wormhole-colors worm) num-colors))
                (wormhole-stars worm)))
    (set! foo2 (current-milliseconds))
    #;(printf "Draw: ~a  \r" (- foo2 foo1)))

    (values make-wormhole-user update-wormhole draw-wormhole)))

(provide run)
(define (run)
  (easy-init 640 480 16)
  (let* ((wormhole (make-wormhole)))
    (game-loop (lambda () (update-wormhole wormhole))
               (lambda (buffer) (draw-wormhole buffer wormhole))
               (frames-per-second 30)))
  (easy-exit))

(run)
)
