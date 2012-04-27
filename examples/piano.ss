(module piano mzscheme

(require "../util.ss"
         "../sound.ss"
         "../keyboard.ss"
         (prefix image- "../image.ss")
         (prefix mouse- "../mouse.ss"))


(require (lib "etc.ss"))

(define-struct note (sound vibrate pitch))
(define-struct position (x1 y1 x2 y2 note))

(define max-notes 16)
(define max-pressure 20)

(define fourth-ratio (expt 2 (/ 5 12)))

(define (upper-forth x)
  (* x fourth-ratio))

(define (lower-forth x)
  (* x (/ fourth-ratio)))

(define (make-notes filename)
  (let ((sound (load-sound filename)))
    (build-list max-notes
		(lambda (n)
		  (let ((pitch (let ((func (if (> n (/ max-notes 2))
					     upper-forth
					     lower-forth)))
				 (let loop ((num (abs (- n (/ max-notes 2))))
					    (p 1000))
				   (if (<= num 0)
				     p
				     (loop (sub1 num) (func p)))))))
		    (make-note sound 0 (real->int pitch)))))))

(define (make-instruments lst)
  (define *dir*
    (let-syntax ((current-module-directory
		 (lambda (stx)
		   (datum->syntax-object
		     stx (current-load-relative-directory)))))
    (current-module-directory)))
  (map (lambda (filename) (make-notes (path->string (build-path *dir* filename)))) lst))

(define (real->int x)
   (inexact->exact (round x)))

(define (note-color vibrate)
  (image-color (real->int (* vibrate (/ 255 max-pressure)))
	       128 0))

(define (draw-instrument buffer instrument y1 y2)
  (let loop ((rest instrument)
	     (num 0))
    (when (not (null? rest))
      (let ((x (real->int (+ 40 (* num (/ 600 max-notes)))))
	    (note (car rest)))
	(image-rectangle-fill buffer x y1 (add1 x) y2
			      (note-color (note-vibrate note)))
	(when (> (note-vibrate note) 0)
	  (set-note-vibrate! note (sub1 (note-vibrate note)))))
      (loop (cdr rest) (add1 num)))))

(define (get-positions board)
  (define (get-set-positions set y1 y2)
    (let loop ((pick '())
	       (rest set)
	       (num 0))
      (if (null? rest)
	pick
	(loop (cons (let ((x (real->int (+ 40 (* num (/ 600 max-notes))))))
		      (make-position x y1 (add1 x) y2 (car rest)))
		    pick)
	      (cdr rest)
	      (add1 num)))))
  (let ((max (length board)))
    (let loop ((pick '())
	       (rest board)
	       (num 0))
      (if (null? rest)
	pick
	(let* ((y1 (+ 10 (* (/ 450 max) num)))
	       (y2 (- (+ y1 (/ 450 max)) 20)))
	  (loop (append pick (get-set-positions (car rest) y1 y2))
		(cdr rest) (add1 num)))))))
 
(define (intersect ax1 ay1 ax2 ay2
		   bx1 by1 bx2 by2)
  (let ((fx1 (if (< ax1 ax2) ax1 ax2))
	(fx2 (if (> ax2 ax1) ax2 ax1))
	(fy1 (if (< ay1 ay2) ay1 ay2))
	(fy2 (if (> ay2 ay1) ay2 ay1)))
    (let ((r (and (<= fx1 bx1)
		  (>= fx2 bx2)
		  (<= fy1 by2)
		  (>= fy2 by1))))
      r)))

(define (find-notes board x1 y1 x2 y2)
  (let loop ((positions (get-positions board))
	     (pick '()))
    (if (null? positions)
      pick
      (let ((position (car positions))
	    (rest (cdr positions)))
	(if (intersect x1 y1 x2 y2
		       (position-x1 position) (position-y1 position)
		       (position-x2 position) (position-y2 position))
	  (loop rest (cons (position-note position) pick))
	  (loop rest pick))))))

(provide run)
(define (run)
  (easy-init 640 480 16)
  (let ((board (make-instruments (list "sounds/shortbeeptone.wav"
				       "sounds/popcork.wav"
				       "sounds/niceday.wav")))
	(last-x (mouse-x))
	(last-y (mouse-y)))
    (game-loop
      (lambda ()
	(when (and (not (mouse-left-click?))
		   (or (not (eq? last-x (mouse-x)))
		       (not (eq? last-y (mouse-y)))))
	  (for-each (lambda (note)
		      (set-note-vibrate! note max-pressure)
		      (play-sound (note-sound note)
				  255 128
				  (note-pitch note)))
		    (find-notes board last-x last-y (mouse-x) (mouse-y))))
	(set! last-x (mouse-x))
	(set! last-y (mouse-y))
	(keypressed? 'ESC))
      (lambda (buffer)
	(image-clear buffer (image-color 255 255 255))
	(let ((max (length board)))
	  (let loop ((rest board)
		     (num 0))
	    (when (not (null? rest))
	      (let* ((y1 (+ 10 (* (/ 450 max) num)))
		     (y2 (- (+ y1 (/ 450 max)) 20)))
		(draw-instrument buffer (car rest) y1 y2))
	      (loop (cdr rest) (add1 num)))))
	(image-circle-fill buffer (mouse-x) (mouse-y)
			   3 (if (mouse-left-click?)
			       (image-color 45 92 200)
			       (image-color 255 0 0)))
	)
      (fps 30)))
  (easy-exit))

(run)

)
