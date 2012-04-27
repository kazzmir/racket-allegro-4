(module exflame mzscheme

(require "../../util.ss"
         "../../keyboard.ss"
         (prefix image- "../../image.ss"))

(require (lib "foreign.ss")) (unsafe!)

;; (require (lib "errortrace.ss" "errortrace"))

(define screen-width 320)
(define screen-height 240)
(define num-spots 40)

;; A pascal style for loop!
(define-syntax for
  (syntax-rules (to by do)
    ((_ start to end by inc do func)
     (let loop ((i start))
       (when (< i end)
	 (func i)
	 (loop (+ i inc)))))))

;; b - box of a number
(define (move-hotspot b)
  (let ((new (+ (unbox b) 
		(- (random 7) 3))))
    (cond
      ((< new 0) screen-width)
      ((> new screen-width) 0)
      (else new))))

(define (clear-memory pointer bytes)
  (for 0 to (/ bytes (ctype-sizeof _int32)) by 1 do 
       (lambda (i) (ptr-set! pointer _uint32 i 0)))
  (for (- bytes (modulo bytes (ctype-sizeof _int32))) to bytes by 1 do
       (lambda (i) (ptr-set! pointer _uint8 i 0))))

(define (draw-hotspots hotspots temp)
  (for-each (lambda (b) (set-box! b (move-hotspot b))) hotspots)
  (clear-memory temp (* (ctype-sizeof _int8) screen-width))
  (for-each (lambda (val)
	      (for (- val 20) to (+ val 20) by 1 do
		   (lambda (c)
		     (when (and (>= c 0) (< c screen-width))
		       (ptr-set! temp _uint8 
				 c (min 192 
					(+ (ptr-ref temp _uint8 c)
					   (- 20 (abs (- val c))))))))))
	    (map (lambda (s) (unbox s)) hotspots)))

(define (try-2 hotspots temp)
  ;; (printf "draw\n")
  (draw-hotspots hotspots temp)
  (for 0 to screen-width by 1 do
       (lambda (x)
	 (image-putpixel-screen x (sub1 screen-height)
				(ptr-ref temp _uint8 x))
	 #;
	 (image-putpixel (image-screen) x (sub1 screen-height)
			 (ptr-ref temp _uint8 x))))
  (for (/ screen-height 2) to (sub1 screen-height) by 1 do
       (lambda (y)
	 (let ((address (image-read-line (image-screen) (+ y 1))))
	   (for 0 to (/ screen-width (ctype-sizeof _uint32)) by 1 do
		(lambda (x)
		  (ptr-set! temp _uint32 x (ptr-ref address _uint32 x))))
	   (for 0 to screen-width by 1 do
		(lambda (x)
		  (ptr-set! temp _uint8 x 
			    (max 0 (sub1 (ptr-ref temp _uint8 x))))))
	   (set! address (image-write-line (image-screen) y))
	   (for 0 to (/ screen-width (ctype-sizeof _uint32)) by 1 do
		(lambda (x)
		  (ptr-set! address _uint32 x (ptr-ref temp _uint32 x)))))))
  (image-unwrite-line (image-screen)))

(provide run)
(define (run)
  (easy-init screen-width screen-height 8 'WINDOWED)
  (let ((p (image-make-palette)))
    (for 0 to 256 by 1 do
       (lambda (c)
	 (let-values (((r g b) (cond
				((< c 64) (values c 0 0))
				((< c 128) (values 63 (- c 64) 0))
				((< c 192) (values 63 63 (- c 192)))
				(else (values 63 63 63)))))
		     (image-set-palette! p c r g b))))
    (image-set-palette! p))
  (image-clear (image-screen))
  (let ((hotspots (let loop ((spots null)
			     (num num-spots))
		    (if (<= num 0)
		      spots
		      (loop (cons (box (random screen-width)) spots) (sub1 num)))))
	(temp (malloc _uint8 screen-width)))
    (let loop ((quit? #f))
      (when (not quit?)
	(try-2 hotspots temp)
	(loop (keypressed? 'ESC)))))
  (easy-exit))

)
