#lang scheme

(require "../util.ss")
(require "../keyboard.ss")

(easy-init 640 480 16)

(printf "Press a key to test readkey\n")

(printf "Key: ~a\n" (readkey))

(printf "Any key pressed = ~a\n" (any-keypressed?))

(simulate-keypress 'F)
(printf "Simulated F = Key: ~a\n" (readkey))

(printf "Press more keys to see the modifiers: alt, ctrl, etc\n")

(game-loop (lambda ()
	     (if (any-keypressed?)
	       (let ((n (readkey)))
		 (if (eq? 'ESC n)
		 #t
		 (begin
		   (printf "Key = ~a. Modifiers = ~a\n" n (key-modifiers))
		   #f)))
	       #f))
	   (lambda (buffer) #f)
	   30)

(printf "Now press a key to end this\n")

(game-loop (lambda () (any-keypressed?))
	   (lambda (buffer) #f)
	   30)

(easy-exit)
