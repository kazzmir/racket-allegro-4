(module sound mzscheme

  (require (lib "foreign.ss")) (unsafe!)
  (require (prefix a: "private/allegro.ss"))

  (provide (rename a:load-sample load-sound)
	   (rename a:destroy-sample destroy-sound)
	   (rename a:stop-sample stop-sound))

  (define (play-sound-internal loop)
    (letrec ((fun (case-lambda
		    ((sound)
		     (fun sound 255))
		    ((sound volume)
		     (fun sound volume 128))
		    ((sound volume pan)
		     (fun sound volume pan 1000))
		    ((sound volume pan frequency)
		     (a:play-sample sound volume pan frequency loop)))))
      fun))

  (provide play-sound-looped play-sound)
  (define play-sound-looped (play-sound-internal 1))
  (define play-sound (play-sound-internal 0))

)
