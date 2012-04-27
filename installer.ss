#lang scheme

(require make scheme/system scheme/foreign)
(unsafe!)

(define verbose? #f)
(define debug
  (lambda v
    (when verbose?
      (apply printf v))))

(define-syntax-rule (ignore-errors expr)
                    (with-handlers ([exn? (lambda (e) (void))])
                                   expr))

(provide pre-installer)
(define (pre-installer PLTHOME directory-path)
  (define (directory-list* dir)
    (map (lambda (x) (build-path dir x)) (directory-list dir)))
  (define (copy-to-dir file dir)
    (ignore-errors
      (copy-file file (let-values (((base name _) (split-path file)))
                        (build-path dir name)))))
  (define (copy-all from to)
    (make-directory* to)
    (map (lambda (f) (copy-to-dir f to))
         (directory-list* from)))
  (define (every-file base)
    (if (directory-exists? base)
      (let ([files (directory-list* base)])
        (apply append (map every-file files)))
      (list base)))
  ;; #t if the library can be loaded with ffi-lib
  (define (can-load? library)
    (debug "Can I load ~a?\n" library)
    (with-handlers ([exn? (lambda (e) #f)])
                   (ffi-lib library)
                   library))
  (define (any-loadable-files base)
    (let ([all-files (every-file base)])
      (debug "Checking loadable files ~a\n" all-files)
      (filter can-load? all-files)))

  (debug "PLTHOME: ~a\n" PLTHOME)
  (parameterize [(current-directory directory-path)]
                (let ([native (build-path "native" (system-library-subpath #f))])
                  (make-directory* native)
                  (case (system-type)
                    [(unix) 
                     (system "sh build-allegro.sh")
                     (copy-all "allegro-4.2.0/lib/unix" native)]
                    ;; osx, windows, uh.. amiga, commodore, VAX, HAL-9000
                    [else
                      (for-each (lambda (file)
                                  (debug "Copying file ~a\n" file)
                                  (copy-to-dir file native))
                                (any-loadable-files "lib"))
                      ]))))
