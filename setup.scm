#! gosh
;; -*- coding: utf-8 -*-

(use file.filter)
(use file.util)
(use gauche.process)

(define package-contents 
  '("Makefile.in" "configure" "configure.ac" "gr2e.in" "lib" "tests"))
(define package-name "gauche-rss2email")
(define package-version "0.1")

(define (main args)
  (define (copy-dir src dest)
    (for-each 
      (lambda (path)
        (let ((dest (build-path dest path)))
          (if (file-is-regular? path)
            (copy-file path dest :if-exists :supersede :keep-mode #t)
            (make-directory* dest))))
      (reverse 
        (directory-fold 
          src 
          (lambda (path seed) (cons path seed)) 
          '() 
          :lister (lambda (path seed) 
            (if (string=? (sys-basename path) ".svn") 
              (values '() seed) 
              (values 
                (directory-list path :add-path? #t :children? #t) 
                (cons path seed))))))))

  (define (install)
    (define (copy-script src dest)
      (define (replace input output)
        (let (
            (line (read-line input))
            (gosh 
              (read-line 
                (process-output 
                  (run-process '("which" "gosh") :output :pipe :wait #t)))))
          (unless (eof-object? line)
            (display (regexp-replace-all #/@gosh@/ line gosh) output)
            (display "\n" output)
            (replace input output))))

        (file-filter replace :input src :output dest)
        (sys-chmod dest #o755))

    (letrec (
        (lib-dir (gauche-site-library-directory))
        (get-parent 
            (lambda (dir n) 
              (if (= n 0) dir (get-parent (sys-dirname dir) (- n 1)))))
        (base-dir (get-parent lib-dir 4))
        (bin-dir (build-path base-dir "bin"))
        (script-name "gr2e"))
      (copy-script 
          (format #f "~a.in" script-name) (build-path bin-dir script-name)))
    0)

  (define (help) 
    (print "gosh install.scm [install|dist|help]")
    0)

  (define (dist script)
    (define (call-with-temporary-dir proc)
      (let (
          (temp-dir 
            (build-path (temporary-directory) (number->string (sys-getpid)))))
        (guard (e 
          (else
            (remove-directory* temp-dir)
            (raise e)))
          (let ((status (proc temp-dir)))
            (remove-directory* temp-dir)
            status))))

    (call-with-temporary-dir 
      (lambda (top-dir)
        (let* (
            (targets (cons script package-contents))
            (full-name (format #f "~a-~a" package-name package-version))
            (dir (build-path top-dir full-name)))
          (make-directory* dir)
          (for-each 
            (lambda (target)
              (if (file-is-regular? target)
                (copy-file target (build-path dir target) :keep-mode #t)
                (copy-dir target (build-path dir (sys-dirname target)))))
            targets)
          (let (
              (package-file (format #f "~a.tar.bz2" full-name))
              (dest-dir 
                (build-path 
                  (sys-normalize-pathname (current-directory) :absolute #t) 
                  "dist")))
            (define (call-with-chdir dir proc)
              (let1 old-dir (current-directory)
                (sys-chdir dir)
                (proc)
                (sys-chdir old-dir)))

            (make-directory* dest-dir)
            (call-with-chdir
              top-dir
              (lambda ()
                (run-process 
                  (list 
                    "tar" "jcf" (build-path dest-dir package-file) full-name)
                  :wait #t)))))
        0)))

  (let ((script (car args)) (command (string->symbol (cadr args))))
    (case command
      ((install) (install))
      ((dist) (dist script))
      (else (help)))))

;; vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=scheme
