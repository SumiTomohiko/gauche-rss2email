#! gosh
;; -*- coding: utf-8 -*-

(define-module gsmtpd (export gsmtpd-start-server))
(select-module gsmtpd)

(use file.util)
(use gauche.fcntl)
(use gauche.interactive)
(use gauche.net)
(use gauche.selector)
(use srfi-13)
(use srfi-19)

(define (gsmtpd-process-request sock spool-dir verbose)
  (let (
      (read-port (socket-input-port sock)) 
      (write-port (socket-output-port sock)))
    (define (print-log s)
      (if (equal? verbose #t)
        (display s)
        #t))

    (define (send-response s)
      (define (send s)
        (print-log s)
        (display s write-port))
      (send s)
      (send "\r\n"))

    (define (send-250)
      (send-response "250 OK"))

    (guard (e (<system-error>))
      (send-response (format "220 ~a" (sys-gethostname))) ;; banner
      (let child-loop ((body ""))
        (let ((line (read-line read-port)) (quited #f))
          (print-log line)
          (print-log "\r\n")

          (rxmatch-case line 
            (#/^EHLO\s+/ (all) (send-250))
            (#/^MAIL\s+FROM:\s+(.*)$/ (all addr) (send-250))
            (#/^RCPT\s+TO:\s+/ (all) (send-250))
            (#/^DATA$/ (all) 
                (send-response "354 End data with <CR><LF>.<CR><LF>")
                (let receive-body ((data ""))
                  (define (join-data s1 s2)
                    (if (string=? s1 "")
                      s2
                      (string-join (list s1 s2) "\r\n")))

                  (let ((line (read-line read-port)))
                    (print-log line)
                    (print-log "\r\n")
                    (if (string=? line ".")
                      (begin
                        (set! body (join-data data line))
                        (send-250))
                      (receive-body (join-data data line))))))
            (#/^QUIT$/ (all) (send-response "221 Bye") (set! quited #t))
            (else (send-response "502 ERROR") (set! quited #t)))
          (if (equal? quited #f)
            (child-loop body)
            (if (not (string-null? body))
              (let* (
                  (body (string-join (map 
                      (lambda (line) 
                        (if (string=? (substring line 0 (if (< 0 (string-length line)) 1 0)) ".") 
                          (substring line 1 (string-length line)) 
                          line)) 
                      (string-split body #/(\r\n|\r|\n)/)) "\r\n"))
                  (time (current-date))
                  (filename (format #f "~d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d~9,'0d"
                      (date-year time) 
                      (date-month time)
                      (date-day time)
                      (date-hour time)
                      (date-minute time)
                      (date-second time)
                      (date-nanosecond time)))
                  (path (build-path spool-dir filename))
                  (file-port (open-output-file path)))
                (display body file-port)
                (close-output-port file-port))
              #f)))))
    (close-output-port write-port)
    (close-input-port read-port)))

(define (gsmtpd-start-server port spool-dir verbose)
  (let ((sock (make-server-socket 'inet port :reuse-addr? #t)) (terminated #f))
    (define (mainloop sock)
      (let ((selector (make <selector>)))
        (define (accept-client fd flags)
          (let accept ((sock sock))
            (guard (e
              ((<system-error> e) 
                (let ((errno (ref e 'errno))) 
                  (if (or (equal? errno EAGAIN) (equal? errno ENETDOWN) (equal? errno EPROTO) (equal? errno ENOPROTOOPT) (equal? errno EHOSTDOWN) (equal? errno ENONET) (equal? errno EHOSTUNREACH) (equal? errno EOPNOTSUPP) (equal? errno ENETUNREACH))
                    (accept sock)
                    (raise e)))))
                (let ((client-sock (socket-accept sock)))
                  (if (equal? (class-of client-sock) <socket>)
                    (begin
                      (gsmtpd-process-request client-sock spool-dir verbose)
                      (socket-close client-sock))
                    (accept sock))))))

        (selector-add! selector (socket-fd sock) accept-client '(r))
        (selector-select selector 1000000)
        (if (not terminated)
          (mainloop sock)
          #t)))

    (set-signal-handler! SIGTERM (lambda (sig) (set! terminated #t)))
    (set-signal-handler! SIGPIPE #f)
    (let ((fd (socket-fd sock)))
      (sys-fcntl fd F_SETFD (logior O_NONBLOCK (sys-fcntl fd F_GETFD))))
    (mainloop sock)
    (socket-close sock)))

(provide "gsmtpd")

;; vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=scheme
