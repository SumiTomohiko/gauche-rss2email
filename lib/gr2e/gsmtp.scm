;; -*- coding: utf-8 -*-

(define-module gr2e.gsmtp 
  (export gsmtp-sendmail)
  (use gauche.net))
(select-module gr2e.gsmtp)

(define (gsmtp-sendmail host port from to body verbose)
  (let* (
      (sock (make-client-socket 'inet host port))
      (input-port (socket-input-port sock))
      (output-port (socket-output-port sock)))
    (define (display-log s)
      (if (equal? verbose #t)
        (display s)
        #t))

    (define (receive-response)
      (let ((line (read-line input-port)))
        (display-log line)
        (display-log "\n")
        (if (string=? " " (substring line 3 4))
          line
          (receive-response))))

    (define (line->code line)
      (car (string-split line " ")))

    (define (raise-error line)
      (error (format #f "Error: ~a" line)))

    (define (raise-if-error line)
      (if (string=? "2" (substring (line->code line) 0 1))
        #t
        (raise-error line)))

    (define (send-string s)
      (display-log s)
      (display s output-port))

    (define (send-command command)
      (send-string command)
      (send-string "\r\n"))

    (raise-if-error (receive-response)) ;; receive banner
    (send-command (format #f "EHLO ~a" (sys-gethostname)))
    (raise-if-error (receive-response))
    (send-command (format #f "MAIL FROM: ~a" from))
    (raise-if-error (receive-response))
    (send-command (format #f "RCPT TO: ~a" to))
    (raise-if-error (receive-response))
    (send-command "DATA")
    (let ((line (receive-response)))
      (if (string=? "354" (line->code line))
        #t
        (raise-error line)))
    (let ((lines (string-split body "\r\n")))
      (define (send-lines lines)
        (if (null? lines)
          #t
          (let ((line (car lines)))
            (send-string 
                (if (string=? "" line) 
                  line 
                  (if (string=? "." (substring line 0 1)) 
                      "." 
                      "")))
            (send-string line)
            (send-string "\r\n")
            (send-lines (cdr lines)))))

        (send-lines lines))
    (send-command ".")
    (raise-if-error (receive-response))
    (send-command "QUIT")
    (raise-if-error (receive-response))
    (socket-close sock)))

(provide "gr2e/gsmtp")

;; vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=scheme
