#! gosh
;; -*- coding: utf-8 -*-

(use file.util)
(use gauche.interactive)
(use gauche.net)
(use gauche.process)
(use gauche.test)
(use rfc.822)
(use rfc.base64)
(use rfc.mime)
(use srfi-1)

(define-syntax finally
  (syntax-rules ()
    ((finally (expr1 ...) expr2 ...)
      (guard (e (else expr1 ... (raise e))) expr2 ... expr1 ...))))

(define (main args)
  (let* (
      (dir (sys-dirname (car args)))
      (ghttpd (build-path dir "ghttpd"))
      (document-root (build-path dir "document-root"))
      (ghttpd-port 10080)
      (ghttpd-proc
          (run-process 
              (list "gosh" ghttpd "--port" (number->string ghttpd-port) "--document-root" document-root)))
      (gsmtpd (build-path dir "gsmtpd"))
      (spool-dir (build-path dir "spool"))
      (gsmtpd-port 10025)
      (gsmtpd-proc 
          (run-process 
              (list "gosh" gsmtpd "--port" (number->string gsmtpd-port) "--spool-dir" spool-dir))))

    (define (make-show-common . args)
      (let-keywords args (
          (from "foo@example.com") 
          (to "bar@example.com")
          (host "example.com")
          (port 10025))
        (format #f "From: ~a
To: ~a
SMTP host: ~a
SMTP port: ~a

" from to host port)))

    (define (make-show-entry id url tags)
      (format #f "id: ~a
url: ~a
tags: ~a

" id url (string-join tags " ")))

    (define (get-settings-path dir) (build-path dir "settings"))

    (define (terminate-proc proc)
      (sys-kill (process-pid proc) SIGTERM)
      (process-wait proc))

    (define (wait-server port)
      (sys-sleep 1)
      (let (
          (sock (make-socket PF_INET SOCK_STREAM))
          (addr (make <sockaddr-in> :host "127.0.0.1" :port port)))
        (guard (e 
            (<system-error> (socket-close sock) (wait-server port)))
          (socket-connect sock addr)
          (socket-close sock))))

    (define (execute-gr2e . args)
      (let1 load-path-old *load-path*
        (finally ((set! *load-path* load-path-old))
          (let-keywords args (
              (config-dir #f) (lib-path #f) (plugins-module #f) (subcommand #f))
            (let1 command '("gr2e")
              (when config-dir
                (set! command (append command (list "-c" config-dir))))
              (when lib-path
                (set! command (append command (list "-I" lib-path))))
              (when plugins-module
                (set! command (append command (list "-p" plugins-module))))
              (when subcommand (set! command (append command subcommand)))
              (guard (e 
                (else 
                  (test* 
                    (format 
                      #f 
                      "コマンド\"~a\"が実行できること。" 
                      (string-join command " "))
                    #t 
                    (ref e 'message))))
                (gr2e-main command)))))))

    (define (get-temp-dir)
      (build-path (temporary-directory) (number->string (sys-getpid))))

    (define (copy-dir from-dir to-dir)
      (let (
        (files (directory-fold from-dir cons '() 
          :lister (lambda (path seed) 
            (values 
              (directory-list path :add-path? #t :children? #t) 
              (cons path seed))))))
        (for-each (lambda (path) 
          (let* (
              (entry 
                (if (equal? from-dir path) 
                  "" 
                  (substring path 
                    (+ (string-length from-dir) 1) (string-length path))))
              (to (build-path to-dir entry)))
            (cond
              ((null? entry) #t)
              ((file-is-directory? path) (make-directory* to))
              (else (copy-file path to))))) (sort files))))

    (define (clean-dir dir)
      (guard (e (<system-error>)) (remove-directory* dir))
      (make-directory* dir))

    (define (call-with-temp-dir proc)
      (let ((temp-dir (get-temp-dir)))
        (clean-dir temp-dir)
        (proc temp-dir)
        (remove-directory* temp-dir)))

    (define (call-with-copying-settings dir-name proc)
      (call-with-temp-dir 
          (lambda (temp-dir)
            (copy-dir (build-path dir dir-name) temp-dir)
            (proc temp-dir))))

    (define (test-show description config-dir-name args expected)
      (let ((port (open-output-string)))
        (with-output-to-port 
          port
          (lambda ()
            (call-with-copying-settings 
              config-dir-name
              (lambda (temp-dir)
                (execute-gr2e 
                  :config-dir temp-dir 
                  :subcommand (cons "show" args))))))
        (test* description expected (get-output-string port))))

    (define (test-run config-dir-name spool-dir . args)
      (clean-dir spool-dir)

      (let-keywords args (
          (lib-path #f)
          (plugins-module #f)
          (mail-count #f) 
          (test-mail-proc #f) 
          (test-config-proc #f))
        (call-with-copying-settings 
            config-dir-name 
            (lambda (temp-dir)
              (define (spin-until seconds proc expected)
                (let ((time-from (sys-time)))
                  (let loop ()
                    (if (< seconds (- (sys-time) time-from))
                      #f
                      (if (equal? expected (proc))
                        #t
                        (loop))))))

              (test*
                (format #f "~エラーが発生しないこと (~a) 。" config-dir-name)
                #f
                (#/\bERROR:/
                  (call-with-output-string 
                    (lambda (port) 
                      (with-ports 
                        #f 
                        port 
                        port 
                        (lambda ()
                          (execute-gr2e 
                            :lib-path lib-path
                            :plugins-module plugins-module
                            :config-dir temp-dir 
                            :subcommand '("run"))))))))
              (spin-until 
                  2 
                  (lambda () 
                    (let (
                        (files
                          (directory-list 
                            spool-dir :add-path? #t :children? #t)))
                      (if (= 0 (length files))
                        #f
                        (fold 
                          (lambda (path init) (and init (< 0 (file-size path))))
                          #t
                          files))))
                  #t)

              (let (
                  (mail-files 
                      (sort 
                          (directory-list 
                              spool-dir :add-path? #t :children? #t))))
                (if mail-count
                  (test* 
                      (format 
                        #f 
                        "~a通のメールが送信されること (~a) 。" 
                        mail-count 
                        config-dir-name) 
                      mail-count 
                      (length mail-files))
                  #t)
                (if test-mail-proc
                  (let test-mail ((n 0) (files mail-files))
                    (if (null? files)
                      #t
                      (begin
                        (test-mail-proc n (car files))
                        (test-mail (+ n 1) (cdr files)))))
                  #t)
                (if test-config-proc (test-config-proc temp-dir) #t))))))

    (define (path->headers-mime path)
      (define (port->header port)
        (let ((headers (rfc822-header->list port)))
          (fold 
              (lambda (header headers) 
                (cons 
                    (list (car header) (mime-decode-word (cadr header))) 
                    headers)) 
              '() 
              (reverse headers))))

      (call-with-input-file
        path
        (lambda (port)
          (let* (
              (headers (port->header port))
              (mime 
                (mime-parse-message 
                  port headers (cut mime-body->string <> <>))))
            (values headers mime)))))

    (define (mime-body mime) 
      (let ((content (ref mime 'content)))
        (if (string=? (ref mime 'type) "multipart")
          (mime-body (car content))
          content)))

    (define (test-update-last-entry config-dir-name description url entry)
      (test-run
          config-dir-name
          spool-dir
          :test-config-proc (lambda (dir)
            (guard (e (else (test* "last-updateファイルが開けること。" #t #f)))
              (call-with-input-file 
                  (build-path dir "last-entry") 
                  (lambda (port)
                    (let ((doc (read port)))
                      (test* description entry (cadr (assoc url doc))))))))))

    (define (test-edit config-dir-name args test-proc)
      (call-with-copying-settings 
          config-dir-name 
          (lambda (temp-dir)
            (execute-gr2e :config-dir temp-dir :subcommand (cons "edit" args))
            (guard (e 
              (else 
                (test* 
                    (format 
                        #f "~aが開けること。" (get-settings-path temp-dir))
                    #t 
                    (ref e 'message))))
                  (call-with-reading-settings 
                      temp-dir (lambda (settings) (test-proc settings)))))))

    (define (test-edit-mail attr label)
      (let ((value "foo@example.com"))
        (test-edit 
            "edit" 
            (list attr value)
            (lambda (settings)
              (test* 
                  (format #f "~aが~aであること。" label value) 
                  value
                  (cadr (assoc (string->symbol attr) settings)))))))

    (define (test-edit-smtp attr value label)
      (let ((s (x->string value)))
        (test-edit
            "edit"
            (list "smtp" attr s)
            (lambda (settings)
              (let ((smtp (cdr (assoc 'smtp settings))))
                (test*
                    (format #f "~aが~aであること。" label s)
                    value
                    (cadr (assoc (string->symbol attr) smtp))))))))

    (define (test-edit-tag config-dir-name tags)
      (test-edit
          config-dir-name
          (append '("tag" "0") tags)
          (lambda (settings)
            (let* (
                (entry (cadr (assoc 'entries settings)))
                (actual (cdr (assoc 'tags entry)))
                (len (length tags)))
              (test* 
                  (format #f "登録されたタグの数が~aであること。" len)
                  len
                  (length actual))
              (for-each 
                  (lambda (expected actual) 
                    (test* 
                        (format #f "タグ~aが登録されていること。" expected)
                        expected 
                        actual))
                  (sort tags)
                  (sort actual))))))

    (define (call-with-reading-settings dir proc)
      (call-with-input-file
          (get-settings-path dir)
          (lambda (port)
            (proc (read port)))))

    (define (call-with-env name value proc)
      (let ((value-old (sys-getenv name)))
        (finally ((sys-putenv name (if (not value-old) value-old "")))
          (sys-putenv name value)
          (proc))))

    (define (test-body description url body mime)
      (test* 
        description 
        (format #f "URL: ~a\n\n~a\n\nURL: ~a" url body url) 
        (mime-body mime)))

    (define (test-mime-file mime getter path)
      (define (file->bytes path)
        (define (file->bytes-internal buffer port)
          (let1 block (read-block 8192 port)
            (if (eof-object? block)
              buffer
              (file->bytes-internal (string-append buffer block) port))))
        (call-with-input-file path (cut file->bytes-internal "" <>)))

      (test* 
        (format #f "mime part must be \"~a\"." path)
        #t
        (string=? 
          (file->bytes path) (ref (getter (ref mime 'content)) 'content))))

    (define (test-add config-name url tags)
      (call-with-copying-settings 
        config-name
        (lambda (temp-dir)
          (execute-gr2e 
            :config-dir temp-dir :subcommand (append (list "add" url) tags))
          (guard (e 
            (else 
              (test* 
                  (format #f "~aが開けること。" (get-settings-path temp-dir)) 
                  #t 
                  (ref e 'message))))
                (call-with-reading-settings 
                  temp-dir
                  (lambda (settings)
                    (let (
                        (expected-id 256)
                        (entry (cadr (assoc 'entries settings))))
                      (test* 
                          (format #f "idが~aであること。" expected-id) 
                          expected-id 
                          (cadr (assoc 'id entry)))
                      (test* 
                          (format #f "urlが~aであること。" url) 
                          url 
                          (cadr (assoc 'url entry)))
                      (let1 entry-tags (cdr (assoc 'tags entry))
                        (test* 
                          (format #f "タグの数が~aであること。" (length tags))
                          (length tags)
                          (length entry-tags))
                        (fold
                          (lambda (elem seed)
                            (let ((no (+ seed 1)) (expected (car elem)))
                              (test* 
                                (format 
                                  #f "~a番目のタグが~aであること。" no expected)
                                expected
                                (cadr elem))
                              no))
                          0
                          (zip (sort tags) (sort entry-tags)))))))))))

    (for-each wait-server (list ghttpd-port gsmtpd-port))
    (unwind-protect
      (begin
        (test-start "gr2e module")
        (use gr2e)

        (test-section "モジュール")

        (test-module 'gr2e)

        (test-section "runコマンド")

        (test-run "nbsp" spool-dir :mail-count 0)

        (test-run 
            "sendrss1.0" 
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path) 
              (receive (headers mime) (path->headers-mime path)
                (test-body 
                  "RSS1.0の本文を送信すること。" 
                  "http://example.com/about"
                  "テスト内容 (RSS1.0)"
                  mime))))

        (test-run 
            "sendrss" 
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path) 
              (receive (headers mime) (path->headers-mime path)
                (test-body 
                  "RSS2.0の本文を送信すること。" 
                  "http://127.0.0.1:10080/new-entry"
                  "テストのエントリです。" 
                  mime))))

        (test-run 
            "sendatom" 
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path) 
              (receive (headers mime) (path->headers-mime path)
                (test-body 
                  "Atomの本文を送信すること。" 
                  "http://example.com/entry/link"
                  "テストエントリコンテント" 
                  mime))))

        (test-run 
            "from-address" 
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path)
              (receive (headers mime) (path->headers-mime path)
                (test*
                    "指定されたFromアドレスを送信すること。"
                    "foo@example.com"
                    (cadr (assoc "from" headers))))))

        (test-run 
            "to-address"
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path)
              (receive (headers mime) (path->headers-mime path)
                (test*
                    "指定されたToアドレスを送信すること。"
                    "bar@example.com"
                    (cadr (assoc "to" headers))))))

        (test-run 
            "subject-rss1.0"
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path)
              (receive (headers mime) (path->headers-mime path)
                (test*
                    "RSS1.0の件名を送信すること。"
                    "{gr2e}テストタイトル (RSS1.0)"
                    (cadr (assoc "subject" headers))))))

        (test-run 
            "subject"
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path)
              (receive (headers mime) (path->headers-mime path)
                (test*
                    "RSS2.0の件名を送信すること。"
                    "{gr2e}テストエントリ"
                    (cadr (assoc "subject" headers))))))

        (test-run 
            "subject-atom"
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path)
              (receive (headers mime) (path->headers-mime path)
                (test*
                    "Atomの件名を送信すること。"
                    "{gr2e}テストエントリタイトル"
                    (cadr (assoc "subject" headers))))))

        (test-run
            "last-entry"
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path)
              (case n
                ((0)
                  (receive (headers mime) (path->headers-mime path)
                    (test* 
                        "最新の記事を送信すること (RSS2.0) 。"
                        "{gr2e}テストエントリ"
                        (cadr (assoc "subject" headers)))))
                (else 
                  (test* "既読の記事が送信されないこと (RSS2.0) 。" #f #t)))))

        (test-run
            "last-entry-atom"
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path)
              (case n
                ((0)
                  (receive (headers mime) (path->headers-mime path)
                    (test* 
                        "最新の記事を送信すること (Atom) 。"
                        "{gr2e}テストエントリタイトル"
                        (cadr (assoc "subject" headers)))))
                (else (test* "既読の記事が送信されないこと (Atom) 。" #f #t)))))

        (test-run
            "tagged-title"
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path)
              (receive (headers mime) (path->headers-mime path)
                (test* 
                    "タグがひとつの記事を送信すること。"
                    "{gr2e}{タグ}テストエントリ"
                    (cadr (assoc "subject" headers))))))

        (test-run
            "tagged-title2"
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path)
              (receive (headers mime) (path->headers-mime path)
                (test* 
                    "タグがふたつの記事を送信すること。"
                    "{gr2e}{タグ1}{タグ2}テストエントリ"
                    (cadr (assoc "subject" headers))))))

        (test-run
            "local"
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path)
              (receive (headers mime) (path->headers-mime path)
                (test-body 
                  "ローカルのRSSを読み込むこと。" 
                  "http://127.0.0.1:10080/new-entry"
                  "テストのエントリです。" 
                  mime))))

        (call-with-env 
          "HOME"
          "tests/document-root"
          (lambda ()
            (test-run
                "home"
                spool-dir
                :mail-count 1
                :test-mail-proc (lambda (n path)
                  (receive (headers mime) (path->headers-mime path)
                    (test-body 
                      "環境変数HOMEを参照すること。"
                      "http://127.0.0.1:10080/new-entry"
                      "テストのエントリです。" 
                      mime))))))

        (test-run
            "plugin"
            spool-dir
            :lib-path "./tests"
            :plugins-module "plugins"
            :mail-count 1
            :test-mail-proc (lambda (n path)
              (receive (headers mime) (path->headers-mime path)
                (test-body
                  "プラグインを実行すること。"
                  "http://example.com/after-plugin"
                  "プラグインによるエントリです。"
                  mime))))

        (test-run
            "image"
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path)
              (receive (headers mime) (path->headers-mime path)
                (test-mime-file mime cadr "tests/document-root/image.png"))))

        (test-run
            "image2"
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path)
              (receive (headers mime) (path->headers-mime path)
                (test-mime-file mime cadr "tests/document-root/image2.png"))))

        (test-run
            "image3"
            spool-dir
            :mail-count 1
            :test-mail-proc (lambda (n path)
              (receive (headers mime) (path->headers-mime path)
                (test-mime-file mime cadr "tests/document-root/image.png")
                (test-mime-file mime caddr "tests/document-root/image2.png"))))

        (test-run "bug-0001" spool-dir)
        (test-run "bug-0002" spool-dir)
        (test-run "bug-0003" spool-dir)
        (test-run "rss2.0-null-description" spool-dir)

        (test-update-last-entry 
            "update-last-entry" 
            "最新のエントリが追加されること (RSS2.0) 。"
            "http://127.0.0.1:10080/rss2"
            "http://127.0.0.1:10080/new-entry")
        (test-update-last-entry 
            "update-last-entry2" 
            "最新のエントリが更新されること (RSS2.0) 。"
            "http://127.0.0.1:10080/rss2"
            "http://127.0.0.1:10080/new-entry")
        (test-update-last-entry 
            "update-last-entry-atom" 
            "最新のエントリが追加されること (Atom) 。"
            "http://127.0.0.1:10080/atom.xml"
            "http://example.com/entry/link")
        (test-update-last-entry 
            "update-last-entry-atom2" 
            "最新のエントリが更新されること (Atom) 。"
            "http://127.0.0.1:10080/atom.xml"
            "http://example.com/entry/link")

        (test-section "initコマンド")

        (let* ((temp-dir (get-temp-dir)))
          (execute-gr2e :config-dir temp-dir :subcommand '("init"))
          (guard (e 
            (else 
              (test* 
                  (format #f "~aが開けること。" (get-settings-path temp-dir))
                  #t 
                  (ref e 'message))))
                (call-with-reading-settings 
                    temp-dir 
                    (lambda (settings)
                      (test* 
                          "Fromアドレスが設定されていないこと。" 
                          "" 
                          (cadr (assoc 'from settings)))
                      (test* 
                          "Toアドレスが設定されていないこと。" 
                          "" 
                          (cadr (assoc 'to settings)))
                      (test*
                          "エントリがないこと。" 
                          '() 
                          (cdr (assoc 'entries settings)))
                      (test* 
                          "next-idが0であること。" 
                          0 
                          (cadr (assoc 'next-id settings)))
                      (let ((smtp (cdr (assoc 'smtp settings))))
                        (test* 
                            "SMTPホストが127.0.0.1であること。" 
                            "127.0.0.1" 
                            (cadr (assoc 'host smtp)))
                        (test* 
                            "SMTPポートが25であること。" 
                            25 
                            (cadr (assoc 'port smtp))))))))

        (test-section "editコマンド")

        (test-edit-mail "from" "Fromアドレス")
        (test-edit-mail "to" "Toアドレス")
        (test-edit-smtp "host" "example.com" "SMTPホスト")
        (test-edit-smtp "port" 10025 "SMTPポート")
        (test-edit-tag "edit-tag" '("タグ"))
        (test-edit-tag "edit-tag" '("タグ1" "タグ2"))
        (test-edit-tag "edit-tag2" '())
        (test-edit-tag "edit-tag2" '("タグ"))

        (test-section "addコマンド")

        (test-add "add" "http://example.com/" '())
        (test-add "add" "http://example.com/" '("タグ1"))
        (test-add "add" "http://example.com/" '("タグ2" "タグ1"))

        (test-section "removeコマンド")

        (call-with-copying-settings 
            "remove"
            (lambda (temp-dir)
              (execute-gr2e :config-dir temp-dir :subcommand '("remove" "1"))
              (guard (e 
                (else 
                  (test* 
                      (format 
                          #f "~aが開けること。" (get-settings-path temp-dir))
                      #t 
                      (ref e 'message))))
                    (call-with-reading-settings 
                        temp-dir
                        (lambda (settings)
                          (let* (
                              (expected-id 0)
                              (entries (cdr (assoc 'entries settings)))
                              (entry (car entries)))
                            (test* 
                                "エントリの数が1であること。" 
                                1 
                                (length entries))
                            (test* 
                                (format #f "idが~aであること。" expected-id) 
                                expected-id 
                                (cadr (assoc 'id entry)))))))))

        (test-section "showコマンド")

        (let (
            (common (make-show-common))
            (entries 
                (list 
                    (make-show-entry 0 "http://127.0.0.1:10080/rss2" '())
                    (make-show-entry 1 "http://127.0.0.1:10080/rss3" '("タグ"))
                    (make-show-entry 
                        2 "http://127.0.0.1:10080/rss4" '("タグ1" "タグ2")))))
          (test-show "共通の設定を出力すること。" "show" '() common)
          (test-show 
              "複数のエントリの設定を出力すること。" 
              "show2" 
              '() 
              (fold 
                  (lambda (elem init) (string-append init elem)) 
                  common 
                  entries))
          (let loop ((n 0) (entries entries))
            (when (pair? entries)
              (test-show 
                  (format #f "id~aのエントリの設定を出力すること。" n)
                  "show2" 
                  (list (number->string n)) 
                  (car entries))
              (loop (+ n 1) (cdr entries)))))

        (test-end))
      (for-each terminate-proc (list gsmtpd-proc ghttpd-proc))))
  0)

;; vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=scheme
