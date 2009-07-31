;; -*- coding: utf-8 -*-

(define-module gr2e
  (export gr2e-main <gr2e-plugin>)
  (use file.util)
  (use gauche.charconv)
  (use gauche.fcntl)
  (use gauche.interactive)
  (use gauche.parseopt)
  (use rfc.base64)
  (use rfc.http)
  (use rfc.mime)
  (use rfc.uri)
  (use util.list)
  (use util.match)
  (use srfi-13)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use gr2e.gsmtp)
  (use html2text)
  (use html2text.htmlprag)
  (use magic))
(select-module gr2e)

(define-class <gr2e-plugin> () (
    (patterns :init-keyword :patterns :init-form '())
    (get-body :init-keyword :get-body :init-value #f)))

(define-class <gr2e> () (
    (verbose :init-keyword :verbose :init-value #f)
    (name :init-keyword :name :init-value "")
    (plugins :init-keyword :plugins :init-form '())
    (config-dir 
      :init-keyword :config-dir 
      :init-value (build-path (home-directory) ".gr2e"))
    (plugins-module :init-keyword :plugins-module :init-value "gr2e.plugins")))

(define-class <smtp> () (
    (host :init-keyword :host :init-value "localhost")
    (port :init-keyword :port :init-value 25)))

(define-class <settings> () (
    (next-id :init-keyword :next-id :init-value 0)
    (from :init-keyword :from :init-value "")
    (to :init-keyword :to :init-value "")
    (entries :init-keyword :entries :init-form (make-hash-table 'string=?))
    (smtp :init-keyword :smtp :init-value (make <smtp>))))

(define-class <entry> () (
    (id :init-keyword :id :init-value 0)
    (tags :init-keyword :tags :init-value '())
    (url :init-keyword :url :init-value "")
    (last-entry :init-keyword :last-entry :init-value "")))

(define-class <rss-proc> () (
    (rss->items :init-keyword :rss->items :init-value #f)
    (item->link :init-keyword :item->link :init-value #f)
    (item->body :init-keyword :item->body :init-value #f)
    (item->title :init-keyword :item->title :init-value #f)))

(define (alist->smtp l)
  (let ((smtp (make <smtp>)))
    (for-each
        (lambda (elem)
          (case (car elem)
            ('host (set! (ref smtp 'host) (car (cdr elem))))
            ('port (set! (ref smtp 'port) (car (cdr elem))))))
        l)
    smtp))

(define (get-entry-attr ht attr)
  (case attr
    ((id url) (car (hash-table-get ht attr)))
    ((tags) (hash-table-get ht attr))))

(define (list->entry l)
  (let ((ht (alist->hash-table l)) (entry (make <entry>)))
    (for-each
        (lambda (attr) (set! (ref entry attr) (get-entry-attr ht attr))) 
        '(tags url id))
    entry))

(define (hash->settings l)
  (let ((settings (make <settings>)))
    (hash-table-map (alist->hash-table l) 
      (lambda (key value) 
        (case key
          ((next-id from to) (set! (ref settings key) (car value)))
          ('entries
            (for-each
                (lambda (l) 
                    (let ((entry (list->entry l)))
                      (hash-table-put! 
                          (ref settings 'entries) (ref entry 'url) entry)))
                value))
          ('smtp (set! (ref settings 'smtp) (alist->smtp value))))))
    settings))

(define (get-settings-path config-dir) (build-path config-dir "settings"))

(define (load-settings config-dir)
  (let (
    (lock (make <sys-flock>)))
    (if (not (file-is-directory? config-dir))
      (make-directory* config-dir))
    (let ((url-file (get-settings-path config-dir)))
      (if (file-exists? url-file)
        (let ((input-port (open-input-file url-file)))
          (sys-fcntl input-port F_SETLKW lock)
          (let ((l (read input-port)))
            (close-input-port input-port)
            (hash->settings l)))
        (make <settings>)))))

(define (get-sxml-element path sxml) 
  (define (get-first-body elements)
    (if (pair? elements)
      (let ((element (car elements)))
        (if (and (pair? element) (equal? (car element) '@))
          (get-first-body (cdr elements))
          element))
      ""))

  (let1 elem ((sxpath path) sxml)
    (if (pair? elem)
      (get-first-body (cdar elem))
      "")))

(define (multipart? mime)
  (let ((type (ref mime 'type)))
    (or (string=? type "multipart") (string=? type "message"))))

(define (mime->header-data mime)
  (let (
      (data "") 
      (transfer-encoding 
          (if (slot-bound? mime 'transfer-encoding) 
            (ref mime 'transfer-encoding) 
            "base64")))
    (define (data-append s)
      (set! data (string-append data s)))
    (define (data-append-empty-line)
      (data-append "\r\n"))

    ;; example; Content-Type: text/plain; charset=utf-8
    (data-append
        (format #f "Content-Type: ~a/~a" (ref mime 'type) (ref mime 'subtype)))
    (if (ref mime 'parameters)
      (begin
        (let append-parameters ((parameters (ref mime 'parameters)))
          (if (null? parameters)
            #t
            (begin
              (data-append 
                  (format 
                      #f
                      "; ~a=\"~a\""
                      (car (car parameters)) (car (cdr (car parameters)))))
              (append-parameters (cdr parameters)))))
        (data-append "\r\n"))
      #t)
    (data-append 
        (if (multipart? mime)
          ""
          (format #f "Content-Transfer-Encoding: ~a\r\n" transfer-encoding)))
    (let data-append-headers ((headers (ref mime 'headers)))
      (if (null? headers)
        #t
        (begin
          (data-append 
              (format 
                  #f
                  "~a: ~a\r\n" 
                  (car (car headers)) 
                  (mime-encode-word (car (cdr (car headers))))))
          (data-append-headers (cdr headers)))))
    (data-append-empty-line)
    (data-append 
        (if (multipart? mime)
          "This is a multi-part message in MIME format.\r\n\r\n"
          ""))
    data))

(define (mime-boundary mime)
  (let find-boundary ((parameters (ref mime 'parameters)))
    (cond
      ((null? parameters) "")
      ((equal? (car (car parameters)) "boundary") (car (cdr (car parameters))))
      (else (cdr parameters)))))

(define (mime->data mime)
  (let (
      (transfer-encoding 
          (if (slot-bound? mime 'transfer-encoding) 
            (ref mime 'transfer-encoding) 
            "base64"))
      (data (mime->header-data mime)))
    (if (multipart? mime)
      (begin
        (let append-contents ((contents (ref mime 'content)))
          (if (null? contents)
            #t
            (begin
              (set! data (string-append data "--" (mime-boundary mime) "\r\n"))
              (set! data (string-append data (mime->data (car contents))))
              (append-contents (cdr contents)))))
        (set! data (string-append data "--" (mime-boundary mime) "--\r\n")))
      (cond 
        ((equal? transfer-encoding "base64")
          (set! 
              data 
              (string-append 
                data (base64-encode-string (ref mime 'content)) "\r\n")))
        (else (error (format #f "Unknown encoding: ~a" transfer-encoding)))))
    data))

(define (send-mail gr2e settings from to mime)
  (let ((data (mime->data mime)) (smtp (ref settings 'smtp)))
    (gsmtp-sendmail 
      (ref smtp 'host) (ref smtp 'port) from to data (ref gr2e 'verbose))))

(define (gr2e-http-get gr2e host port path query fragment)
  (let (
      (url (format #f "~a~a" host (if port (format #f ":~a" port) "")))
      (path 
        (format 
          #f 
          "~a~a~a" 
          path 
          (if query (format #f "?~a" query) "") 
          (if fragment (format #f "#~a" fragment) ""))))
    (when (ref gr2e 'verbose) (format #t "HTTP GET http://~a~a\n" url path))
    (http-get url path)))

(define (send-rss-mail url rss-proc gr2e settings entry items)
  (define (apply-plugin gr2e rss-proc item)
    (define (apply-plugin-internal plugins rss-proc item link content)
      (if (null? plugins)
        (values link content)
        (let1 plugin (car plugins)
          (define (apply-patterns patterns link content)
            (if (null? patterns)
              (values link content)
              (if ((car patterns) link)
                (receive (link content) 
                    ((ref plugin 'get-body) link rss-proc item)
                  (apply-patterns (cdr patterns) link content))
                (apply-patterns (cdr patterns) link content))))
          (apply-patterns (ref plugin 'patterns) link content))))
    (apply-plugin-internal 
      (ref gr2e 'plugins) 
      rss-proc 
      item 
      ((ref rss-proc 'item->link) item) 
      ((ref rss-proc 'item->body) item)))

  (define (extract-images html)
    (define (extract-images-internal sxml images)
      (if (null? sxml)
        images
        (let1 elem (car sxml)
          (if (pair? elem) 
            (if (equal? (car elem) 'img)
              (extract-images-internal (cdr sxml) (append images (list elem)))
              (extract-images-internal 
                (cdr sxml) (append (extract-images-internal elem '()) images)))
            (extract-images-internal (cdr sxml) images)))))
    (extract-images-internal (html->sxml html) '()))

  (define (fetch-images url tags)
    (fold 
      (lambda (tag seed)
        (define (join-url url1 url2)
          (cond 
            ((#/^[A-Z]+:\/\//i url2) url2)
            ((string-prefix? url2 "/") 
              (receive 
                  (scheme user host port path query fragment) (uri-parse url1)
                (uri-compose 
                  :scheme scheme
                  :userinfo user
                  :host host
                  :port port
                  :path url2)))
            (else
              (receive
                  (scheme user host port path query fragment) (uri-parse url1)
                (let1 dir (sys-dirname path)
                  (uri-compose
                    :scheme scheme
                    :userinfo user
                    :host host
                    :port port
                    :path 
                      (simplify-path 
                        (string-join 
                          (list dir url2) 
                          (if (string-suffix? "/" dir) "" "/")))))))))

        (receive 
            (scheme user host port path query fragment) 
            (uri-parse (join-url url (cadar ((sxpath '(@ src)) tag))))
          (receive 
              (status headers body) 
              (gr2e-http-get gr2e host port path query fragment)
            (append seed (if (string=? status "200") (list body) '())))))
      '()
      tags))

  (if (null? items)
    #t
    (let* (
        (item (car items)) 
        (link ((ref rss-proc 'item->link) item)))
      (if (string=? (ref entry 'last-entry) link)
        #t
        (receive (link content) (apply-plugin gr2e rss-proc item)
          (let (
              (boundary "----=_NextPart_34204_8606_01C88AB5.532EF880")
              (from (ref settings 'from))
              (to (ref settings 'to))
              (body (make <mime-part>)) 
              (mail (make <mime-part>)))
            (set! (ref body 'type) "text")
            (set! (ref body 'subtype) "plain")
            (set! (ref body 'parameters) '(("charset" "utf-8")))
            (set! (ref body 'transfer-encoding) "base64")
            (set! (ref body 'headers) '())
            (set! (ref body 'parent) mail)
            (set! (ref body 'index) 0)
            (set! (ref body 'content) 
              (string-append
                (format #f "URL: ~a\n\n" link)
                (html2text link content)
                (format #f "\n\nURL: ~a" link)))
            (set! (ref mail 'type) "multipart")
            (set! (ref mail 'subtype) "alternative")
            (set! (ref mail 'parameters) (list (list "boundary" boundary)))
            (set! (ref mail 'headers) 
              (list
                (list "From" from)
                (list "To" to)
                (list 
                  "Subject" 
                  (format 
                    #f 
                    "{gr2e}~a~a" 
                    (string-join 
                      (map 
                        (cut format #f "{~a}" <>) 
                        (sort (ref entry 'tags) (cut string<? <> <>)))
                      "") 
                    (string-trim-both ((ref rss-proc 'item->title) item))))))
            (set! (ref mail 'parent) #f)
            (set! (ref mail 'content) (cons body '()))
            (let1 images (extract-images content)
              (when (pair? images)
                (let1 cookie (magic-open MAGIC_MIME)
                  (magic-load cookie "")
                  (fold 
                    (lambda (image seed)
                      (let* (
                          (type-subtype 
                            (string-split 
                              (magic-buffer cookie image (string-size image)) 
                              "/"))
                          (type (car type-subtype))
                          (subtype (cadr type-subtype)))
                        (if (string=? type "image")
                          (begin
                            (let1 mime (make <mime-part>)
                              (set! (ref mime 'type) type)
                              (set! (ref mime 'subtype) subtype)
                              (set! (ref mime 'parameters) '())
                              (set! (ref mime 'transfer-encoding) "base64")
                              (set! (ref mime 'headers) '())
                              (set! (ref mime 'parent) mail)
                              (set! (ref mime 'index) seed)
                              (set! (ref mime 'content) image)
                              (set! 
                                (ref mail 'content) 
                                (append (ref mail 'content) (list mime))))
                            (+ seed 1))
                          seed)))
                    2
                    (fetch-images url images))
                  (magic-close cookie))))

            (send-mail gr2e settings from to mail)
            (send-rss-mail url rss-proc gr2e settings entry (cdr items))))))))

(define (type-of-rss rss)
  (define (skip-processing-instruction rss)
    (let loop ((rss rss))
      (let1 elem (car rss)
        (if (pair? elem)
          (if (#/^\*[A-Z]+\*$/ (symbol->string (car elem)))
            (loop (cdr rss))
            rss)
          (loop (cdr rss))))))

  (let1 tag (caar (skip-processing-instruction rss))
    (case tag
      ((rdf RDF) 'rss1.0)
      ((rss RSS) 'rss2.0)
      ((feed FEED) 'atom)
      (else
        (let1 tag (car (reverse (string-split (symbol->string tag) ":")))
          (cond
            ((string-ci=? tag "rdf") 'rss1.0)
            ((string-ci=? tag "rss") 'rss2.0)
            ((string-ci=? tag "feed") 'atom)
            (else #f)))))))

(define (extract-namespace s)
  (let1 splitter ":"
    (string-join (reverse (cdr (reverse (string-split s splitter)))) splitter)))

(define (tag->symbol namespace tag)
  (string->symbol (string-join (list namespace tag) ":")))

(define (make-atom-proc rss)
  (let1 namespace (extract-namespace (symbol->string (caaddr rss)))
    (let (
        (feed-tag (tag->symbol namespace "feed"))
        (entry-tag (tag->symbol namespace "entry"))
        (link-tag (tag->symbol namespace "link"))
        (content-tag (tag->symbol namespace "content"))
        (title-tag (tag->symbol namespace "title")))
      (make <rss-proc> 
        :rss->items (lambda (rss) ((sxpath (list feed-tag entry-tag)) rss))
        :item->link 
          (lambda (item) (get-sxml-element (list link-tag '@ 'href) item))
        :item->body (lambda (item) (get-sxml-element (list content-tag) item))
        :item->title 
          (lambda (item) (get-sxml-element (list title-tag) item))))))

(define rss2.0-proc 
  (make <rss-proc> 
    :rss->items (lambda (rss) ((sxpath '(rss channel item)) rss))
    :item->link (lambda (item) (get-sxml-element '(link) item))
    :item->body (lambda (item) (get-sxml-element '(description) item))
    :item->title (lambda (item) (get-sxml-element '(title) item))))

(define (make-rss1.0-proc rss)
  (let* (
      (tag-namespace "http://purl.org/rss/1.0/")
      (attr-namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
      (rdf-tag (tag->symbol attr-namespace "RDF"))
      (item-tag (tag->symbol tag-namespace "item"))
      (about-tag (tag->symbol attr-namespace "about"))
      (description-tag (tag->symbol tag-namespace "description"))
      (title-tag (tag->symbol tag-namespace "title")))
  (make <rss-proc>
    :rss->items (lambda (rss) ((sxpath (list rdf-tag item-tag)) rss))
    :item->link (lambda (item) (cadar ((sxpath (list '@ about-tag)) item)))
    :item->body (lambda (item) (get-sxml-element (list description-tag) item))
    :item->title (lambda (item) (get-sxml-element (list title-tag) item)))))

(define (make-rss-proc rss)
  (let ((type (type-of-rss rss)))
    (case type
      ((atom) (make-atom-proc rss))
      ((rss2.0) rss2.0-proc)
      ((rss1.0) (make-rss1.0-proc rss))
      (else (error (format #f "Unknown RSS type: ~a" type))))))

(define (replace-env s)
  (regexp-replace-all 
    #/(?:^|[^$])${(?<name>[A-Z]+)}/ 
    s
    (lambda (m) 
      (let* (
          (name (rxmatch-substring m 'name))
          (value (if (string=? name "HOME") (sys-getenv name) #f)))
        (if value value (format #f "${~a}" name))))))

(define (get-rss-body gr2e entry)
  (let1 entry-url (ref entry 'url)
    (if (string-prefix? "http://" entry-url)
      (receive (scheme user-info host port path query fragment) 
          (uri-parse entry-url)
        (receive (status header body) 
            (gr2e-http-get gr2e host port path query fragment)
          (if (equal? status "200") (ces-convert body "*JP") #f)))
      (call-with-input-file (replace-env entry-url) (cut port->string <>)))))

(define (get-rss gr2e settings entry)
  (guard (e 
      (else (format #t "ERROR: ~a\n~a\n" (ref entry 'url) (ref e 'message))))
    (let1 body (get-rss-body gr2e entry)
      (when body
        (let* (
            (rss (ssax:xml->sxml (open-input-string body) '()))
            (rss-proc (make-rss-proc rss))
            (items ((ref rss-proc 'rss->items) rss)))
          (when (pair? items)
            (send-rss-mail (ref entry 'url) rss-proc gr2e settings entry items)
            (set! 
                (ref entry 'last-entry) 
                ((ref rss-proc 'item->link) (car items)))))))))

(define (load-last-entry settings config-dir)
  (guard (e (<error> (print "INFO: " (ref e 'message))))
    (let* (
        (path (build-path config-dir "last-entry"))
        (port (open-input-file path))
        (ht (alist->hash-table (read port))))
      (close-input-port port)
      (hash-table-map ht 
          (lambda (key value) 
            (guard (e (<error> (print (ref e 'message))))
              (let* (
                  (entries (ref settings 'entries))
                  (entry (hash-table-get entries key)))
                (set! (ref entry 'last-entry) (car value)))))))))

(define (call-with-locking path operation proc)
  (let ((lock (make <sys-flock> :type F_WRLCK)))
    (call-with-output-file
        path
        (lambda (port) (when (sys-fcntl port operation lock) (proc))))))

(define (call-with-locking-settings config-dir proc)
  (call-with-locking (build-path config-dir ".lock.settings") F_SETLKW proc))

(define (call-with-locking-run config-dir proc)
  (call-with-locking (build-path config-dir ".lock.run") F_SETLK proc))

(define (make-common-description settings)
  (let ((smtp (ref settings 'smtp)))
    (format #f "From: ~a
To: ~a
SMTP host: ~a
SMTP port: ~a

" (ref settings 'from) (ref settings 'to) (ref smtp 'host) (ref smtp 'port))))

(define (make-entry-description entry)
  (format #f "id: ~a
url: ~a
tags: ~a

" (ref entry 'id) (ref entry 'url) (string-join (sort (ref entry 'tags)) " ")))

(define (execute-show gr2e args)
  (define (usage gr2e) 
    (format #t "Usage: ~a show [ID]\n" (ref gr2e 'name))
    (exit 0))

  (let* (
      (args (parse-options args (("h|help" => (cut usage gr2e)))))
      (id (if (pair? args) (car args) #f))
      (config-dir (ref gr2e 'config-dir))
      (settings #f))
    (call-with-locking-settings 
        config-dir
        (lambda () (set! settings (load-settings config-dir))))
    (display 
      (if id
        (make-entry-description 
            (find 
                (lambda (entry) (= (ref entry 'id) (string->number id)))
                (hash-table-values (ref settings 'entries))))
        (string-join 
            (append 
                (list (make-common-description settings)) 
                (map 
                    (cut make-entry-description <>) 
                    (sort 
                        (hash-table-values (ref settings 'entries)) 
                        (lambda (entry-a entry-b) 
                          (< (ref entry-a 'id) (ref entry-b 'id))))))
            ""))))
  0)

(define (update-last-entry config-dir entries)
  (call-with-output-file
      (build-path config-dir "last-entry")
      (lambda (port)
        (write 
            (fold 
                (lambda (entry init) 
                  (cons (list (ref entry 'url) (ref entry 'last-entry)) init)) 
                '() 
                (hash-table-values entries))
            port))))

(define (load-plugins gr2e)
  (define (symbol->module-name module)
    (regexp-replace-all #/\./ (symbol->string module) "/"))

  (define (get-plugin-name plugin) (car (reverse (string-split plugin "/"))))

  (for-each 
    (lambda (module)
      (load module)
      (set! 
        (ref gr2e 'plugins)
        (append
          (ref gr2e 'plugins) 
          (list
            ((eval 
              (string->symbol 
                (format #f "get-~a-plugin" (get-plugin-name module)))
              (interaction-environment)))))))
    (library-fold 
      (regexp-replace-all 
          #/\./ (format #f "~a.*" (ref gr2e 'plugins-module)) "/")
      (lambda (name path seed) (cons name seed))
      '())))

(define (execute-run gr2e args)
  (define (usage gr2e) 
    (format #t "Usage: ~a run\n" (ref gr2e 'name))
    (exit 0))

  (let1 args (parse-options args (("h|help" => (cut usage gr2e))))
    (load-plugins gr2e)

    (let ((config-dir (ref gr2e 'config-dir)))
      (call-with-locking-run 
          config-dir
          (lambda ()
            (let ((settings #f))
              (call-with-locking-settings 
                config-dir
                (lambda () (set! settings (load-settings config-dir))))
              (load-last-entry settings config-dir)
              (let ((entries (ref settings 'entries)))
                (hash-table-map 
                  entries
                  (lambda (key value) (get-rss gr2e settings value)))
                (update-last-entry config-dir entries)))))))
  0)

(define (execute-init gr2e args)
  (define (usage gr2e) 
    (format #t "Usage: ~a init\n" (ref gr2e 'name))
    (exit 0))

  (let (
      (args (parse-options args (("h|help" => (cut usage gr2e)))))
      (config-dir (ref gr2e 'config-dir)))
    (make-directory* config-dir)
    (call-with-output-file
      (get-settings-path config-dir)
      (lambda (port)
        (write 
          (list
            (list 'from "")
            (list 'to "")
            (list 'next-id 0)
            (list 'entries)
            (list 'smtp (list 'host "127.0.0.1") (list 'port 25))) 
          port))))
  0)

(define (save-settings config-dir settings)
  (call-with-output-file 
      (get-settings-path config-dir)
      (lambda (port)
        (let ((entries (ref settings 'entries)) (smtp (ref settings 'smtp)))
          (write 
            (list 
              (list 'next-id (ref settings 'next-id))
              (list 'from (ref settings 'from))
              (list 'to (ref settings 'to))
              (append 
                '(entries)
                (map 
                  (lambda (entry)
                    (list 
                      (list 'id (ref entry 'id))
                      (list 'url (ref entry 'url))
                      (append '(tags) (ref entry 'tags))))
                  (hash-table-values entries)))
                (list 
                  'smtp
                  (list 'host (ref smtp 'host))
                  (list 'port (ref smtp 'port))))
            port)))))

(define (call-with-read-write-settings config-dir proc)
  (call-with-locking-settings
      config-dir
      (lambda ()
        (let ((settings (load-settings config-dir)))
          (let ((status (proc settings)))
            (save-settings config-dir settings)
            status)))))

(define (execute-edit gr2e args)
  (define (usage gr2e) 
    (let1 name (ref gr2e 'name)
      (format #t "Usage: ~a edit from <From>\n" name)
      (format #t "       ~a edit to <To>\n" name)
      (format #t "       ~a edit tag <ID> [TAGS...]\n" name)
      (format #t "       ~a edit smtp host <Host>\n" name)
      (format #t "       ~a edit smtp port <Port>\n" name))
    (exit 0))

  (let (
      (args (parse-options args (("h|help" => (cut usage gr2e)))))
      (config-dir (ref gr2e 'config-dir)))
    (call-with-read-write-settings 
      config-dir
      (lambda (settings)
        (let ((entries (ref settings 'entries)) (smtp (ref settings 'smtp)))
          (match args
            (("from" from) (set! (ref settings 'from) from))
            (("to" to) (set! (ref settings 'to) to))
            (("tag" id . tags) 
              (let* (
                  (id (string->number id))
                  (entry 
                    (find 
                      (lambda (entry) (= (ref entry 'id) id)) 
                      (hash-table-values entries))))
                (set! (ref entry 'tags) tags)))
            (("smtp" "host" host) (set! (ref smtp 'host) host))
            (("smtp" "port" port) (set! (ref smtp 'port) (string->number port)))
            (else (usage gr2e)))))))
  0)

(define (execute-add gr2e args)
  (define (usage gr2e) 
    (format #t "Usage: ~a add <URL> [TAGS...]\n" (ref gr2e 'name))
    (exit 0))

  (let1 args (parse-options args (("h|help" => (cut usage gr2e))))
    (when (null? args) (usage gr2e))
    (let (
      (url (car args)) (tags (cdr args)) (config-dir (ref gr2e 'config-dir)))
    (call-with-read-write-settings 
      config-dir
      (lambda (settings)
        (hash-table-put!  
          (ref settings 'entries) 
          url 
          (make <entry> :url url :id (ref settings 'next-id) :tags (sort tags)))
        (set! (ref settings 'next-id) (+ (ref settings 'next-id) 1))))))
  0)

(define (execute-remove gr2e args)
  (define (usage gr2e) 
    (format #t "Usage: ~a remove <ID>\n" (ref gr2e 'name))
    (exit 0))

  (let1 args (parse-options args (("h|help" => (cut usage gr2e))))
    (when (null? args) (usage gr2e))
    (let ((id (car args)) (config-dir (ref gr2e 'config-dir)))
      (call-with-read-write-settings
        config-dir
        (lambda (settings)
          (let (
              (url 
                (ref 
                  (find 
                    (lambda (entry) (= (ref entry 'id) (string->number id)))
                    (hash-table-values (ref settings 'entries)))
                  'url)))
            (hash-table-delete! (ref settings 'entries) url))))))
  0)

(define (gr2e-main args)
  (define (usage gr2e)
    (format #t "Usage: ~a [OPTIONS...] SUBCOMMAND [OPTIONS...]

OPTIONS
  -p: plugins module name
  -I: load-path
  -c --config: configure directory
  -h --help: show this message

SUBCOMMAND
  add: add new RSS entry.
  edit: edit RSS entry.
  remove: remove RSS entry.
  run: fetch RSS.
  show: show entry infomation.

For detail, use --help option for each subcommand.
" (ref gr2e 'name))
    (exit 0))

  (debug-print-width 1024)
  (let ((gr2e (make <gr2e> :name (car args))))
    (let (
        (args 
          (parse-options (cdr args) (
            ("v|verbose" => (lambda () (set! (ref gr2e 'verbose) #t)))
            ("h|help" => (cut usage gr2e))
            ("p=s" => 
              (lambda (plugins-module) 
                (set! (ref gr2e 'plugins-module) plugins-module)))
            ("I=s" => (lambda (path) (append! *load-path* (list path))))
            ("c|config=s" => 
              (lambda (dir) (set! (ref gr2e 'config-dir) dir)))))))
      (if (pair? args)
        (let ((subcommand (string->symbol (car args))) (subargs (cdr args)))
          (case subcommand
            ((run) (execute-run gr2e subargs))
            ((show) (execute-show gr2e subargs))
            ((init) (execute-init gr2e subargs))
            ((edit) (execute-edit gr2e subargs))
            ((add) (execute-add gr2e subargs))
            ((remove) (execute-remove gr2e subargs))
            (else 
              (format #t "Unknown subcommand: ~a\n" subcommand)
              (usage gr2e))))
        (usage gr2e)))))

(provide "gr2e")

;; vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=scheme
