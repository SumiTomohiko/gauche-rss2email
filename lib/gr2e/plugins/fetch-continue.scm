;; -*- coding: utf-8 -*-

(use gauche.charconv)
(use rfc.http)
(use rfc.uri)
(use srfi-13)
(use gr2e)
(use gr2e.extractcontent)
(use nkf)

(define (get-fetch-continue-plugin)
  (make <gr2e-plugin> 
    :patterns (list #//)
    :get-body 
      (lambda (url rss-proc item)
        (let1 body (string-trim-right ((ref rss-proc 'item->body) item))
          (if (string-suffix? ".." body)
            (receive (scheme user host port path query fragment) (uri-parse url)
              (receive (status headers body) 
                  (http-get 
                    (string-append 
                      (if user (format #f "~a@" user) "")
                      host
                      (if port (format #f ":~a" port) ""))
                    (string-append
                      path
                      (if query (format #f "?~a" query) "")
                      (if fragment (format #f "#~a" fragment) "")))
                (if (string=? status "200")
                  (values url (extract-content (nkf '("-w") body)))
                  (values url body))))
            (values url body))))))

;; vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=scheme
