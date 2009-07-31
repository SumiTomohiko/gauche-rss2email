;; -*- coding: utf-8 -*-

;;(define-module plugins.test-plugin
;;  (export get-test-plugin-plugin)
;;  (use gr2e))
(use gr2e)

(define (get-test-plugin-plugin)
  (define (get-body url rss-proc item)
    (values 
      "http://example.com/after-plugin" 
      "<p>プラグインによるエントリです。</p>"))

  (make <gr2e-plugin> 
    :patterns (list #/^http:\/\/example\.com\/plugin$/)
    :get-body get-body))

;; vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=scheme
