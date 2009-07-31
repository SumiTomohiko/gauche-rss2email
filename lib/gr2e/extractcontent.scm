;;;
;;; extractcontent
;;;

(define-module gr2e.extractcontent
  (export extract-content))
(select-module gr2e.extractcontent)

(define (extract-content html)
  (define (extract-google-ad-section html)
    (let (
        (from (rxmatch-end (#/<!--\s*google_ad_section_start\s*-->/ html)))
        (to (rxmatch-start (#/<!--\s*google_ad_section_end\s*-->/ html))))
      (substring html from to)))

  (if (#/<!--\s*google_ad_section_start\s*-->/ html)
    (extract-google-ad-section html)
    (let loop (
        (max-score 0) 
        (content "")
        (contents (string-split html #/<\/?(?:div|td)\s*[^>]*>/i)))
      (define (count-punctuations s) (length (string-split s #/[、。！？]/)))

      (if (null? contents)
        content
        (let ((score (count-punctuations (car contents))))
          (when (< max-score score)
            (set! content (car contents))
            (set! max-score score))
          (loop max-score content (cdr contents)))))))

;; Epilogue
(provide "gr2e/extractcontent")

;; vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=scheme
