(load "E:/0 wehiby/ssax/web_tem/templates/SXML-tree-trans.scm")


;; Load content definitions from the templates directory
(load "E:/0 wehiby/ssax/web_tem/templates/page1.sxslt")
(load "E:/0 wehiby/ssax/web_tem/templates/page2.sxslt")
(load "E:/0 wehiby/ssax/web_tem/templates/page3.sxslt")

;; Load XSLT templates from the templates directory
(load "E:/0 wehiby/ssax/web_tem/templates/index.sxslt")
(load "E:/0 wehiby/ssax/web_tem/templates/sections.sxslt")
(load "E:/0 wehiby/ssax/web_tem/templates/footer.sxslt")
(load "E:/0 wehiby/ssax/web_tem/templates/sections.sxslt")

;; Define entag function
(define (entag tag elems)
  (string-append "<" (symbol->string tag) ">"
                 (apply string-append
                        (map (lambda (e)
                               (if (string? e)
                                   e
                                   (symbol->string e))) elems))
                 "</" (symbol->string tag) ">"))

;; Define universal-conversion-rules
(define universal-conversion-rules
  `((^
      ((*DEFAULT*       ; local override for attributes
        . ,(lambda (attr-key . value) (enattr attr-key value))))
      . ,(lambda (trigger . value) (cons '^ value)))
    (*DEFAULT* . ,(lambda (tag . elems) (entag tag elems)))
    (*TEXT* . ,(lambda (trigger str) 
                 (if (string? str) (string->goodHTML str) str)))
    (n_        ; a non-breaking space
     . ,(lambda (tag . elems)
          (cons "&nbsp;" elems)))))


;; Define string-contains function
(define (string-contains str substr)
  (let loop ((i 0))
    (cond ((>= i (- (string-length str) (string-length substr))) #f)
          ((string=? (substring str i (+ i (string-length substr))) substr) i)
          (else (loop (+ i 1))))))

;; Define string-replace function
(define (string-replace str old new)
  (let loop ((str str) (result '()))
    (if (string-contains str old)
        (let ((pos (string-contains str old)))
          (loop (substring str (+ pos (string-length old)))
                (append result (list (substring str 0 pos) new))))
        (string-append (apply string-append (reverse result)) str))))

;; Define string->goodHTML function
(define (string->goodHTML str)
  (string-replace (string-replace (string-replace (string-replace (string-replace str "&" "&amp;") "<" "&lt;") ">" "&gt;") "\"" "&quot;") "'" "&#39;"))

;; Define ssax:output-xml function
(define (ssax:output-xml content)
  ;; Implementation of ssax:output-xml
  (display (pre-post-order content universal-conversion-rules)))

;; Define function to generate individual pages in the output directory
(define (generate-page content title filename)
  (with-output-to-file (string-append "output/" filename)
    (lambda ()
      (ssax:output-xml
       `(html
         (head (title ,title))
         (body
          (h1 ,title)
          ,@content))))))

;; Define function to generate the index page in the output directory
(define (generate-index)
  (with-output-to-file "output/index.html"
    (lambda ()
      (ssax:output-xml
       `(html
         (head (title "Index"))
         (body
          (h1 "Index of Pages")
          (ul
           (li (a (@ (href "page1.html")) "Page 1"))
           (li (a (@ (href "page2.html")) "Page 2"))
           (li (a (@ (href "page3.html")) "Page 3")))))))))

;; Generate the individual pages
(generate-page page1-content "Page 1" "page1.html")
(generate-page page2-content "Page 2" "page2.html")
(generate-page page3-content "Page 3" "page3.html")

;; Generate the index page
(generate-index)