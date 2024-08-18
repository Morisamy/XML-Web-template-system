;; Load SXML tree transformation library
(load "SXML-tree-trans.scm")

;; Load content definitions from the templates directory
(define content-definitions
  (list "page1.sxslt" "page2.sxslt" "page3.sxslt"))
(for-each load content-definitions)

;; Load XSLT templates from the templates directory
(define xslt-templates
  (list "index.sxslt" "sections.sxslt" "footer.sxslt" "sections.sxslt"))
(for-each load xslt-templates)

;; Define entag function
(define (entag tag elems)
  (string-append "<" (symbol->string tag) ">"
                 (string-join (map (lambda (e)
                                      (if (string? e)
                                          e
                                          (symbol->string e)))
                                    elems)
                               "")
                 "</" (symbol->string tag) ">"))
(define (string-join lst sep)
  (fold (lambda (x y) (string-append x sep y)) (car lst) (cdr lst)))
;; Define enattr function
(define (enattr attr-key value)
  (string-append (symbol->string attr-key) "=\"" value "\""))
(define (string-join lst sep)
  (define (loop lst result)
    (if (null? lst)
        result
        (loop (cdr lst) (string-append result sep (car lst)))))
  (if (null? lst)
      ""
      (loop (cdr lst) (car lst))))

;; Define universal-conversion-rules
(define universal-conversion-rules
  `((^
     ((*DEFAULT* . ,(lambda (attr-key . value) (enattr attr-key value))))
     . ,(lambda (trigger . value) (cons '^ value)))
    (*DEFAULT* . ,(lambda (tag . elems) (entag tag elems)))
    (*TEXT* . ,(lambda (trigger str) str))))

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
      (display  "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\"><body><h1>Index of Pages</h1><ul>")
      (display "<li><a href=\"page1.html\">Page 1</a></li>")
      (display "<li><a href=\"page2.html\">Page 2</a></li>")
      (display "<li><a href=\"page3.html\">Page 3</a></li>")
      (display "</ul></body></html>"))))
  

  ;; Generate the individual pages
(generate-page page1-content "Page 1" "page1.html")
(generate-page page2-content "Page 2" "page2.html")
(generate-page page3-content "Page 3" "page3.html")
 
;; Generate the index page
(generate-index)
