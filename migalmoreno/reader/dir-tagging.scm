;; Copied from https://github.com/Tass0sm/Tass0sm.github.io/blob/master/haunt/reader/dir-tagging-reader.scm
(define-module (migalmoreno reader dir-tagging)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (haunt reader)
  #:export (make-dir-tagging-reader))

(define file-name-separator-char (string-ref file-name-separator-string 0))

(define (remove-last lst)
  (if (or (null? lst) (null? (cdr lst)))
      '()
      (cons (car lst) (remove-last (cdr lst)))))

(define (list-middle lst)
  (if (null? lst)
      '()
      (remove-last (cdr lst))))

(define (dir-tagging-reader-proc reader-proc)
  ;; Returns a lambda which reads a post from a filename using READER-PROC and
  ;; augments the metadata with the directories in the filename.
  (lambda (filename)
    (let* ((dirs (list-middle (string-split filename file-name-separator-char)))
           (dir-metadata (map (lambda (d) (cons (string->symbol d) #t)) dirs)))
      (let-values (((metadata sxml) (reader-proc filename)))
        (values (append dir-metadata metadata) sxml)))))

(define (make-dir-tagging-reader reader)
  (make-reader (reader-matcher reader)
               (dir-tagging-reader-proc (reader-proc reader))))
