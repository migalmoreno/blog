(define-module (portfolio)
  #:use-module (haunt artifact)
  #:use-module (haunt html)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (project
            project?
            project-name
            project-synopsis
            project-link
            project-tags
            project-license
            project-involvement
            project-description
            projects/filter-authored
            portfolio
            portfolio-theme
            portfolio-theme?
            portfolio-name
            portfolio-layout
            portfolio-project-template
            portfolio-collection-template))

(define-record-type <project>
  (make-project name synopsis link tags license involvement description)
  project?
  (name project-name)
  (synopsis project-synopsis)
  (description project-description)
  (tags project-tags)
  (link project-link)
  (license project-license)
  (involvement project-involvement))

(define (projects/filter-authored projects)
  (filter (lambda (project)
            (equal? (project-involvement project) 'author)) projects))

(define* (project #:key
                  name synopsis description tags link involvement
                  (license "Unspecified"))
  (make-project name synopsis link tags license involvement description))

(define-record-type <portfolio-theme>
  (make-portfolio-theme name layout project-template collection-template)
  portfolio-theme?
  (name portfolio-theme-name)
  (layout portfolio-theme-layout)
  (project-template portfolio-theme-project-template)
  (collection-template portfolio-theme-collection-template))

(define* (portfolio-theme #:key
                        name layout project-template collection-template)
  (make-portfolio-theme name layout project-template collection-template))

(define (with-portfolio-layout theme site title body)
  ((portfolio-theme-layout theme) site title body))

(define (render-project theme site project)
  ((portfolio-theme-project-template theme) project))

(define (render-portfolio theme site title projects prefix)
  ((portfolio-theme-collection-template theme) site title projects prefix))

(define* (portfolio #:key theme prefix projects collections (filter identity))
  "Return a procedure that creates pages decorated by THEME for a list of
PROJECTS that satisfy FILTER, whose URLs start with PREFIX, and for
COLLECTIONS."
  (define (make-file-name base-name)
    (if prefix
        (string-append prefix "/" base-name)
        base-name))

  (lambda (site posts)
    (define (project->page project)
      (let ((base-name (string-append (project-name project) ".html")))
        (serialized-artifact (make-file-name base-name)
                             (with-portfolio-layout
                              theme site (project-name project)
                              (render-project theme site project))
                             sxml->html)))

    (define collection->page
      (match-lambda
        ((title file-name)
         (list
          (serialized-artifact (make-file-name file-name)
                               (with-portfolio-layout
                                theme site title
                                (render-portfolio
                                 theme site title projects prefix))
                               sxml->html)))))
    (append (map project->page (filter projects))
            (append-map collection->page collections))))
