(use-modules (haunt artifact)
             (haunt asset)
             (haunt builder blog)
             (haunt builder atom)
             (haunt builder assets)
             (haunt html)
             (haunt post)
             (haunt reader)
             (haunt reader commonmark)
             (haunt site)
             (web uri)
             (ice-9 match)
             (migalmoreno reader dir-tagging)
             (migalmoreno reader org-mode)
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-26)
             (sxml simple))

(define (org-string->date str)
  "Convert STR, a string in Org format, into a SRFI-19 date object."
  (catch 'misc-error
    (lambda () (string->date str "<~Y-~m-~d ~a ~H:~M>"))
    (lambda (key . parameters) (string->date str "<~Y-~m-~d ~a>"))))

(register-metadata-parser! 'date org-string->date)


;;
;; Components/Utilities
;;

(define (static-page title filename body)
  (lambda (site posts)
    (serialized-artifact filename
                         (with-layout %blog-theme site title body)
                         sxml->html)))

(define* (anchor label url #:key external? extra-classes (extra-attributes '()))
  `(a (@ (href ,url)
         ,@(if extra-classes
               `((class ,(string-append "main__anchor " extra-classes)))
               '((class "main__anchor")))
         ,@(append extra-attributes
                   (if external?
                       '((rel noopener)
                         (target _blank))
                       '())))
      ,label))

(define (bulleted-list entries)
  `(ul (@ (class "list"))
       ,@(map (lambda (i)
                `(li (@ (class "list-item--type-bulleted")) ,i))
              entries)))

(define (blog-entries site posts)
  (if (> (length posts) 0)
      (map (lambda (post)
             `(a (@ (class "post-item")
                    (href ,(blog-post-uri post)))
                 (span (@ (class "post-item__title"))
                       ,(post-title post))
                 (span (@ (class "post-item__date"))
                       ,(date->string* (post-date post)))))
           posts)
      '((p "No blog posts found."))))

(define (portfolio-entries site projects)
  (map (lambda (project)
         `(div (@ (class "project-item"))
               (div (@ (class "project-item__wrapper"))
                (div (@ (class "project-item__heading"))
                     (a (@ (class "project-item__title")
                           (href ,(portfolio-post-uri project)))
                        ,(post-title project)))
                (div (@ (class "project-item__synopsis"))
                     (span ,(post-ref project 'synopsis))))
               (ul (@ (class "tags"))
                   ,@(map (lambda (tag)
                            `(li (@ (class "tag")) ,tag))
                          (post-ref project 'tags)))))
       projects))

(define* (stylesheet name #:key local?)
  `(link (@ (rel "stylesheet")
            (href ,(if local?
                       (string-append "/assets/css/" name ".css")
                       name)))))

(define (script name)
  `(script (@ (type "text/javascript")
              (src ,(string-append "/assets/js/" name ".js")))))

(define (post-uri post prefix)
  (string-append prefix "/" (post-slug post) ".html"))

(define %blog-prefix "/blog")
(define %portfolio-prefix "/projects")
(define (blog-post-uri post) (post-uri post %blog-prefix))
(define (portfolio-post-uri post) (post-uri post %portfolio-prefix))


;;
;; Defaults
;;

(define %domain "migalmoreno.com")
(define %email "mail@migalmoreno.com")
(define %username "migalmoreno")
(define %fullname "Miguel Ángel Moreno")

(define* (logo
          #:key
          (viewBox "0 0 100 100")
          (fill "var(--blue-warmer)")
          (height 20)
          (width 20))
  `(svg (@ (xmlns "http://www.w3.org/2000/svg")
           (viewBox ,viewBox)
           (fill "none")
           (stroke ,fill)
           (height ,height)
           (width ,width)
           (stroke-width "8")
           (stroke-linecap "round"))
        (polyline
         (@ (fill "none")
            (points "25,35 10,50 25,65")))
        (line (@ (x1 "60") (y1 "20") (x2 "40") (y2 "80")))
        (polyline
         (@ (fill "none")
            (points "75,35 90,50 75,65")))))


;;
;; Layouts
;;

(define navbar
  `(header (@ (class "navbar"))
           (nav (@ (class "navbar__nav"))
                (input (@ (class "navbar__mobile-menu")
                     (type "checkbox") (id "mobile-menu")))
                (div (@ (class "navbar__images"))
                     ,(anchor `(span (@ (class "navbar__logo"))
                                     ,(logo)
                                     ,%fullname)
                              "/" #:extra-classes "navbar__link")
                     (label (@ (class "navbar__menu-icon") (for "mobile-menu"))
                            (span (@ (class "menu-icon")))))
                (ul (@ (class "navbar__menu"))
                    ,@(map (lambda (a)
                             `(li (@ (class "menu-item"))
                                  ,(anchor (car a) (cdr a)
                                           #:extra-classes "menu-item__link")))
                           '(("Home" . "/")
                             ("Projects" . "/projects")
                             ("Blog" . "/blog")
                             ("Contact" . "/contact.html")))))))


(define (base-layout site title body)
  `((doctype "html")
    (html
     (head
      (meta (@ (charset "utf-8")))
      (meta (@ (name "viewport")
               (content "width=device-width,initial-scale=1")))
      (title ,(string-append (site-title site) " - " title))
      (link
       (@ (rel "icon")
          (type "image/svg+xml")
          (href ,(string-append
                  "data:image/svg+xml,"
                  (uri-encode
                   (with-output-to-string
                     (lambda ()
                       (sxml->xml (logo #:fill "#c4c4c4")))))))))
      ,(stylesheet "main" #:local? #t))
     (body
      ,navbar
      (div (@ (class "body-container"))
           (main (@ (class "main")) ,body)
           (footer (@ (class "footer"))
                   (div (@ (class "footer__wrapper"))
                        ,(format #f "© 2024 ~a" %fullname))))))))


;;
;; Templates/themes
;;

(define (post-template post)
  `((div (@ (class "post"))
         (h1 (@ (class "main__title")) ,(post-title post))
         (div (@ (class "post__metadata"))
              (div (@ (class "post__metadata-items"))
                   (span (@ (class "post__subtitle"))
                         ,(date->string* (post-date post)))
                   (ul (@ (class "tags"))
                       ,@(map (lambda (tag)
                                `(li (@ (class "tag"))
                                     (a (@ (href
                                            ,(string-append "/feeds/tags/"
                                                            tag ".xml"))
                                           (class "tag__link"))
                                        ,tag)))
                              (post-ref post 'tags)))))
         (div (@ (class "post__container")) ,(post-sxml post)))))

(define (project-template project)
  `((div (@ (class "post project"))
         (h1 (@ (class "main__title")) ,(post-title project))
         (div (@ (class "post__metadata"))
              (div (@ (class "post__metadata-items"))
                   ,(anchor "GitHub"
                            (format #f "https://github.com/~a/~a"
                                    %username (post-title project))
                            #:external? #t
                            #:extra-classes "project__link")
                   ,(anchor "Cgit"
                            (format #f "https://git.~a/~a"
                                    %domain (post-title project))
                            #:external? #t
                            #:extra-classes "project__link")
                   ,(post-ref project 'license))
              (div (@ (class "post__metadata-items"))
                   (h4 (@ (class "post__subtitle"))
                       ,(post-ref project 'synopsis))
                   (ul (@ (class "tags"))
                       ,@(map (lambda (tag)
                                `(li (@ (class "tag")) ,tag))
                              (post-ref project 'tags)))))
         (div (@ (class "project__container"))
              ,@(post-sxml project)))))

(define (blog-template site title posts prefix)
  `((div (@ (class "blog"))
         (div (@ (class "main__title"))
              (h1 (@ (class "blog__title")) ,title)
              (button (@ (class "button button--type-border"))
                      ,(anchor "Feed" "/feed.xml")))
         (div (@ (class "blog-entries"))
              ,@(blog-entries site (posts/reverse-chronological posts))))))

(define (portfolio-template site title projects prefix)
  `((div (@ (class "portfolio"))
         (div (@ (class "main__title"))
              (h1 (@ (class "portfolio__title")) ,title))
         (div (@ (class "portfolio-entries"))
              ,@(portfolio-entries site projects)))))

(define %blog-collections
  `(("Blog" ,(format #f "~a/index.html" %blog-prefix) ,posts/reverse-chronological)))

(define %portfolio-collections
  `(("Projects" ,(format #f "~a/index.html" %portfolio-prefix) ,identity)))

(define %blog-theme
  (theme #:name %username
         #:layout base-layout
         #:post-template post-template
         #:collection-template blog-template))

(define %portfolio-theme
  (theme #:name %username
         #:layout base-layout
         #:post-template project-template
         #:collection-template portfolio-template))


;;
;; Pages
;;

(define index-page
  (lambda (site posts)
    (list
     (serialized-artifact
      "index.html"
      (with-layout
       %blog-theme site "Home"
       `((div
          (@ (class "hero"))
          (h1 (@ (class "hero__title"))
              ,(format #f "Hi, I'm ~a"
                       (string-join
                        (drop-right (string-split %fullname #\space) 1))))
          (p "Software developer with experience in project settings across
different industries. Enthusiastic about building robust solutions following
correct practices. Particularly interested in functional programming."))
         (div (@ (class "blog blog--type-preview"))
              (h2 (@ (class "blog__title")) "Latest Posts"
                  (button (@ (class "button button--type-border"))
                          ,(anchor "See all" "/blog")))
              (div (@ (class "blog-entries"))
                   ,(blog-entries site
                                  (remove (lambda (post)
                                            (post-ref post 'projects))
                                          posts))))
         (div (@ (class "portfolio portfolio--type-preview"))
              (h2 (@ (class "portfolio__title")) "Projects"
                  (button (@ (class "button button--type-border"))
                          ,(anchor "See all" "/projects")))
              (div (@ (class "portfolio-entries"))
                   ,@(portfolio-entries site
                                        (take (filter (lambda (post)
                                                          (post-ref post 'projects))
                                                      posts)
                                              6))))))
      sxml->html))))

(define (contact-entry title text)
  `(div (@ (class "descriptions__wrapper"))
        (dt (@ (class "descriptions__title")) ,title)
        (dd (@ (class "descriptions__text")) ,text)))

(define contact-page
  (static-page
   "Contact"
   "/contact.html"
   `((h1 (@ (class "main__title")) "Contact me")
     (dl (@ (class "list"))
         ,(contact-entry
           '(span "Email")
           '(span (code "mail") " at " (code "$DOMAIN")))
         ,(contact-entry
           '(span "PGP")
           (anchor '(code "4956 DAC8 B077 15EA 9F14  E13A EF1F 69BF 5F23 F458")
                   "/assets/pubkey.asc"
                   #:external? #t))))))

(define not-found-page
  (static-page
   "404 Not found"
   "/404.html"
   `((div (@ (class "not-found"))
          (h1 "404")
          (h1 "Not Found")))))

(define (portfolio)
  (define portfolio-blog
    (blog #:prefix %portfolio-prefix
          #:theme %portfolio-theme
          #:collections %portfolio-collections))
  (lambda (site posts)
    (portfolio-blog site
                    (filter (lambda (post) (post-ref post 'projects)) posts))))

(define (make-builder-with-blog-posts builder)
  (lambda (site posts)
    (builder site (remove (lambda (post) (post-ref post 'projects)) posts))))

(site #:title %fullname
      #:domain %domain
      #:default-metadata
      `((author . ,%fullname)
        (email . ,%email))
      #:readers (list (make-dir-tagging-reader html-reader)
                      (make-dir-tagging-reader org-mode-reader)
                      (make-dir-tagging-reader commonmark-reader))
      #:builders (list index-page
                       (portfolio)
                       (make-builder-with-blog-posts
                        (blog #:prefix %blog-prefix
                              #:theme %blog-theme
                              #:collections %blog-collections))
                       contact-page
                       not-found-page
                       (make-builder-with-blog-posts (atom-feed))
                       (make-builder-with-blog-posts (atom-feeds-by-tag))
                       (static-directory "assets")))
