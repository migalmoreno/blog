(use-modules (haunt artifact)
             (haunt asset)
             (haunt builder blog)
             (haunt builder atom)
             (haunt builder assets)
             (haunt html)
             (haunt post)
             (haunt reader)
             (haunt site)
             (ice-9 match)
             (srfi srfi-19)
             (syntax-highlight)
             (syntax-highlight lisp)
             (syntax-highlight scheme)
             (syntax-highlight xml)
             (portfolio))


;;
;; Utilities
;;

(define* (highlight-code code #:key (lang 'scheme))
  (let ((lexer (match lang
                 ('scheme lex-scheme)
                 ('lisp lex-lisp)
                 ('xml lex-xml)
                 ('c lex-c)
                 (_ #f))))
    (if lexer
        `(pre (code ,(highlights->sxml (highlight lexer code))))
        code)))

(define (static-page title filename body)
  (lambda (site posts)
    (serialized-artifact filename
                         (with-layout %blog-theme site title body)
                         sxml->html)))

(define* (anchor label url #:key external?)
  `(a (@ (href ,url)
         ,@(if external?
               '((rel noopener)
                 (target _blank))
               '()))
      ,label))

(define (post-uri site post)
  (string-append "/posts/" (site-post-slug site post) ".html"))


(define (project-item label link involvement description)
  `(div
    (dt ,(anchor label link #:external? #t)
        (span (@ (class "text-italic"))
              ,(string-append " (" involvement ")")))
    (dd ,description)))



(define* (stylesheet name #:key local?)
  `(link (@ (rel "stylesheet")
            (href ,(if local?
                       (string-append "/assets/css/" name ".css")
                       name)))))

(define (script name)
  `(script (@ (type "text/javascript")
              (src ,(string-append "/assets/js/" name ".js")))))

(define navbar
  `(header
    (div (@ (id "logo")))
    (input (@ (class "mobile-menu") (type "checkbox") (id "mobile-menu")))
    (label (@ (class "hamburger") (for "mobile-menu"))
           (span (@ (class "hamburger-icon"))))
    (nav
     (ul
      ,@(map (lambda (a)
               `(li ,(anchor (car a) (cdr a))))
             '(("Home" . "/")
               ("Projects" . "/projects.html")
               ("Blog" . "/posts")
               ("Contact" . "/contact.html")))))))

(define footer
  `(footer
    (div (@ (class "container"))
         "© "
         (span (@ (id "year")))
         " Miguel Moreno —"
         (span (@ (id "source"))
               ,(anchor "Source" "https://git.sr.ht/~mmoreno/blog")))))

(define (base-layout site title body)
  `((doctype "html")
    (html
     (head
      (meta (@ (charset "utf-8")))
      (meta (@ (name "viewport")
               (content "width=device-width,initial-scale=1")))
      (title ,(string-append (site-title site) " - " title))
      ,(stylesheet "main" #:local? #t)
      ,(stylesheet "https://use.fontawesome.com/releases/v6.3.0/css/all.css"))
     (body
      ,navbar
      (div (@ (class "container"))
           (main ,body)
           ,footer)
      ,(script "main")))))

(define (post-template post)
  `((h1 (@ (class "title")) ,(post-ref post 'title))
    (span (@ (class "text-italic")) " on " ,(date->string* (post-date post)))
    (div (@ (id "post-container")) ,(post-sxml post))))

(define (collection-template site title posts prefix)
  `((div (@ (class "container"))
         (h1 (@ (class "title")) "Blog")
         (ul
          ,@(map (lambda (post)
                   `(li ,(anchor (post-ref post 'title) (post-uri site post))
                        ,(string-append
                          " on " (date->string* (post-date post)))))
                 (posts/reverse-chronological posts))))))

(define %collections
  `(("Blog" "index.html" ,posts/reverse-chronological)))

(define mmoreno-haunt-theme
  (theme #:name "mmoreno"
         #:layout base-layout
         #:post-template post-template
         #:collection-template collection-template))

(site #:title "Miguel Moreno"
      #:domain "mmoreno.eu"

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
       `((div (@ (id "about"))
              (h1 (@ (class "title")) "Hi, I'm Miguel Ángel!")
              (p "I'm a software engineer based in Barcelona and a recent
Computer Science graduate from the University of Kent.")
              (p "My interests currently revolve around these topics:")
              (ul (@ (class "bulleted"))
                  (li "Functional programming")
                  (li "LISP")
                  (li "Web development")
                  (li "Operating systems")
                  (li "Introspectable and extensible tooling")
                  (li "Digital privacy")
                  (li "Free and open source software")))
         (div (@ (class "blog preview"))
              (h2 "Latest Posts" ,(anchor '(button "See all") "/posts"))
              ,(post-entries site posts))))
      sxml->html))))

(define contact-page
  (static-page
   "Contact"
   "/contact.html"
   `((h1 (@ (class "title")) "Contact me")
     (dl
      (div
       (dt (@ (class "text-bold text-italic")) "Email")
       (dd (code "me") " at " (code "$DOMAIN")))
      (div
       (dt (@ (class "text-bold text-italic")) "PGP")
       (dd ,(anchor "4956 DAC8 B077 15EA 9F14  E13A EF1F 69BF 5F23 F458"
                    "assets/pubkey.asc")))
      (div
       (dt (@ (class "text-bold text-italic")) "IRC")
       (dd (code "ardon") " on " ,(anchor "Libera.chat" "https://libera.chat"))
       (dd (code "nvsop") " on " ,(anchor "OFTC.net" "https://www.oftc.net")))
      (div
       (dt (@ (class "text-bold text-italic")) "Matrix")
       (dd (code "@sloan:conses.eu")))))))

(define not-found-page
  (static-page
   "404 Not found"
   "/404.html"
   `((div (@ (id "not-found"))
          (h1 "404")
          (h1 "Not Found")))))

      #:default-metadata
      '((author . "Miguel Moreno")
        (email . "mmoreno@mmoreno.eu"))
      #:readers (list html-reader)
      #:builders (list index-page
                       (blog #:prefix "/posts"
                             #:theme mmoreno-haunt-theme
                             #:collections %collections)
                       projects-page
                       contact-page
                       (atom-feed)
                       (static-directory "assets")))
