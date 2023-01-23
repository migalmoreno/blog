(use-modules (haunt asset)
             (haunt builder blog)
             (haunt builder atom)
             (haunt builder assets)
             (haunt html)
             (haunt page)
             (haunt post)
             (haunt reader)
             (haunt site)
             (srfi srfi-19))

(define (static-page title filename body)
  (lambda (site posts)
    (make-page filename
               (with-layout conses-haunt-theme site title body)
               sxml->html)))

(define* (anchor label url #:key external?)
  `(a (@ (href ,url)
         ,@(if external? '((target "_blank")) '()))
      ,label))

(define (post-uri site post)
  (string-append "/posts/" (site-post-slug site post) ".html"))

(define index-page
  (static-page
   "Home"
   "index.html"
   `((div (@ (id "about"))
           (h1 (@ (class "title")) "About me")
           (p "Software engineer and recent Computer Science graduate.")
           (p "My interests currently revolve around the following topics.")
           (ul
            (li "Functional programming")
            (li "LISP")
            (li "Web development")
            (li "Operating systems")
            (li "Introspectable and extensible tooling")
            (li "Digital privacy")
            (li "Free and open source software"))))))

(define (project-item label link involvement description)
  `(div
    (dt ,(anchor label link #:external? #t)
        (span (@ (class "text-italic")) ,(string-append " (" involvement ")")))
    (dd ,description)))

(define projects-page
  (static-page
   "Projects"
   "/projects.html"
   `((h1 (@ (class "title")) "Projects")
     (p "Some of the open source projects I've been involved in.")
     (dl (@ (class "bulleted"))
      ,(project-item "Tubo" "https://github.com/efimerspan/tubo" "Author"
                     "A web client for many streaming sites")
      ,(project-item "nx-router" "https://github.com/efimerspan/nx-router" "Author"
                     "A general-purpose routing extension for the Nyxt browser")
      ,(project-item "nx-tailor" "https://github.com/efimerspan/nx-tailor" "Author"
                     "A theme manager for the Nyxt browser")
      ,(project-item "fdroid.el" "https://github.com/efimerspan/fdroid.el" "Author"
                     "An Emacs interface to the F-Droid package repository")
      ,(project-item "nyxt.el" "https://github.com/efimerspan/nyxt.el" "Author"
                     "A minimal API to interact with the Nyxt browser from Emacs")
      ,(project-item "dotfiles" "https://github.com/efimerspan/dotfiles" "Author"
                     "My personal set of configuration files, based on top of RDE and GNU Guix")
      ,(project-item "GNU Guix" "https://guix.gnu.org" "Contributor"
                     "A functional, transactional, and declarative package manager")
      ,(project-item "RDE" "https://sr.ht/~abcdw/rde" "Co-maintainer"
                     "A configuration framework on top of GNU Guix")
      ,(project-item "mpv.el" "https//github.com/kljohann/mpv.el" "Contributor"
                     "Helpers to interact with MPV from Emacs via its IPC interface")
      ,(project-item "Nyxt" "https://github.com/atlas-engineer/nyxt" "Contributor"
                     "An infinitely extensible power browser")
      ,(project-item "pulseaudio-control" "https://git.sr.ht/~flexibeast/pulseaudio-control" "Co-maintainer"
                     "Helpers to interact with the PulseAudio daemon from Emacs")
      ,(project-item "nx-search-engines" "https://github.com/aartaka/nx-search-engines" "Contributor"
                     "A collection of easy-to-setup search engines for Nyxt")))))

(define contact-page
  (static-page
   "Contact"
   "/contact.html"
   `((h1 (@ (class "title")) "Contact me")
     (dl
      (div
       (dt (@ (class "text-bold text-italic")) "Source Forges")
       (dd ,(anchor  "Sourcehut" "https://sr.ht/~conses" #:external? #t))
       (dd ,(anchor "GitHub" "https://github.com/efimerspan" #:external? #t)))
      (div
       (dt (@ (class "text-bold text-italic")) "Email")
       (dd (code "contact") " at " (code "$DOMAIN")))
      (div
       (dt (@ (class "text-bold text-italic")) "PGP")
       (dd ,(anchor "4956 DAC8 B077 15EA 9F14  E13A EF1F 69BF 5F23 F458" "assets/pubkey.asc")))
      (div
       (dt (@ (class "text-bold text-italic")) "IRC")
       (dd (code "ardon") " on " ,(anchor "Libera.chat" "https://libera.chat"))
       (dd (code "nvsop") " on " ,(anchor "OFTC.net" "https://www.oftc.net")))
      (div
       (dt (@ (class "text-bold text-italic")) "Matrix")
       (dd (code "@sloan:$DOMAIN")))))))

(define (stylesheet name)
  `(link (@ (rel "stylesheet")
            (href ,(string-append "/assets/css/" name ".css")))))

(define (script name)
  `(script (@ (type "text/javascript")
              (src ,(string-append "/assets/js/" name ".js")))))

(define navbar
  `(nav
    (div (@ (class "container"))
         (div (@ (id "logo"))
          (h3 ,(anchor "(cons e s)" "/")))
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
         " conses")))

(define (base-layout site title body)
  `((doctype "html")
    (html
     (head
      (meta (@ (charset "utf-8")))
      (title ,(string-append (site-title site) " - " title))
      ,(stylesheet "main"))
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
                        ,(string-append " on " (date->string* (post-date post)))))
                 (posts/reverse-chronological posts))))))

(define %collections
  `(("Blog" "index.html" ,posts/reverse-chronological)))

(define conses-haunt-theme
  (theme #:name "conses"
         #:layout base-layout
         #:post-template post-template
         #:collection-template collection-template))

(site #:title "conses"
      #:domain "conses.eu"
      #:default-metadata
      '((author . "conses")
        (email . "contact@conses.eu"))
      #:readers (list html-reader)
      #:builders (list index-page
                       (blog #:prefix "/posts"
                             #:theme conses-haunt-theme
                             #:collections %collections)
                       projects-page
                       contact-page
                       (atom-feed)
                       (static-directory "assets")))
