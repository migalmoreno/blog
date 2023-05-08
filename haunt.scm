(use-modules (haunt artifact)
             (haunt asset)
             (haunt builder blog)
             (haunt builder atom)
             (haunt builder assets)
             (haunt html)
             (haunt post)
             (haunt reader)
             (haunt site)
             (portfolio))

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

(define index-page
  (static-page
   "Home"
   "index.html"
   `((div (@ (id "about"))
          (h1 (@ (class "title")) "Hi! Welcome to my site")
          (p ,(string-join
               (list
                "I'm Miguel Moreno, a software engineer based in Spain"
                "and a recent BSc. Computer Science graduate.")))
          (p "My interests currently revolve around the topics below.")
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
        (span (@ (class "text-italic"))
              ,(string-append " (" involvement ")")))
    (dd ,description)))

(define projects-page
  (static-page
   "Projects"
   "/projects.html"
   `((h1 (@ (class "title")) "Projects")
     (p "Below are some of the open source projects I've worked in.")
     (dl (@ (class "bulleted"))
         ,(project-item
           "Tubo" "https://sr.ht/~mmoreno/tubo" "Author"
           "An alternative web front-end for various streaming sites")
         ,(project-item
           "nx-router" "https://sr.ht/~mmoreno/nx-router" "Author"
           "A general-purpose routing extension for the Nyxt browser")
         ,(project-item
           "nx-tailor" "https://sr.ht/~mmoreno/nx-tailor" "Author"
           "A theme manager for the Nyxt browser")
         ,(project-item
           "fdroid.el" "https://sr.ht/~mmoreno/fdroid.el" "Author"
           "An Emacs interface to the F-Droid package repository")
         ,(project-item
           "nyxt.el" "https://sr.ht/~mmoreno/nyxt.el" "Author"
           "A minimal API to interact with the Nyxt browser from Emacs")
         ,(project-item
           "dotfiles" "https://sr.ht/~mmoreno/dotfiles" "Author"
           "My personal configuration based on top of RDE and GNU Guix")
         ,(project-item
           "GNU Guix" "https://guix.gnu.org" "Contributor"
           "A functional and declarative package manager")
         ,(project-item
           "RDE" "https://sr.ht/~abcdw/rde" "Co-maintainer"
           "A configuration framework on top of GNU Guix")
         ,(project-item
           "mpv.el" "https://github.com/kljohann/mpv.el" "Contributor"
           "Emacs helpers to interact with MPV via its IPC interface")
         ,(project-item
           "Nyxt" "https://github.com/atlas-engineer/nyxt" "Contributor"
           "An infinitely extensible power browser")
         ,(project-item
           "pulseaudio-control"
           "https://git.sr.ht/~flexibeast/pulseaudio-control" "Co-maintainer"
           "Emacs helpers to interact with the PulseAudio daemon")
         ,(project-item
           "nx-search-engines"
           "https://github.com/aartaka/nx-search-engines" "Contributor"
           "Collection of easy-to-setup search engines for Nyxt")))))

(define contact-page
  (static-page
   "Contact"
   "/contact.html"
   `((h1 (@ (class "title")) "Contact me")
     (dl
      (div
       (dt (@ (class "text-bold text-italic")) "Sourcehut")
       (dd ,(anchor  "~mmoreno" "https://sr.ht/~mmoreno" #:external? #t)))
      (div
       (dt (@ (class "text-bold text-italic")) "Email")
       (dd (code "mmoreno") " at " (code "$DOMAIN")))
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
       (dd (code "@sloan:$DOMAIN")))))))

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
