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


;;
;; Defaults
;;

(define %domain "mianmoreno.com")
(define %email "me@mianmoreno.com")
(define %username "mianmoreno")
(define %fullname "Miguel Ángel Moreno")
(define %projects
  (list
   (project
    #:name "tubo"
    #:synopsis "An alternative web front-end for various streaming sites"
    #:link "https://git.mianmoreno.com/tubo"
    #:tags '("clojure" "clojurescript" "privacy")
    #:license "GPL-3.0+"
    #:involvement 'author
    #:description
    `((p "Tubo is an alternative web frontend to some of the most popular
streaming sites, including:")
      (ul (@ (class "bulleted"))
          (li "YouTube")
          (li "SoundCloud")
          (li "media.ccc.de")
          (li "PeerTube")
          (li "Bandcamp"))
      (p "It acts like a privacy-friendly proxy that compiles all the content
from the above sites and presents it to you in a distraction-free interface. It
gathers the necessary data via "
         ,(anchor "NewPipeExtractor"
                  "https://github.com/TeamNewPipe/NewPipeExtractor")
         ", the library that powers the popular "
         ,(anchor "NewPipe" "https://github.com/TeamNewPipe/NewPipe") " Android
app, which means no official APIs are used. The ultimate goal behind this
project is to offer a web-based alternative to NewPipe for desktop and
non-Android users.")
      (p "Currently, it features an audio-only player and basic search and
watch functionality, but support is planned for user playlists, stream
downloads, offline mode, and more.")
      (figure
       (img (@ (src "https://files.mianmoreno.com/tubo_channel.jpg")
               (style "width:100%")
               (alt "Tubo channel page screenshot")))
       (figcaption "Channel page view"))))
   (project
    #:name "nx-router"
    #:synopsis "A general-purpose routing extension for the Nyxt browser"
    #:link "https://git.mianmoreno.com/nx-router"
    #:tags '("common-lisp" "nyxt" "browser")
    #:license "BSD 3-Clause"
    #:involvement 'author
    #:description
    `((p "nx-router is a full-fledged URL routing extension for the "
         ,(anchor "Nyxt" "https://nyxt.atlas.engineer") " browser.")
      (p "It offers a convenient wrapper around the built-in resource handling
functionality in Nyxt by introducing " (code "route") " objects users can
include in their configuration to define redirects, blocklists, resource
openers, among other, with an easy syntax.")
      (p "It aims to be simple while staying composable and flexible.
Routes are defined like this:")
      ,(highlight-code
        "(make-instance 'router:redirector
  :trigger \"https://([\\w'-]+)\\.fandom.com/wiki/(.*)\"
  :redirect-url \"https://breezewiki.com/\\1/wiki/\\2\")"
        #:lang 'lisp)
      (p "You can also set reverse redirects so that certain URLs get recorded
with their original path, which is useful if you don't want to have to deal
with unmaintained instances of privacy frontends.")
      ,(highlight-code
        "(make-instance 'router:redirector
  :trigger (match-regex \"https://.*google.com/search.*\")
  :redirect-url (quri:uri \"http://localhost:5000\")
  :original-url (quri:uri \"https://www.google.com\"))"
        #:lang 'lisp)))
   (project
    #:name "nx-tailor"
    #:synopsis "A theme manager for the Nyxt browser"
    #:link "https://git.mianmoreno.com/nx-tailor"
    #:tags '("common-lisp" "nyxt" "browser")
    #:license  "BSD 3-Clause"
    #:involvement 'author
    #:description
    `((p "nx-tailor is a theme manager for the "
         ,(anchor "Nyxt" "https://nyxt.atlas.engineer") " browser. It leverages
the built-in " (code "nyxt/theme") " library to allow defining multiple themes
to switch between at browser runtime.")
      (p "It also has a timer functionality to automatically change them
depending on the time of the day.")
      (figure
       (video (@ (src "https://files.mianmoreno.com/nx_tailor.mp4")
                 (style "width:100%")
                 (autoplay "true")
                 (controls "true")))
       (figcaption "Changing the theme via the prompt buffer"))))
   (project
    #:name "fdroid.el"
    #:synopsis "An Emacs interface to the F-Droid package repository"
    #:link "https://git.mianmoreno.com/fdroid.el"
    #:tags '("emacs" "emacs-lisp" "fdroid")
    #:license "GPL-3.0+"
    #:involvement 'author
    #:description
    `((p "fdroid.el is a simple completion-based interface to work with
F-Droid packages from Emacs.")
      (p "Having to deal with Android emulators quite often and needing to
install packages on initial setup, I developed this project to be able to quickly
manage F-Droid packages without having to resort to the F-Droid website or
having to download APKs manually.")))
   (project
    #:name "nyxt.el"
    #:synopsis "A minimal API to interact with the Nyxt browser from Emacs"
    #:link "https://git.mianmoreno.com/nyxt.el"
    #:tags '("emacs" "emacs-lisp" "nyxt")
    #:license "GPL-3.0+"
    #:involvement 'author
    #:description
    `((p "nyxt.el is a minimal API to interact with Nyxt from Emacs.")
      (p "It contains a useful helper " (code "nyxt-run") " to run a Nyxt
process seamlessly from Emacs and provides optional integration with the
Emacs X Window Manager (EXWM).")))
   (project
    #:name "dotfiles"
    #:synopsis "Personal configuration based on top of RDE and GNU Guix"
    #:link "https://git.mianmoreno.com/dotfiles"
    #:tags '("guix" "rde" "dotfiles")
    #:license "GPL-3.0+"
    #:involvement 'author
    #:description
    `((p "My personal set of configuration files crafted on top of Guix and
RDE. The project is focused on a multi-user setup, with a development build for
my local machine, a home configuration on top of a foreign distribution for my
smartphone, and a self-hosted setup for my personal VPS.")
      (p "Previously, I used to maintain this project as a personal Guix
channel, but over time I found this to be unsustainable, so I now contribute
packages, services, and features upstream as much as I can.")))
   (project
    #:name "blog"
    #:synopsis "Personal site and blog"
    #:link "https://git.mianmoreno.com/blog"
    #:tags '("scheme" "org-mode" "haunt")
    #:license "GPL-3.0+"
    #:involvement 'author
    #:description
    `((p "My personal blog built with the "
         ,(anchor "Haunt" "https://dthompson.us/projects/haunt.html")
         " static site generator, which has allowed me to write the entire site
as a Guile Scheme program, and "
         ,(anchor "ox-haunt" "https://git.sr.ht/~jakob/ox-haunt")
         ", an Org mode export back-end to generate the corresponding HTML
files for the blog entries.")
      (p "For the most part, I've followed the Haunt guidelines outlined in its
manual and use the default utilities, although I've  added a custom "
         (code "portfolio") " builder to make it more convenient to list and
describe my personal projects and contributions.")))
   (project
    #:name "GNU Guix"
    #:synopsis "A functional and declarative package manager"
    #:link "https://guix.gnu.org"
    #:tags '("scheme" "linux" "gnu")
    #:involvement 'contributor)
   (project
    #:name "RDE"
    #:synopsis "A configuration framework on top of GNU Guix"
    #:link "https://sr.ht/~abcdw/rde"
    #:tags '("scheme" "linux" "dotfiles")
    #:involvement 'co-maintainter)
   (project
    #:name "mpv.el"
    #:synopsis "Emacs helpers to interact with MPV via its IPC interface"
    #:link "https://github.com/kljohann/mpv.el"
    #:tags '("emacs" "mpv" "emacs-lisp")
    #:involvement 'co-maintainer)
   (project
    #:name "Nyxt"
    #:synopsis "An infinitely extensible power browser"
    #:link "https://github.com/atlas-engineer/nyxt"
    #:tags '("browser" "common-lisp" "emacs")
    #:involvement 'contributor)
   (project
    #:name "pulseaudio-control"
    #:synopsis "Emacs helpers to interact with the PulseAudio daemon"
    #:link "https://sr.ht/~flexibeast/pulseaudio-control"
    #:tags '("emacs" "emacs-lisp" "pulseaudio")
    #:involvement 'co-maintainer)
   (project
    #:name "nx-search-engines"
    #:synopsis "A collection of easy-to-setup search engines for Nyxt"
    #:link "https://github.com/aartaka/nx-search-engines"
    #:tags '("nyxt" "common-lisp" "browser")
    #:involvement 'contributor)))


;;
;; Components
;;

(define (post-entries site posts)
  `(ul
    ,@(map (lambda (post)
             `(a (@ (class "post")
                    (href ,(post-uri site post)))
                 (span (@ (class title)) ,(post-ref post 'title))
                 (span (@ (class "date")) ,(date->string* (post-date post)))))
           posts)))

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
    (input (@ (class "mobile-menu") (type "checkbox") (id "mobile-menu")))
    (div
     (div (@ (id "logo")) ,(anchor %fullname "/"))
     (label (@ (class "hamburger") (for "mobile-menu"))
            (span (@ (class "hamburger-icon")))))
    (nav
     (ul
      ,@(map (lambda (a)
               `(li ,(anchor (car a) (cdr a))))
             '(("Home" . "/")
               ("Projects" . "/projects")
               ("Blog" . "/posts")
               ("Contact" . "/contact.html")))))))

(define footer
  `(footer
    (div (@ (class "container"))
         ,(anchor '(i (@ (class "fa-solid fa-envelope")))
                  (string-append "mailto:" %email)
                  #:external? #t)
         ,(anchor '(i (@ (class "fa-brands fa-linkedin")))
                  "https://linkedin.com/in/mianmoreno"
                  #:external? #t)
         ,(anchor '(i (@ (class "fa-brands fa-git-alt")))
                  (string-append "https://git." %domain)
                  #:external? #t)
         ,(anchor '(i (@ (class "fa-regular fa-circle")))
                  "https://sr.ht/~mmoreno"
                  #:external? #t)
         ,(anchor '(i (@ (class "fa-brands fa-github")))
                  "https://github.com/mianmoreno"
                  #:external? #t)
         ,(anchor '(i (@ (class "fa-brands fa-gitlab")))
                  "https://gitlab.com/mianmoreno"
                  #:external? #t))
    (div (@ (class "container"))
         "© "
         (span (@ (id "year")))
         ,(format #f " ~a —" %fullname)
         (span (@ (id "source"))
               ,(anchor "Source" (format #f "https://git.~a/blog" %domain))))))

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


;;
;; Templates/themes
;;

(define (post-template post)
  `((button (@ (class "back"))
            (i (@ (class "fa-solid fa-caret-left")))
            (a (@ (href "/posts")) "Back to posts"))
    (h1 (@ (class "title")) ,(post-ref post 'title))
    (span (@ (class "text-italic")) " on " ,(date->string* (post-date post)))
    (ul (@ (class "tags"))
        ,@(map (lambda (tag)
                 `(li (a (@ (href ,(string-append "/feeds/tags/" tag ".xml")))
                         ,tag)))
               (assq-ref (post-metadata post) 'tags)))
    (div (@ (class "post-container")) ,(post-sxml post))))

(define (project-template project)
  `((button (@ (class "back"))
            (i (@ (class "fa-solid fa-caret-left")))
            (a (@ (href "/projects")) "Back to projects"))
    (h1 ,(project-name project))
    (h4 (@ (class "text-italic text-faint")) ,(project-synopsis project))
    (div (@ (class "metadata"))
         (span (i (@ (class "fa-brands fa-git-alt")))
               ,(anchor (project-link project)
                        (string-append (project-link project) "/about")
                        #:external? #t))
         (span (i (@ (class "fa-solid fa-file-lines")))
               ,(project-license project)))
    (ul (@ (class "tags"))
        ,@(map (lambda (tag)
                 `(li ,tag)) (project-tags project)))
    ,@(project-description project)))

(define (blog-template site title posts prefix)
  `((div (@ (class "blog"))
         (h1 (@ (class "title")) ,title)
         ,(post-entries site (posts/reverse-chronological posts)))))

(define (portfolio-template site title projects prefix)
  (define (project-uri project)
    (if (equal? (project-involvement project) 'author)
        (string-append (or prefix "") "/" (project-name project) ".html")
        (project-link project)))

  `((h1 (@ (class "title")) ,title)
    (div (@ (id "portfolio"))
         ,@(map (lambda (project)
                  `(a (@ (href ,(project-uri project))
                         (class "project")
                         ,@(if (equal? (project-involvement project)
                                       'author)
                               '()
                               '((rel noopener)
                                 (target _blank))))
                      (h1 ,(project-name project))
                      (h4 (@ (class "text-italic text-faint"))
                          ,(project-involvement project))
                      (ul (@ (class "tags"))
                          ,@(map (lambda (tag)
                                   `(li ,tag)) (project-tags project)))
                      (p ,(project-synopsis project))))
                projects))))

(define %blog-collections
  `(("Blog" "index.html" ,posts/reverse-chronological)))

(define %portfolio-collections
  `(("Projects" "index.html")))

(define %blog-theme
  (theme #:name %username
         #:layout base-layout
         #:post-template post-template
         #:collection-template blog-template))

(define %portfolio-theme
  (portfolio-theme #:name %username
                   #:layout base-layout
                   #:project-template project-template
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

(site #:title %fullname
      #:domain %domain
      #:default-metadata
      `((author . ,%fullname)
        (email . ,%email))
      #:readers (list html-reader)
      #:builders (list index-page
                       (portfolio #:prefix "/projects"
                                  #:theme %portfolio-theme
                                  #:projects %projects
                                  #:filter projects/filter-authored
                                  #:collections %portfolio-collections)
                       (blog #:prefix "/posts"
                             #:theme %blog-theme
                             #:collections %blog-collections)
                       contact-page
                       not-found-page
                       (atom-feed)
                       (atom-feeds-by-tag)
                       (static-directory "assets")))
