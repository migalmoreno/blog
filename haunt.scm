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
             (srfi srfi-1)
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
        `(pre (@ (class "codeblock"))
              (code (@ (class "codeblock__code"))
                    ,(highlights->sxml (highlight lexer code))))
        code)))

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

(define (post-uri site post)
  (string-append "/posts/" (site-post-slug site post) ".html"))


;;
;; Defaults
;;

(define %domain "migalmoreno.com")
(define %email "mail@migalmoreno.com")
(define %username "migalmoreno")
(define %fullname "Miguel Ángel Moreno")
(define %projects
  (list
   (project
    #:name "tubo"
    #:synopsis "A libre streaming front-end for the web"
    #:link (format #f "https://github.com/~a/tubo" %username)
    #:tags '("clojure" "clojurescript")
    #:license "GPL-3.0+"
    #:description
    `((p "Tubo is a libre web front-end for some of the most popular
streaming sites, including YouTube, SoundCloud, Bandcamp, among other.")
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
      (p "Currently, it features search and watch functionality, audio-only
playback, media queuing, bookmarks, and settings. Its roadmap includes feeds,
downloads, offline mode, and more.")
      (figure
       (img (@ (src
                ,(format #f "https://files.~a/tubo_channel_dark_media_queue.jpg"
                         %domain))
               (style "width:100%")
               (alt "Tubo channel page screenshot")
               (class "post__image")))
       (figcaption "Channel page view"))))
   (project
    #:name "nx-router"
    #:synopsis "A declarative URL routing extension for Nyxt"
    #:link (format #f "https://github.com/~a/nx-router" %username)
    #:tags '("common-lisp" "browser")
    #:license "BSD 3-Clause"
    #:description
    `((p "nx-router is a declarative URL routing extension for the "
         ,(anchor "Nyxt" "https://nyxt.atlas.engineer") " browser.")
      (p "It offers a convenient wrapper around the built-in request resource
handling functionality in Nyxt by introducing" (code "router") " objects users
can include in their configuration to define redirects, blocklists, and resource
openers with an easy and declarative syntax.")
      (p "It aims to be simple while staying composable and flexible.
Routers are defined like this:")
      ,(highlight-code
        "(make-instance 'router:redirector
  :route \"https://([\\w'-]+)\\.fandom.com/wiki/(.*)\"
  :redirect \"https://breezewiki.com/\\1/wiki/\\2\")"
        #:lang 'lisp)
      (p "You can also set reverse redirects so that certain URLs get recorded
with their original path, which is useful if you don't want to have to deal
with unmaintained instances of privacy frontends.")
      ,(highlight-code
        "(make-instance 'router:redirector
  :route (match-regex \"https://.*google.com/search.*\")
  :redirect (quri:uri \"http://localhost:5000\")
  :reverse (quri:uri \"https://www.google.com\"))"
        #:lang 'lisp)))
   (project
    #:name "nx-tailor"
    #:synopsis "A theme manager for Nyxt"
    #:link (format #f "https://github.com/~a/nx-tailor" %username)
    #:tags '("common-lisp" "browser")
    #:license  "BSD 3-Clause"
    #:description
    `((p "nx-tailor is a theme manager for the "
         ,(anchor "Nyxt" "https://nyxt.atlas.engineer") " browser. It leverages
the built-in " (code "nyxt/theme") " library to allow defining multiple themes
to switch between at browser runtime.")
      (p "It also has a timer functionality to automatically change them
depending on the time of the day.")
      (figure
       (video (@ (src ,(format #f "https://files.~a/nx_tailor.mp4" %domain))
                 (style "width:100%")
                 (autoplay "true")
                 (controls "true")
                 (class "post__image")))
       (figcaption "Changing the theme via the prompt buffer"))))
   (project
    #:name "fdroid.el"
    #:synopsis "An Emacs interface to the F-Droid package repository"
    #:link (format #f "https://github.com/~a/fdroid.el" %username)
    #:tags '("emacs-lisp" "fdroid")
    #:license "GPL-3.0+"
    #:description
    `((p "fdroid.el is a completion-based interface to work with
F-Droid packages from Emacs.")
      (p "Having to deal with Android emulators quite often and needing to
install packages on initial setup, I developed this project to be able to
quickly manage F-Droid packages without having to resort to the F-Droid website
or having to download APKs manually.")))
   (project
    #:name "nyxt.el"
    #:synopsis "A minimal API to interact with Nyxt from Emacs"
    #:link (format #f "https://github.com/~a/nyxt.el" %username)
    #:tags '("emacs-lisp" "nyxt")
    #:license "GPL-3.0+"
    #:description
    `((p "nyxt.el is a minimal API to interact with Nyxt from Emacs.")
      (p "It contains a useful helper " (code "nyxt-run") " to run a Nyxt
process seamlessly from Emacs and provides optional integration with the
Emacs X Window Manager (EXWM).")))
   (project
    #:name "guix-config"
    #:synopsis "Personal Guix configuration"
    #:link (format #f "https://github.com/~a/guix-config" %username)
    #:tags '("scheme" "rde" "dotfiles")
    #:license "GPL-3.0+"
    #:description
    `((p "My personal set of configuration files built on top of Guix and
RDE. The project is focused on providing a central point to all my systems, from
a home/system configuration in my local development machine to a self-hosted
setup in my personal VPS.")
      (figure
       (img (@ (src ,(format #f "https://files.~a/guix_config_setup.jpg"
                             %domain))
               (style "width:100%")
               (alt "Screenshot of my Guix configuration")
               (class "post__image")))
       (figcaption "Screenshot of my setup"))
      (p "Previously, I used to maintain this project as a personal Guix
channel, but over time I found this to be unsustainable, so I now contribute
packages, services, and features upstream as much as I can.")))
   (project
    #:name "blog"
    #:synopsis "Personal site and blog"
    #:link (format #f "https://github.com/~a/blog" %username)
    #:tags '("scheme" "org-mode" "haunt")
    #:license "GPL-3.0+"
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
describe my personal projects and contributions.")))))


;;
;; Components
;;

(define (post-entries site posts)
  `(ul (@ (class "blog__wrapper"))
    ,@(map (lambda (post)
             `(a (@ (class "post-item")
                    (href ,(post-uri site post)))
                 (span (@ (class "post-item__title")) ,(post-ref post 'title))
                 (span (@ (class "post-item__date"))
                       ,(date->string* (post-date post)))))
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
  `(header (@ (class "navbar"))
           (input (@ (class "navbar__mobile-menu")
                     (type "checkbox") (id "mobile-menu")))
           (div (@ (class "navbar__images"))
                (div (@ (class "navbar__logo"))
                     ,(anchor %fullname "/" #:extra-classes "navbar__link"))
                (label (@ (class "navbar__menu-icon") (for "mobile-menu"))
                       (span (@ (class "menu-icon")))))
           (nav (@ (class "navbar__nav"))
                (ul (@ (class "navbar__menu"))
                    ,@(map (lambda (a)
                             `(li (@ (class "menu-item"))
                                  ,(anchor (car a) (cdr a)
                                           #:extra-classes "menu-item__link")))
                           '(("Home" . "/")
                             ("Projects" . "/projects")
                             ("Blog" . "/posts")
                             ("Contact" . "/contact.html")))))))

(define footer
  `(footer (@ (class "footer"))
           (div (@ (class "footer__wrapper"))
                ,@(map (match-lambda
                         ((? list? e) e)
                         ((label . class)
                          (anchor `(i (@ (class ,(format #f "~a footer__icon"
                                                         class))))
                                  label
                                  #:external? #t
                                  #:extra-classes "footer__link")))
                       (list
                        (cons "/feed.xml" "fa-solid fa-rss")
                        (cons (format #f "https://linkedin.com/in/~a" %username)
                              "fa-brands fa-linkedin")
                        (anchor
                         `(i (@ (class "fa-brands fa-mastodon footer__icon")))
                         (format #f "https://fosstodon.org/@~a" %username)
                         #:external? #t
                         #:extra-classes "footer__link"
                         #:extra-attributes '((rel "me")))
                        (cons (format #f "https://github.com/~a" %username)
                              "fa-brands fa-github"))))
    (div (@ (class "footer__wrapper"))
         "© "
         (span (@ (class "footer__year")))
         ,(format #f " ~a" %fullname))))

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
      (div (@ (class "body-container"))
           (main (@ (class "main")) ,body)
           ,footer)
      ,(script "main")))))


;;
;; Templates/themes
;;

(define (post-template post)
  `((div (@ (class "post__metadata"))
         (button (@ (class "button button--type-bare"))
                 (i (@ (class "fa-solid fa-caret-left button__icon")))
                 (a (@ (href "/posts") (class "button__label")) "Back to posts"))
         (h1 (@ (class "main__title")) ,(post-ref post 'title))
         (span (@ (class "post__subtitle")) " on "
               ,(date->string* (post-date post)))
         (ul (@ (class "tags"))
             ,@(map (lambda (tag)
                      `(li (@ (class "tag"))
                           (a (@ (href
                                  ,(string-append "/feeds/tags/" tag ".xml"))
                                 (class "tag__link"))
                              ,tag)))
                    (assq-ref (post-metadata post) 'tags))))
    (div (@ (class "post__container")) ,(post-sxml post))))

(define (project-template project)
  `((div (@ (class "project"))
         (button (@ (class "button--type-bare"))
                 (i (@ (class "fa-solid fa-caret-left button__icon")))
                 (a (@ (href "/projects") (class "button__label"))
                    "Back to projects"))
         (h1 ,(project-name project))
         (h4 (@ (class "project__subtitle")) ,(project-synopsis project))
         (div (@ (class "project__metadata"))
              (span (@ (class "project__metadata-items"))
                    (i (@ (class "fa-brands fa-github project__icon")))
                    ,(anchor (project-link project) (project-link project)
                             #:external? #t
                             #:extra-classes "project__link"))
              (span (@ (classs "project__metadata-items"))
                    (i (@ (class "fa-solid fa-file-lines project__icon")))
                    ,(project-license project)))
         (ul (@ (class "tags"))
             ,@(map (lambda (tag)
                      `(li (@ (class "tag")) ,tag)) (project-tags project)))
         (div (@ (class "project__container"))
                ,@(project-description project)))))

(define (blog-template site title posts prefix)
  `((div (@ (class "blog"))
         (h1 (@ (class "main__title")) ,title)
         ,(post-entries site (posts/reverse-chronological posts)))))

(define (portfolio-template site title projects prefix)
  (define (project-uri project)
    (string-append (or prefix "") "/" (project-name project) ".html"))

  `((h1 (@ (class "portfolio__title")) ,title)
    (div (@ (class "portfolio"))
         ,@(map (lambda (project)
                  `(div (@ (class "project-item"))
                      (div (@ (class "project-item__heading"))
                           (div
                            (a (@ (class "project-item__title")
                                  (href ,(project-uri project)))
                               ,(project-name project))
                            ,(anchor '(i (@ (class "fa-brands fa-github")))
                                     (project-link project)
                                     #:extra-classes "project-item__link"
                                     #:external? #t))
                           (div
                            (@ (class "project-item__stats"))
                            (div
                             (a (@ (class "project-item__stargazers")
                                   (href ,(string-append (project-link project)
                                                         "/stargazers")))))
                            (div
                             (i (@ (class "icon__star fa-regular fa-star"))))))
                      (ul (@ (class "tags"))
                          ,@(map (lambda (tag)
                                   `(li (@ (class "tag")) ,tag))
                                 (project-tags project)))
                      (p (@ (class "project-item__synopsis"))
                         ,(project-synopsis project))))
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
       `((div
          (@ (class "hero"))
          (h1 (@ (class "hero__title"))
              ,(format #f "Hi, I'm ~a!"
                       (string-join
                        (drop-right (string-split %fullname #\space) 1))))
          (p "I'm a software developer currently based in Madrid working at "
             ,(anchor "GMV" "https://gmv.com" #:external? #t)
             " (SST & STM) and an open source software enthusiast.")
          (p "My interests currently revolve around these topics:")
          (ul (@ (class "list"))
              ,@(map (lambda (i)
                       `(li (@ (class "list-item--type-bulleted")) ,i))
                     (list "Functional programming"
                           "Web development"
                           "Operating systems and reproducibility"
                           "Compilers"
                           "Introspectable and extensible tooling"))))
         (div (@ (class "blog blog--type-preview"))
              (h2 (@ (class "blog__title")) "Latest Posts"
                  ,(anchor '(button (@ (class "button button--type-border"))
                                    "See all") "/posts"))
              ,(post-entries site posts))))
      sxml->html))))

(define contact-page
  (static-page
   "Contact"
   "/contact.html"
   `((h1 (@ (class "main__title")) "Contact me")
     (dl (@ (class "list"))
      (div (@ (class "descriptions__wrapper"))
       (dt (@ (class "descriptions__title")) "Email")
       (dd (@ (class "descriptions__text"))
           (code "mail") " at " (code "$DOMAIN")))
      (div (@ (class "descriptions__wrapper"))
       (dt (@ (class "descriptions__title")) "PGP")
       (dd (@ (class "descriptions__text"))
           ,(anchor '(code "4956 DAC8 B077 15EA 9F14  E13A EF1F 69BF 5F23 F458")
                    (format #f "https://files.~a/pubkey.asc" %domain)
                    #:external? #t)))))))

(define not-found-page
  (static-page
   "404 Not found"
   "/404.html"
   `((div (@ (class "not-found"))
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
                                  #:collections %portfolio-collections)
                       (blog #:prefix "/posts"
                             #:theme %blog-theme
                             #:collections %blog-collections)
                       contact-page
                       not-found-page
                       (atom-feed)
                       (atom-feeds-by-tag)
                       (static-directory "assets")))
