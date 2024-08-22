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
;; Components/Utilities
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

(define (bulleted-list entries)
  `(ul (@ (class "list"))
       ,@(map (lambda (i)
                `(li (@ (class "list-item--type-bulleted")) ,i))
              entries)))

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


(define (post-uri site post)
  (string-append "/posts/" (site-post-slug site post) ".html"))


;;
;; Defaults
;;

(define %domain "migalmoreno.com")
(define %email "mail@migalmoreno.com")
(define %username "migalmoreno")
(define %fullname "Miguel Ángel Moreno")

(define (project-uri name)
  (format #f "https://git.~a/~a" %domain name))

(define %projects
  (list
   (project
    #:name "tubo"
    #:synopsis "A libre streaming front-end for the web"
    #:link (project-uri "tubo")
    #:tags '("clojure" "clojurescript")
    #:license "AGPL-3.0"
    #:description
    `((p "Tubo is a web front-end to many streaming platforms (YouTube,
 SoundCloud, Bandcamp, etc.).")
      (p "It acts like a privacy-friendly middleman that gathers the content
from these sites and displays it to you in a distraction-free interface along
with features that are usually locked behind premium subscriptions. Its ultimate
goal is to offer a full-fledged web-based alternative to "
         ,(anchor "Newpipe" "https://newpipe.net") ".")))
   (project
    #:name "nx-router"
    #:synopsis "A declarative URL routing extension for Nyxt"
    #:link (project-uri "nx-router")
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
        "(list
  (make-instance 'router:redirector
    :name 'fandom
    :route \"https://([\\w'-]+)\\.fandom.com/wiki/(.*)\"
    :redirect \"https://breezewiki.com/\\1/wiki/\\2\")
  (make-instance 'router:blocker
    :name 'fandom
    :route (match-domain \"fandom.com\")
    :instances-builder router:breezewiki-instances-builder
    :blocklist \".*/search\")
  (make-instance 'router:opener
    :name 'fandom
    :resource \"mpv --video=no ~a\"))"
        #:lang 'lisp)
      (p "A particular feature of the redirector router is support for reverse redirects
so that certain URLs get recorded with their original path, which is useful if
you don't want to record alternative frontends URLs and potentially deal with
unmaintained instances in the future.")
      ,(highlight-code
        "(make-instance 'router:redirector
  :route (match-regex \"https://.*google.com/search.*\")
  :redirect (quri:uri \"http://localhost:5000\")
  :reverse (quri:uri \"https://www.google.com\"))"
        #:lang 'lisp)))
   (project
    #:name "nx-tailor"
    #:synopsis "A theme manager for Nyxt"
    #:link (project-uri "nx-tailor")
    #:tags '("common-lisp" "browser")
    #:license  "BSD 3-Clause"
    #:description
    `((p "nx-tailor is a theme manager for the "
         ,(anchor "Nyxt" "https://nyxt.atlas.engineer") " browser. It leverages
the built-in " (code "nyxt/theme") " library to allow defining multiple themes
to switch between at browser runtime.")
      (p "I developed this extension because to this day there's no easy way
to preview Nyxt themes without restarting the browser and I wanted to bring the
same REPL-based interactivity to theme development.")
      (p "Since then, the feature set has grown and it now also supports user-specified
criteria for theme selection at launch and a timer functionality to
automatically switch it based on the time of the day.")))
   (project
    #:name "nx-mosaic"
    #:synopsis "A configurable new-buffer page for Nyxt"
    #:link (project-uri "nx-mosaic")
    #:tags '("common-lisp" "browser")
    #:license  "BSD 3-Clause"
    #:description
    `((p "nx-mosaic is a highly customizable new-buffer page (startpage) for
the " ,(anchor "Nyxt" "https://nyxt.atlas.engineer") " browser. It's inspired
by the " ,(anchor "Tabliss" "https://tabliss.io") " web extension, and aims to
create a Nyxt-native startpage that you can control to your heart's content
via a wide array of widgets.")))
   (project
    #:name "fdroid.el"
    #:synopsis "An Emacs interface to the F-Droid package repository"
    #:link (project-uri "fdroid.el")
    #:tags '("emacs-lisp" "fdroid")
    #:license "GPL-3.0+"
    #:description
    `((p "fdroid.el is a completion-based interface to work with
F-Droid packages from Emacs.")
      (p "Having to deal with Android emulators quite often and needing to
install packages on initial setup, I developed this library to be able to
quickly manage F-Droid packages without having to resort to the F-Droid website
or having to download APKs manually.")))
   (project
    #:name "nyxt.el"
    #:synopsis "A minimal API to interact with Nyxt from Emacs"
    #:link (project-uri "nyxt.el")
    #:tags '("emacs-lisp" "nyxt")
    #:license "GPL-3.0+"
    #:description
    `((p "nyxt.el is a minimal API to interact with Nyxt from Emacs. I developed this
library to make it easy for others to build their own Nyxt<->Emacs functionality.")
      (p "For this, it contains a useful helper " (code "nyxt-run") " which launches
or connects to an existing Nyxt process with a specified Nyxt command.")
      (p "If you're interested in getting data from Nyxt without sending any commands
through, you can just use the " (code "nyxt--sly-eval") " function.")))))


;;
;; Layouts
;;

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
           (footer (@ (class "footer"))
                   (div (@ (class "footer__wrapper"))
                        ,(format #f "© 2024 ~a" %fullname))))))))


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
                    (i (@ (class "fa-brands fa-git-alt project__icon")))
                    ,(anchor (project-link project)
                             (string-append (project-link project) "/about")
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
         (div (@ (class "blog__title"))
          (h1 (@ (class "main__title")) ,title)
          ,(anchor '(i (@ (class "fa-solid fa-rss"))) "/feed.xml"))
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
                               ,(project-name project)))
                           (div
                            ,(anchor '(i (@ (class "fa-brands fa-git-alt")))
                                     (string-append (project-link project)
                                                    "/about")
                                     #:external? #t)))
                      (ul (@ (class "tags"))
                          ,@(map (lambda (tag)
                                   `(li (@ (class "tag")) ,tag))
                                 (project-tags project)))
                      (p (@ (class "project-item__synopsis"))
                         ,(project-synopsis project))))
                projects))))

(define %blog-collections
  `(("Blog" "posts/index.html" ,posts/reverse-chronological)))

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
          (p "Software developer with a strong interest in FOSS, functional programming,
reproducible systems, and compilers."))
         (div (@ (class "blog blog--type-preview"))
              (h2 (@ (class "blog__title")) "Latest Posts"
                  ,(anchor '(button (@ (class "button button--type-border"))
                                    "See all") "/posts"))
              ,(post-entries site posts))))
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
           '(span (i (@ (class "fa-solid fa-envelope footer__icon")))
                  "Email")
           '(span (code "mail") " at " (code "$DOMAIN")))
         ,(contact-entry
           '(span (i (@ (class "fa-solid fa-key footer__icon")))
                  "PGP")
           (anchor '(code "4956 DAC8 B077 15EA 9F14  E13A EF1F 69BF 5F23 F458")
                   (format #f "https://files.~a/pubkey.asc" %domain)
                   #:external? #t))))))

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
