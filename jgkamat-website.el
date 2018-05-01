;;; jgkamat-website.el --- generation scripts for jgkamat's website

;; Copyright (C) 2016-present Jay Kamat
;; Author: Jay Kamat <jaygkamat@gmail.com>
;; Version: 1.0
;; Keywords: jgkamat
;; URL: https://github.com/jgkamat/jgkamat.github.io
;; Package-Requires: ((emacs "25.0") (htmlize))

;;; Commentary:
;;Creates and builds org project files for my website

;; To run: cask eval "(progn (require 'jgkamat-website) (jgkamat-publish))" in the root of the project.


;;; Code:


(require 'ox-publish)
(require 'ox-rss)
(require 'cl-lib)

(defvar website-publish-dir (concat (file-name-directory load-file-name) "./html"))

;; Force htmlize to activate even in nogui mode:
;; https://stackoverflow.com/questions/3591337/emacs-htmlize-in-batch-mode
;; http://sebastien.kirche.free.fr/emacs_stuff/elisp/my-htmlize.el
;; Get fancy colors! (but this will screw up your native emacs install)
(when noninteractive
  ;; Don't run in interactive mode to avoid breaking your colors
  (custom-set-faces
   ;; Draculized minimal: https://github.com/jgkamat/darculized
   ;; TODO find out why default face is not applying.
   '(default                      ((t (:foreground "#909396" :background "#262626"))))
   '(font-lock-builtin-face       ((t (:foreground "#598249"))))
   '(font-lock-comment-face       ((t (:foreground "#5e6263"))))
   '(font-lock-constant-face      ((t (:foreground "#15968D"))))
   '(font-lock-function-name-face ((t (:foreground "#2F7BDE"))))
   '(font-lock-keyword-face       ((t (:foreground "#598249"))))
   '(font-lock-string-face        ((t (:foreground "#15968D"))))
   '(font-lock-type-face		       ((t (:foreground "#598249"))))
   '(font-lock-variable-name-face ((t (:foreground "#2F7BDE"))))
   '(font-lock-warning-face       ((t (:foreground "#bd3832" :weight bold)))))
  (setq htmlize-use-rgb-map 'force)
  (require 'htmlize))


;; I reccomend you publish with FORCE on to avoid loosing changes in history files
(let ((proj-base (file-name-directory load-file-name)))
  (setq project-base (concat proj-base "src/"))
  (setq
   org-publish-project-alist
   `(("jgkamat.github.io"
      :base-directory ,project-base
      :recursive t
      :publishing-directory ,website-publish-dir
      ;; Add my CSS and fonts
      :html-head "<link rel=\"stylesheet\" href=\"https://jgkamat.github.io/src/jgkamat.css\">"
      :html-head-include-default-style nil
      :title nil
      :with-headline-numbers nil
      :with-date nil
      :time-stamp-file nil
      :auto-sitemap t
      :sitemap-sort-folders first
      :exclude "blog/rss.org"
      :sitemap-sort-files anti-chronologically
      :publishing-function org-html-publish-to-html)
     ("jgkamat.github.io-blogmap"
      :base-directory ,(concat project-base "./blog")
      :recursive t
      :publishing-directory ,(concat website-publish-dir "./blog")
      :title nil
      :table-of-contents nil
      :auto-sitemap t
      :exclude "home.org"
      :sitemap-filename "rss.org"
      :sitemap-title "Blog Rss"
      :sitemap-function jay-publish-sitemap-expand
      :sitemap-sort-folders first
      :sitemap-sort-files anti-chronologically
      :publishing-function ignore)
     ("jgkamat.github.io-rss"
      :base-directory ,(concat project-base "./blog")
      :recursive t
      :publishing-directory ,(concat website-publish-dir "/blog/")
      :title nil
      :with-headline-numbers nil
      :with-date nil
      :time-stamp-file nil
      :section-numbers nil
      :table-of-contents nil
      :author "Jay Kamat"
      :html-link-home "https://jgkamat.github.io/blog/"
      :rss-image-url "https://jgkamat.github.io/favicon.ico"
      :exclude ".*"
      :include ("./rss.org")
      :rss-extension "xml"
      :publishing-function (org-rss-publish-to-rss)))))

(setq org-export-date-timestamp-format "%Y-%m-%d"
      org-html-metadata-timestamp-format org-export-date-timestamp-format
      org-html-postamble t
      org-html-postamble-format
      '(("en" "
<div>
<p class=\"author\">Author: <a href=\"https://github.com/jgkamat/jgkamat.github.io\">%a</a></p>
<p class=\"date\">Published: %d</p>
<p class=\"creator\">%c</p>
</div>")))

(setq blog-homepage (concat project-base "/blog/home.org"))
(setq overview-exclude-files '("rss.org"))


;; Blog generators
(defun org-timestamp-to-str (stamp)
  "Returns string value if org timestamp. Else, return stamp."
  (if (and (not (stringp stamp)) (eql (first stamp) 'timestamp))
      (plist-get (second stamp) ':raw-value)
    stamp))

(defun gen-raw-org-date (filename)
  "Generates raw date headers from filenames"
  (let ((filename (file-relative-name (file-truename filename))))
    (with-temp-buffer
      (insert-file-contents filename)
      (plist-get (org-export-get-environment) ':date))))

(defun gen-org-property (filename)
  "Generates an org property from a filename"
  (let ((filename (file-relative-name (file-truename filename))))
    (with-temp-buffer
      (insert-file-contents filename)
      ;; TODO use a plist here instead of hacky ordering
      `(,filename ,(plist-get (org-export-get-environment) ':date) ,(plist-get (org-export-get-environment) ':title)))))

(defun gen-links-properties (&optional directory)
  "Gens a sorted (by date) (filename . properties) from an org directory"
  (let* ((directory (or directory (concat project-base "/blog")))
         (files (directory-files-recursively directory "^.*\.org$")))
    (sort
     ;; Map environments to (filename . property titles)
     (mapcar #'gen-org-property
             (seq-remove
              (lambda (test-file)
                (cl-find-if (lambda (exclude-file)
                              (file-equal-p test-file exclude-file))
                            overview-exclude-files))
              files))
     (lambda (one two)
       (let ((x (org-timestamp-to-str (first (second one))))
             (y (org-timestamp-to-str (first (second two)))))
         ;; (print (concat x " " y))
         (when (and (not (or (eql x nil) (eql y nil)))
                    (or (eql 0 (org-2ft x)) (eql 0 (org-2ft y))))
           (error (concat "Org parsing error found: "
                          x ":" y)))
         (org-time< x y))))))

(defun org-property-to-link (x &optional pre post)
  "Turns a property genrated by gen-org-properties into an org link"
  (let ((pre (or pre "")) (post (or post "")))
    (format "[[file:%s][%s%s%s]]\n" (first x) pre (first (first (last x))) post)))
;; The directory you pass in must be the relative directory to work from (and must be relative to this file)
(defun gen-links (&optional directory)
  "Generates a list of links from a directory"
  (interactive)
  ;; Reduce everything into a string
  (reduce #'concat
          ;; Get desired sorting order
          (reverse
           ;; Map properties to strings
           (mapcar (lambda (x)
                     (format "1. %s" (org-property-to-link x)))
                   (gen-links-properties directory)))))


(defun gen-prev-next (&optional directory)
  (interactive)
  (unless (cl-find-if (lambda (exclude-file)
                        (file-equal-p buffer-file-name exclude-file))
                      overview-exclude-files)
    (let* ((current-property (gen-org-property buffer-file-name))
           (properties (gen-links-properties directory))
           (index (position current-property properties :test #'equal)))
      (when (eql index nil)
        (error "This org file was not part of this project"))
      (let* ((next (elt properties (min (1- (length properties)) (+ index 1))))
             (prev (elt properties (max 0 (- index 1)))))
        (concat
         "#+begin_div-wrap\n"
         "#+begin_div-left\n"
         (org-property-to-link prev "\\leftarrow ")
         "#+end_div-left\n"
         "#+begin_div-center\n"
         (format "[[file:%s][⟨Blog Home⟩]]\n" (file-relative-name blog-homepage))
         "#+end_div-center\n"
         "#+begin_div-right\n"
         (org-property-to-link next nil " \\rightarrow")
         "\n#+end_div-right"
         "\n#+end_div-wrap")))))


;; Don't prompt for build stuff
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("emacs-lisp" "python" "sh" "dot"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq org-cycle-include-plain-lists 'integrate ;; Cycle through plain lists
      org-pretty-entities t
      org-src-fontify-natively t)

(defun jgkamat-publish ()
  "Export this website.  Assumes this file has set up the projects already."
  (let ((make-backup-files nil))
    (org-publish-all t)))

(defun jay-publish-sitemap-expand (title list)
  "Site map, as a string.
TITLE is the the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (with-temp-buffer
    (org-mode)
    (insert (org-list-to-subtree list))
    (org-map-entries
     (lambda ()
       (let* (
              ;; TODO stop hard-coding blog dir here.
              (default-directory (concat project-base "./blog/"))
              (line (thing-at-point 'line t))
              (components (org-heading-components))
              (title (cl-fifth components))
              (link-target-raw (and (string-match org-bracket-link-regexp title)
                                    (match-string 1 title)))
              (link-target (car-safe (last (split-string link-target-raw ":"))))
              (rss-permalink (concat (file-name-sans-extension link-target) ".html")))

         (end-of-line)
         (insert (concat "\n"
                         "#+INCLUDE: \"" link-target) "\" :only-contents t :lines \"4-\"")
         (org-set-property "PUBDATE" (org-timestamp-to-str (first (gen-raw-org-date link-target))))
         (org-set-property "RSS_PERMALINK" rss-permalink))))
    (buffer-string)))

(provide 'jgkamat-website)

;;; jgkamat-website.el ends here
