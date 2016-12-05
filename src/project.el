
(require 'ox-publish)

(let ((proj-base (file-name-directory load-file-name)))
  (setq project-base proj-base)
  (setq org-publish-project-alist
    `(("jgkamat.github.io"
        :base-directory ,(concat proj-base ".")
        :recursive t
        :publishing-directory ,(concat proj-base  "../")
        :html-head-extra "<link rel=\"stylesheet\" type=\"text/css\" href=\"/src/jgkamat.css\"> <link href=\"https://fonts.googleapis.com/css?family=Open+Sans\" rel=\"stylesheet\">"
        :title nil
        :with-headline-numbers nil
        :toc 3
        :with-date nil
        :time-stamp-file nil
        :publishing-function org-twbs-publish-to-html))))
(setq org-twbs-postamble 't)
(setq org-twbs-postamble-format
  '(("en" "
<div>
<p class=\"author\">Author: %a</p>
<p class=\"date\">Published: %d</p>
<p class=\"creator\">%c</p>
</div>")))
;; Export format of DATE:
(setq org-export-date-timestamp-format "%Y-%m-%d")

(setq blog-homepage (concat project-base "/blog/home.org"))


;; Blog generators
(defun org-timestamp-to-str (stamp)
  "Returns string value if org timestamp. Else, return stamp."
  (if (and (not (stringp stamp)) (eql (first stamp) 'timestamp))
    (plist-get (second stamp) ':raw-value)
    stamp))

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
          (files (directory-files-recursively directory ".*\.org")))
    (sort
      ;; Map environments to (filename . property titles)
      (mapcar #'gen-org-property files)
      '(lambda (one two)
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
      (mapcar '(lambda (x)
                 (format "1. %s" (org-property-to-link x)))
        (gen-links-properties directory)))))


(defun gen-prev-next (&optional directory)
  (interactive)
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
        (format "[[file:%s][\\langle{}Blog Home\\rangle]]\n" (file-relative-name blog-homepage))
        "#+end_div-center\n"
        "#+begin_div-right\n"
        (org-property-to-link next nil " \\rightarrow")
        "\n#+end_div-right"
        "\n#+end_div-wrap"))))

;; (require 'request)
;; (defun gh-stars (repo-string)
;;   (if (string= "-" repo-string)
;;     "-"
;;     (progn (require 'request)
;;       (defvar result nil)
;;       (request (concat  "https://api.github.com/repos/" repo-string "/stargazers?per_page=1")
;;         :parser 'json-read
;;         :sync t
;;         :success
;;         (cl-function
;;           (lambda (&key response &allow-other-keys)
;;             (setq result response))))
;;       result)))

;; (defun parse-gh-str (url-string)
;;   "Parses a github url and returns a nice representation like: jgkamat/alda-mode"
;;   (let* ((var (split-string url-string "/"))
;;           (len (length var)))
;;     (when (string= (elt (last var) 0) "")
;;       (setq len (- len 1)))
;;     (concat (elt var (- len 2)) "/" (elt var (- len 1)))))

;; (defun parse-org-link (org-link)
;;   "takes in an org link and returns a string link"
;;   (symbol-name (elt (elt org-link 0) 0)))

;; (defun org-link-to-str (link)
;;   "Turns org links to jgkamat/alda-mode format"
;;   (let ((str-link (parse-org-link link)))
;;     (if (string-match "github" str-link)
;;       (parse-gh-str str-link)
;;       "-")))
