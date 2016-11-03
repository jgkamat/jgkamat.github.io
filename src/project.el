
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
        :publishing-function org-twbs-publish-to-html))))
(setq orgtwbs-postamble 't)
(setq org-twbs-postamble-format
  '(("en" "
<div>
<p class=\"author\">Author: %a</p>
<p class=\"date\">Updated: %d</p>
<p class=\"creator\">%c</p>
</div>")))

;; Blog generators
;; The directory you pass in must be the relative directory to work from (and must be relative to this file)
(defun gen-links (&optional directory)
  (interactive)
  (let* ((directory (or directory (concat project-base "/blog")))
         (files (directory-files directory nil ".*\.org")))
    ;; Reduce everything into a string
    (reduce #'concat
    ;; Map properties to strings
    (mapcar '(lambda (x)
               (format "[[file:%s][%s]]\n\n" (first x) (first (first (last x)))))
      (sort
        ;; Map environments to (filename . property titles)
        (mapcar '(lambda (x)
                   (with-temp-buffer
                     (let ((filename (concat directory "/" x)))
                       (insert-file-contents filename)
                       `(,x ,(plist-get (org-export-get-environment) ':date) ,(plist-get (org-export-get-environment) ':title)))))
          files)
        '(lambda (one two)
           (let ((x (date-to-time (first one)))
                  (y (date-to-time(first two))))
             (calendar-date-compare x y))))))))





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
