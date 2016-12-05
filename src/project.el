
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
(defun org-timestamp-to-str (stamp)
  "Returns string value if org timestamp. Else, return stamp."
  (if (and (not (stringp stamp)) (eql (first stamp) 'timestamp))
    (plist-get (second stamp) ':raw-value)
    stamp))

(defun gen-org-property (filename)
  "Generates an org property from a filename"
    (with-temp-buffer
      (insert-file-contents filename)
      ;; TODO use a plist here instead of hacky ordering
      `(,filename ,(plist-get (org-export-get-environment) ':date) ,(plist-get (org-export-get-environment) ':title))))

(defun gen-links-properties (&optional directory)
  "Gens a sorted (by date) (filename . properties) from an org directory"
  (let* ((directory (or directory (concat project-base "/blog")))
          (files (directory-files directory nil ".*\.org"))
          (default-directory directory))
    (sort
      ;; Map environments to (filename . property titles)
      (mapcar #'gen-org-property files)
      '(lambda (one two)
         (let ((x (org-timestamp-to-str (first (second one))))
                (y (org-timestamp-to-str (first (second two)))))
           (when (or (eql 0 (org-2ft x)) (eql 0 (org-2ft y)))
             (error (concat "Org parsing error found: "
                      x " " y)))
           (org-time< x y))))))

(defun org-property-to-link (x)
  "Turns a property genrated by gen-org-properties into an org link"
  (format "[[file:%s][%s]]\n\n" (first x) (first (first (last x)))))
;; The directory you pass in must be the relative directory to work from (and must be relative to this file)
(defun gen-links (&optional directory)
  "Generates a list of links from a directory"
  (interactive)
  ;; Reduce everything into a string
  (reduce #'concat
    ;; Map properties to strings
    (mapcar '(lambda (x)
               (format "1. %s" (org-property-to-link x)))
      (gen-links-properties directory))))


(defun gen-prev-next (&optional directory)
  (interactive)
  (let* ((current-property (gen-org-property buffer-file-name))
         (properties (gen-links-properties directory))
         (index (position properties current-property)))
    (when (eql index nil)
      (error "This org file was not part of this project"))
    ;; TODO get next nad prev link
    ))








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
