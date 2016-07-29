
(require 'ox-publish)

(setq org-publish-project-alist
  '(("jgkamat.github.io"
      :base-directory "."
      :publishing-directory "../"
      :publishing-function org-twbs-publish-to-html)))

;; (setq org-twbs-postamble 'auto)
;; (setq org-twbs-postamble-format
;;   '(("en" " <p class=\"author\">Author: %a (%e)</p>
;; <p class=\"author\">Author: %a (%e)</p>
;; <p class=\"date\">Date: %d</p>
;; <p class=\"creator\">%c</p>")))


(require 'request)
(defun gh-stars (repo-string)
  (if (string= "-" repo-string)
    "-"
    (progn (require 'request)
      (defvar result nil)
      (request (concat  "https://api.github.com/repos/" repo-string "/stargazers?per_page=1")
        :parser 'json-read
        :sync t
        :success
        (cl-function
          (lambda (&key response &allow-other-keys)
            (setq result response))))
      result)))

(defun parse-gh-str (url-string)
  "Parses a github url and returns a nice representation like: jgkamat/alda-mode"
  (let* ((var (split-string url-string "/"))
          (len (length var)))
    (when (string= (elt (last var) 0) "")
      (setq len (- len 1)))
    (concat (elt var (- len 2)) "/" (elt var (- len 1)))))

(defun parse-org-link (org-link)
  "takes in an org link and returns a string link"
  (symbol-name (elt (elt org-link 0) 0)))

(defun org-link-to-str (link)
  "Turns org links to jgkamat/alda-mode format"
  (let ((str-link (parse-org-link link)))
    (if (string-match "github" str-link)
      (parse-gh-str str-link)
      "-")))
