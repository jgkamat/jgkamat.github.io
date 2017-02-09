
;; automate building the website.
;; This will install everything needed to build my website, so it's meant to be run in a completely clean eamcs

;; To run:
;; emacs -nw -q --script buildproject.el
;; REQUIRES EMACS 25.1 OR LATER

(package-initialize)

(require 'package)
(require 'org)
(require 'cl)
(setq package-archives `(("gnu" . "http://elpa.gnu.org/packages/")
                          ("melpa" . "http://melpa.org/packages/")))

(package-refresh-contents)
(package-install 'ox-twbs)
(package-install 'htmlize)

;; Must have html dir in the root
(setq website-publish-dir (concat (file-name-directory load-file-name) "../html"))

;; Load the current directory
(print (concat (file-name-directory load-file-name) "project.el"))
(load-file (concat (file-name-directory load-file-name) "project.el"))

;; Don't prompt for build stuff
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("emacs-lisp" "python"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; Export all projects. Don't load any projects except the one we want or you might be rekt
(let ((project-list (mapcar #'first org-publish-project-alist)))
  (dolist (x project-list)
    (org-publish-project x t)))
