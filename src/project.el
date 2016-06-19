
(require 'ox-publish)

(setq org-publish-project-alist
  '(("jgkamat.github.io"
      :base-directory "."
      :publishing-directory "../"
      :publishing-function org-twbs-publish-to-html
      :html-head "
<link  href=\"https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.1/css/bootstrap.min.css\" rel=\"stylesheet\">
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/jquery/1.11.2/jquery.min.js\"></script>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.1/js/bootstrap.min.js\"></script>
")))
