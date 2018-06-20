;; (require 'ob-R)
;; (require 'ob-sh)
;; (require 'ob-python)
;; (require 'ob-latex)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (R . t)
   (sh . t)
   (latex . t)
   (python . t)))
