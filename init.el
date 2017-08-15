;;; J R Varma's init.el file 
;; This is a stub file that simply loads the main init file
;; Of course, emacs itself adds customization variables here

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;; Add ~/.emacs.d/site-lisp/ and its subdirectories to load path
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Keep all the init files in compiled form for faster load
;; All init files are in a separate folder "~/.emacs.d/site-lisp/jrv"
(byte-recompile-directory (expand-file-name "~/.emacs.d/site-lisp/jrv") 0)

;; Load main init files
(require 'jrv-init)             ; Main init file loads all other files

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (esup counsel bind-key web-mode web-mode-edit-element lua-mode w3m tool-bar+ py-autopep8 polymode outline-magic markdown-mode+ magit latex-pretty-symbols gnus-alias flycheck ess-smart-underscore ess-smart-equals ess-R-object-popup ess-R-data-view escreen elpy dired-sort-menu+ dired+ company-quickhelp company-auctex auto-complete-c-headers apel))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
