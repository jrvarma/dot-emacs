;;; J R Varma's init.el file 
;;; This is a stub file that simply loads the actual init files

;;; Add ~/.emacs.d/site-lisp/ and its subdirectories to load path
;; Various function definitions used here are in separate files in site-lisp

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;;; Keep all the init files in compiled form for faster load
(byte-recompile-directory (expand-file-name "~/.emacs.d/site-lisp/jrv"))

(require 'jrv-init)             ; Main init file

