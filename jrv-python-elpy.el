;;; Python mode
(provide 'jrv-python-elpy)
;; emacs packages desired are
;;       elpy, flycheck
;; related emacs packages (not used) py-autopep8, ein
;; python pip packages desired are
;;       rope_py3k, jedi, flake8, importmagic, autopep8, yapf
(defvar python-shell-interpreter-args)

(declare-function elpy-enable  "elpy")
(declare-function elpy-use-ipython  "elpy")
(declare-function dired-do-async-shell-command  "dired-aux")
(declare-function flycheck-mode  "flycheck.el")

(elpy-enable)
(elpy-use-ipython)
(setq python-shell-interpreter-args "--TerminalInteractiveShell.simple_prompt=True")

(defvar elpy-rpc-backend)
(setq elpy-rpc-backend "jedi")

(defvar elpy-modules)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'my-flycheck))

(defun my-flycheck()
  ;; flycheck-mode creates problems in pweave documents
  (unless (string-match (file-name-extension buffer-file-name) "Plw$")
      (flycheck-mode)))

;; Restore windmove (meta-arrow key) bindings in elpy mode
(defvar elpy-mode-map)
(defun restore-windmove-keys()
  "Restore windmove (meta-arrow key) bindings in elpy mode"
  (define-key elpy-mode-map [(meta left)] nil)
  (define-key elpy-mode-map [(meta right)] nil)
  (define-key elpy-mode-map [(meta up)] nil)
  (define-key elpy-mode-map [(meta down)] nil))

(eval-after-load 'elpy '(restore-windmove-keys))

;;; Load offline python html documentation in browser
(defvar offline-python-help-file
  "/path/to/python-doc-dir/index.html")
(defun python-help ()
  "Launch python help in browser or windows help file"
  (interactive)
    (let ((process-connection-type nil)) 
      (dired-do-async-shell-command  "xdg-open" nil 
                                     (list offline-python-help-file))))

(add-hook
 'python-mode-hook
 '(lambda ()
    (when (file-exists-p offline-python-help-file)
      (define-key python-mode-map [(control ?c) (control ?h)] 'python-help))
    (when (require 'jrv-pydoc nil my-minimal) 
      (define-key python-mode-map [(control ?c) (d)] 'python-insert-docstring))
))

;; This creates problems in pweave documents
;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
