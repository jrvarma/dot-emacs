(provide 'jrv-redact-settings)

;; Used to print redacted version of jrv-settings.el
;; Variable values and initial values are redacted
;; Variable name, type and docstring are preserved

(defun jrv/redact/create(out-file-name)
  (interactive)
  (find-file out-file-name)
  (erase-buffer)
  (insert
   ";;; Redacted version of jrv-settings.el\n"
   ";;; Variable values are masked with *******\n\n"
   (mapconcat
    'concat
    (mapcar
     #'(lambda(v)
         (format 
          "(%s %s %s\n  \"%s\"\n  :type '%s\n  :group 'JRVarma)\n"
          "defcustom"
          v "*******"
          (get v 'variable-documentation)
          (get v 'custom-type)))
     (apropos-internal "^jrv/settings/" 'custom-variable-p))
    "\n"))
  (save-buffer)
  (kill-buffer))

(jrv/redact/create
 "~/.emacs.d/my-lisp/jrv-settings-redacted.el")
