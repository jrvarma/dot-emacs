(provide 'jrv-gpg-only-in-phone.el)
(setq epg-gpg-program "/usr/bin/gpg2")

(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
        str))
