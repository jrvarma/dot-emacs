(provide 'jrv-aliases-short-keys)

;;; Aliases for common commands
;; Typically space followed by letter (a/b/c/h/l/m/s/-/;)

(defvar jrv/aliases) ;; suppress compiler warning
;; jrv/aliases is a list of elements of the form
;; ("ALIAS" . DEFINITION)
;; Note: ALIAS is a string, but DEFINITION is an unquoted lisp object
;; we run mapc below to create a defalias for each such element
(setq jrv/aliases
      '(
        (" a" . (lambda () (interactive) (ansi-term "/bin/bash")))
        (" b" . (lambda ()
                  (interactive)
                  (async-shell-command "bashmount" "*bashmount*")
                  (other-window 1)
                  ))
        (" c" . calculator)
        (" h" . hfyview-buffer)
        (" l" . locate-with-filter)
        (" m" . menu-bar-mode)
        (" s" . jrv/buffer-kill-star-buffers)
        (" -" . jrv/text-space-to-dash)
        (" ;" . jrv/dired-launch-file)
        (" g" . magit-status)
        ("&" . jrv/shell-command-on-buffer-file)
        ;; We have rebound C-x C-c to save-buffers-kill-emacs.
        ;; The original binding of C-x C-c is now aliased to 1k
        ("1k" . save-buffers-kill-terminal)
        ("2h" . jrv/shell-send-buffer-file-to-h)
        ("pr" . jrv/text-proper-case-region)
        ("wc" . jrv/text-wc)
        ("ntu" . (lambda ()
                   (interactive)
                   (notmuch-tree "tag:unread")))
        ("nh" . (lambda ()
                   (interactive)
                   (notmuch-hello)))
        ))

(mapc
 (lambda (pair)
   (defalias
     ;; intern converts a string to a symbol
     (intern (car pair))
     (cdr pair)))
 jrv/aliases)

;;; Key bindings map "ESC ESC letter" to frequent commands
;;; For example, "ESC ESC s" (same as "ESC M-s") saves buffer

(defvar jrv/frequent-keys) ;; suppress compiler warning
;; jrv/frequent-keys is a list of elements of the form
;; ("LETTER" . DEFINITION)
;; Note: LETTER is a (single character) string,
;;       but DEFINITION is an unquoted lisp object
;; we run mapc below to create a define-key for each such element
(setq jrv/frequent-keys
      '(
        ("b" . ido-switch-buffer)
        ("B" . jrv/buffer-switch-buffer-other-window)
        ("c" . quick-calc)
        ("f" . find-file)
        ("g" . jrv/mypaths-find-file-real-path)
        ("i" . jrv/mypaths-get-real-path)
        ;; ido uses remap key to change all key bindings of kill-buffer
        ;; to use ido-kill-buffer
        ;; if we want to bind a key to the original kill-buffer,
        ;; we have to wrap it inside a function
        ("k" . (lambda () (interactive) (kill-buffer)))
        ("K" . jrv/buffer-kill-buffer-other-window)
        ("m" . make-frame)
        ("s" . save-buffer)
        ("w" . ido-select-window)
        ("&" . jrv/shell-command-on-buffer-file)
        ;; ("`" . self-insert-command)
        ))

(mapc
 (lambda (pair)
   (define-key global-map
     (kbd (concat "ESC M-" (car pair)))
     (cdr pair)))
 jrv/frequent-keys)
