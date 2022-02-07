(provide 'jrv-aliases-short-keys)

(defun jrv/aliases/echo-full-mode-line()
  "Echo full mode line (useful when modeline truncated)"
  (interactive)
  (message "%s" (format-mode-line mode-line-format)))

;;; Aliases for common commands
;; Typically space followed by letter (a/b/c/h/l/m/s/-/;)

(let (aliases-list)
  ;; aliases-list is a list of elements of the form
  ;; ("ALIAS" . DEFINITION)
  ;; Note: ALIAS is a string, but DEFINITION is an unquoted lisp object
  ;; we run mapc below to create a defalias for each such element
  (setq aliases-list
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
          (" s" . jrv/buffer/kill-star-buffers)
          (" -" . jrv/text/space-to-dash)
          (" ;" . jrv/dired/launch-file)
          (" g" . magit-status)
          ("&" . jrv/shell/command-on-buffer-file)
          ;; We have rebound C-x C-c to save-buffers-kill-emacs.
          ;; The original binding of C-x C-c is now aliased to 1k
          ("1k" . save-buffers-kill-terminal)
          ("2h" . jrv/shell/send-buffer-file-to-h)
          ("pr" . jrv/text/proper-case-region)
          ("wc" . jrv/text/wc)
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
   aliases-list))


(let (frequent-keys-list frequent-keys-list2)
  ;; Map "C-z C-?" (God-Mode z?) to frequent commands
  ;; Map "C-z ?" (God-Mode z<space>?) to less frequent commands
  ;;
  ;; For example, "C-z C-k" (God-Mode zk) kills buffer
  ;;              "C-z k" (God-Mode z k) kills buffer in other window
  (setq frequent-keys-list
        ;; frequent-keys-list is a list of elements of the form
        ;; ("LETTER" . DEFINITION)
        ;; Note: LETTER is a (single character) string,
        ;;       but DEFINITION is an unquoted lisp object
        ;; mapc below create a define-key for each such element
        '(
          ("b" . ido-switch-buffer)
          ("c" . quick-calc)
          ("f" . jrv/mypaths/find-file-short-path)
          ("i" . jrv/mypaths/get-file-short-path)
          ;; ido uses remap key to change all key bindings of kill-buffer
          ;; to use ido-kill-buffer
          ;; if we want to bind a key to the original kill-buffer,
          ;; we have to wrap it inside a function
          ("k" . (lambda () (interactive) (kill-buffer)))
          ("m" . make-frame)
          ("s" . save-buffer)
          ("w" . ido-select-window)
          ("z" . jrv/aliases/echo-full-mode-line)
          ("&" . jrv/shell/command-on-buffer-file)
          ("`" . font-lock-fontify-buffer)
          (";" . jrv/mypaths/launch-file-short-path)
          ("/" . jrv/mypaths/get-real-path)
          ;; ("`" . self-insert-command)
          ))
  (setq frequent-keys-list2
        '(
          ("b" . jrv/buffer/switch-buffer-other-window)
          ("k" . jrv/buffer/kill-buffer-other-window)
          ("f" . find-file)
          ))
  (define-prefix-command 'freq-keys)
  (global-set-key (kbd "C-z") 'freq-keys)
  (mapc
   (lambda (pair)
     (define-key global-map
       (kbd (concat "C-z C-" (car pair)))
       (cdr pair)))
   frequent-keys-list)
  (mapc
   (lambda (pair)
     (define-key global-map
       (kbd (concat "C-z " (car pair)))
       (cdr pair)))
   frequent-keys-list2))

;; insert \pause at the beginning of each beamer slide
(fset 'jrv/aliases/pause-in-beamer-slides
   [?\C-s ?# ?# end right ?\\ ?p ?a ?u ?s ?e])

;; Bind Alt-Tab to switch buffer
;; (declare-function bind-key* "bind-key")
(eval-and-compile  ;; suppress compiler warnings
  (require 'package)
  (setq package-enable-at-startup nil)
  (package-initialize)
  (require 'bind-key)
)

(bind-key* "C-M-i" 'ido-switch-buffer)
