;;;; K E Y     B I N D I N G S

(provide 'jrv-key-alias)

;; set windmove key bindings using bind-key to override bindings in other modes
;; (require 'windmove)
;; (require 'bind-key)
;; (bind-key* "C-M-<left>" 'windmove-left)
;; (bind-key* "C-M-<right>" 'windmove-right)
;; (bind-key* "C-M-<up>" 'windmove-up)
;; (bind-key* "C-M-<down>" 'windmove-down)
(eval-when-compile 
  (require 'package)
  (package-initialize))

(require 'my-settings)
(defvar my-require-key-chord)       ; from my-settings
(when my-require-key-chord
  (require 'jrv-key-chord))         ;; key-chord alternatives for function keys

;; always delete whole line
(setq kill-whole-line 'always) 
;; typing replaces selection
(if (fboundp 'pending-delete-mode)
    (pending-delete-mode 1))
;; cut paste into clipboard
(when (display-graphic-p)
  (setq select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; shifted keys for functions on other window
;; eg, [(control x) (shift b)] does on other window
;; what [(control x) (b)] does on same window
(global-set-key [(control x) (shift b)] 'switch-buffer-other-window) 
(global-set-key [(control x) (shift k)] 'kill-buffer-other-window)
;; [(control x) (shift o)] is analogous to [(control x) (o)] 'other-window
(global-set-key [(control x) (shift o)] 'swap-buffers-in-windows)      
;; (global-set-key [(control x) (!)] 'delete-other-windows-and-buffer)
;; F4 cycles through other buffers 
;; S-F4 cycles through other buffers in other window
(global-set-key [f4] 'ido-switch-buffer)     ; in addition to [(control x) b] 
(global-set-key [(shift f4)] 'switch-buffer-other-window) 
;; shifted keys for dired in other window
(global-set-key [(control x) (shift d)] 'dired-in-other-window)
(global-set-key [(control x) (shift control f)] 'find-file-in-other-window)

;; M-C-! like M-!, but executes in a terminal. 
(global-set-key [(meta control !)] 'shell-command-in-shell)

(global-set-key [(control x) (control x)] 'decapitalize-word)

;; Since I often press C-z by mistake I initially made it do nothing
;; (global-unset-key [(control z)])
;; Then put it to good alternative use except in termux
(require 'my-settings)
(defvar my-phone)
(unless my-phone
  (global-set-key [(control z)] 'delete-other-windows))

;; org-mode keys
(declare-function org-clock-in "org-clock")
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cA" 'org-agenda)
(define-key global-map "\C-ca" (lambda (&optional arg)
                                 (interactive "P")(org-agenda arg "a")))
(define-key global-map "\C-cz"  
  (lambda () (interactive) (org-clock-in '(4))))

;; On my computer, I want C-x C-c to kill daemon also
;; By default C-x C-c runs save-buffers-kill-terminal which does not kill daemon
;; The kill-emacs command does not offer to save files, so we use save-buffers-kill-emacs
;; The old binding save-buffers-kill-terminal is later aliased to 1k below
(unless my-phone
  (define-key global-map "\C-x\C-c" 'save-buffers-kill-emacs))

;;; Abbreviations for common commands
;; space followed by letter (a/b/c/h/l/m/s)
;; backslash is needed to include punctuation characters in function names
(defalias '\ a '(lambda () (interactive) (ansi-term "/bin/bash")))
(defalias '\ b '(lambda ()
                  (interactive)
                  (async-shell-command "bashmount" "*bashmount*")
                  (other-window 1)
                  ))
(defalias '\ c 'calculator)
(defalias '\ h 'hfyview-buffer)

(defalias '\ l 'locate-with-filter)
(defvar locate-command)
(setq locate-command "locate -iA")
(defvar locate-prompt-for-command t)
(setq locate-prompt-for-command t)

(defalias '\ m 'menu-bar-mode)
(defalias '\ s 'kill-star-buffers)
(defalias '2h 'send-buffer-file-to-h)
(defalias '\ - 'space-to-dash)

(defalias '\ \; 'mylaunch)
(defalias '\ g 'magit-status)

;; We have rebound C-x C-c to save-buffers-kill-emacs.
;; The original binding save-buffers-kill-terminal is now aliased to 1k
(defalias '1k 'save-buffers-kill-terminal)

(defvar jrv/frequent-keys)
(setq jrv/frequent-keys
      '(
        ("b" . ido-switch-buffer)
        ("B" . switch-buffer-other-window)
        ("c" . quick-calc)
        ("f" . find-file)
        ("g" . jrv-find-file-real-path)
        ("i" . get-real-path)
        ;; ido uses remap key to change all key bindings of kill-buffer to use ido-kill-buffer
        ;; if we want to bind a key to the original kill-buffer, we have to wrap it inside a function
        ("k" . (lambda () (interactive) (kill-buffer)))
        ("K" . kill-buffer-other-window)
        ("m" . make-frame)
        ("s" . save-buffer)
        ("w" . ido-select-window)
        ("`" . self-insert-command)))

;; (defvar jrv/backquote-map)
;; (setq jrv/backquote-map (make-sparse-keymap))
;; (mapc
;;  (lambda (pair) (define-key jrv/backquote-map (kbd (car pair)) (cdr pair)))
;;  jrv/frequent-keys)
;; (define-key global-map "`" jrv/backquote-map)

(mapc
 (lambda (pair) (define-key global-map (kbd (concat "ESC M-" (car pair))) (cdr pair)))
 jrv/frequent-keys)

