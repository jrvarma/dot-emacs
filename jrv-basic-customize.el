(provide 'jrv-basic-customize)
(require 'my-settings)

;; set the theme
(defvar my-theme-name) ; from my-settings
(load-theme (intern my-theme-name))
;; set cursor color
;; the backquote-comma combination forces evaluation of my-cursor-color
(add-to-list 'default-frame-alist `(cursor-color . ,my-cursor-color))
(defvar my-maximize-at-startup)  ; from my-settings
(when my-maximize-at-startup
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

;;; Splash screen, Initial Buffer, buffer title line and col numbers
;; inhibit flash screen
(setq inhibit-startup-screen t)
;; buffername in frame title
(setq frame-title-format "%b - emacs")
;; Better uniqify buffer names   
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(defvar my-pop-up-windows) ; from my-settings
(setq pop-up-windows my-pop-up-windows)
;; Enable line numbers and column numbers.
(line-number-mode 1)
(column-number-mode 1)
;; menu bar and tool bar
(menu-bar-mode -1) ;; switch menu bar off
(tool-bar-mode -1)  ;; switch tool bar off
;;; Protect scratch and Messages buffers from accidental deletion
(require 'keep-buffers)                     ;; Load the package.
(keep-buffers-protect-buffer "*scratch*")   ;; Protect the *scratch* buffer
(keep-buffers-protect-buffer "*Messages*")  ;; Protect the *Messages* buffer
(keep-buffers-erase-on-kill nil)            ;; Do not erase buffers on kill
;; make message buffer bigger
(setq message-log-max 2000)
;;; History, recent files and backups
;; History
(savehist-mode t)                      ; do customization before activate
;; Recent files
(require 'recentf)
(recentf-mode 1)
;; Manage backups
(defvar my-backup-directory) ; from my-settings
;; syntax for backup-directory-alist is ((REGEXP . DIRECTORY))
;; the backquote-comma combination forces evaluation of my-backup-directory
(setq backup-directory-alist `(("." . ,my-backup-directory))) 
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;;; Confirm on exit
(defvar confirm-kill-emacs)
(setq confirm-kill-emacs 'y-or-n-p)

;;; Autofill, font lock, matching parantheses, mode sensitive help, 
;;  indent with spaces, case changing, yes/no questions
;; Disable  auto-fill completely
(setq-default fill-column most-positive-fixnum)
(defun de-fill ()
  "Remove filling from paragraph or region (useful mainly in message mode)"
  (interactive)
  (let ((old-fill-column fill-column))
	(set-fill-column most-positive-fixnum)
    (if (use-region-p)
        (fill-region (region-beginning) (region-end))
      (fill-paragraph))
	(set-fill-column old-fill-column))) 
;; visual mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'nxml-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))
;; Font-Lock for  syntax-highlighting
(defvar font-lock-use-default-fonts)
(setq font-lock-use-default-fonts t)
(defvar font-lock-use-default-colors)
(setq font-lock-use-default-colors t)
(require 'font-lock)
;; Always show matching parantheses 
(show-paren-mode 1)
;; Indent with spaces (mainly for python)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; Enable case change of region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; Major mode sensitive help agent.
(require 'info-look)
;; Replace yes/no questions with y/n
(defalias 'yes-or-no-p 'y-or-n-p)

(defvar calculator-number-digits)
(setq calculator-number-digits 6)

;; Let Ediff show files side by side without creating new frames
(defvar ediff-split-window-function)
(defvar ediff-window-setup-function)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(defvar ediff-keep-variants)
(setq ediff-keep-variants nil)

;; emacs does not use xdg-open for the browser unless it detects one of the supported desktop environments (gnome, KDE, XFCE, LXDE). See browse-url-can-use-xdg-open in browse-url.el. So we explicitly set the browser to xdg-open. Even when there is no desktop environment (for example, when using openbox or some other widow manager directly), xdg-open checks for $BROWSER which can be set correctly in ~/.profile by querying xdg-mime for text/html.
(setq browse-url-browser-function 'browse-url-xdg-open)

(defvar my-tap-to-click-toggle) ; from my-settings
;; this disables mousepad tap-to-click inside emacs and enables it outside
;; the magic codes here are for ETPS/2 Elantech Touchpad
(when my-tap-to-click-toggle
  (defun tap-to-click-disable()
    (start-process "t2c-disable" "*tap-to-click*" "xinput" "set-prop" "ETPS/2 Elantech Touchpad" "Synaptics Tap Action" "2," "3," "0," "0," "0," "0," "0"))

  (defun tap-to-click-enable()
    (start-process "t2c-disable" "*tap-to-click*" "xinput" "set-prop" "ETPS/2 Elantech Touchpad" "Synaptics Tap Action" "2," "3," "0," "0," "1," "3," "2"))

  (add-hook 'focus-in-hook 'tap-to-click-disable)
  (add-hook 'focus-out-hook 'tap-to-click-enable))

;; see outline-magic.el
(defvar outline-minor-mode-map)
(defvar outline-mode-prefix-map)
(add-hook 'outline-minor-mode-hook
          (lambda ()
            (require 'outline-magic)
            (define-key outline-minor-mode-map [(C-tab)] 'outline-cycle)
            (local-set-key [(control ?c) (control ? )] outline-mode-prefix-map)))
