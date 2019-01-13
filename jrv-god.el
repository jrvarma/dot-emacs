;; God Mode (emacs without modifier keys)
;; Used only in mobile phones (Termux) where modifier keys are hard to use
;; requires god-mode, bind-key and key-chord
(provide 'jrv-god)
(eval-when-compile 
  (require 'package)
  (package-initialize))
(require 'bind-key)

(defvar god-exempt-major-modes)
(defvar god-exempt-predicates)
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)
(declare-function god-mode-all "god-mode")

(declare-function unbind-key "bind-key")
(declare-function bind-key* "bind-key")
(defvar override-global-map)

(defvar my-god-key "M-1")

(defun bind-god-key ()
  "Bind my-god-key to toggle god-local-mode. Set modeline"
  (interactive)
  (bind-key* my-god-key 'my-god-mode-all))
  
(bind-god-key)

(defun unbind-god-key()
  "Unset god-mode binding of my-god-key"
  (interactive)
  (unbind-key my-god-key override-global-map))

(defvar original-modeline-bg)
(defvar original-modeline-inactive-bg)
(defvar original-modeline-fg)
(defvar original-modeline-inactive-fg)
(defvar god-global-mode)

(defun my-set-fg (active-god-fg inactive-god-fg)
   (let* ((active-color (if god-global-mode
                           active-god-fg
                         original-modeline-fg))
         (inactive-color (if god-global-mode
                             inactive-god-fg
                             original-modeline-inactive-fg)))
    (set-face-foreground 'mode-line active-color)
    (set-face-foreground 'mode-line-inactive inactive-color)))


(defun my-set-bg (active-god-bg inactive-god-bg)
  (let* ((active-color (if god-global-mode
                           active-god-bg
                         original-modeline-bg))
         (inactive-color (if god-global-mode
                             inactive-god-bg
                           original-modeline-inactive-bg)))
    (set-face-background 'mode-line active-color)
    (set-face-background 'mode-line-inactive inactive-color)))

(defvar limited-color-terminal)
(defvar original-modeline-fg nil)
(defvar original-modeline-inactive-fg nil)
(defvar original-modeline-bg nil)
(defvar original-modeline-inactive-bg nil)

(defun my-god-mode-all ()
  "toggle god-mode-all and set modeline color accordingly"
  (interactive)
  (god-mode-all)
  (unless original-modeline-bg
    (setq original-modeline-fg
          (face-attribute 'mode-line :foreground))
    (setq original-modeline-inactive-fg
          (face-attribute 'mode-line-inactive :foreground))
    (setq original-modeline-bg
          (face-attribute 'mode-line :background))
    (setq original-modeline-inactive-bg
          (face-attribute 'mode-line-inactive :background)))
  (if (not limited-color-terminal)
      (my-set-bg "#884444" "#FFAAAA")
    (my-set-fg "red" "white")
    (my-set-bg "white" "red")))

;; isearch integration
(require 'god-mode-isearch)
(defvar isearch-mode-map)
(defvar god-mode-isearch-map)
(define-key isearch-mode-map (kbd my-god-key) 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd my-god-key) 'god-mode-isearch-disable)

;; org-mode fast keys
(defvar god-local-mode-map)
(add-hook 'god-local-mode-hook
          '(lambda ()
             (define-key god-local-mode-map
               [remap self-insert-command] 'my-god-mode-self-insert)))

(defun my-god-mode-self-insert ()
  (interactive)
  (if (and (bolp)
           (eq major-mode 'org-mode))
      (call-interactively 'org-self-insert-command)
    (call-interactively 'god-mode-self-insert)))

;; (defun my-god-mode-update-modeline ()
;;   "Use the active modeline color to indicate that we are in god-mode"
;;   (defvar god-local-mode)
;;   (let* ((my-god-modeline-color "#884444")
;;          (my-god-modeline-inactive-color "#FFAAAA")
;;          (active-color (if god-local-mode
;;                            my-god-modeline-color
;;                          original-modeline-color))
;;          (inactive-color (if god-local-mode
;;                            my-god-modeline-inactive-color
;;                          original-modeline-inactive-color)))
;;     (set-face-background 'mode-line active-color)
;;     (set-face-background 'mode-line-inactive inactive-color)))

;; (defun my-god-modeline ()
;;   "activate my-god-mode-update-modeline by adding to hooks"
;;   (defvar god-local-mode-hook)
;;   (unless (and (boundp 'god-local-mode-hook)
;;                (member 'my-god-mode-update-modeline god-loca

;; (defun god-esc ()
;;   "Bind escape to toggle god-local-mode. Set modeline"
;;   (interactive)
;;   (bind-key* "<escape>" 'my-god-mode-all))
  
;; (defun god-9 ()
;;   "Bind 9 to toggle god-local-mode. Bind ,, to insert 9. Set modeline"
;;   (interactive)
;;   (bind-key* "9" 'my-god-mode-all)
;;   ;; If we want to enable/disable god-mode without using modifier keys
;;   ;; we can repurpose the "9" key for this purpose
;;   ;; We have lost the ability to type "9", we get it back using a key chord
;;   (require 'jrv-key-chord) 
;;   (declare-function key-chord-define-global "key-chord")
;;   (defun write-9 () (interactive) (insert-char 57))
;;   (key-chord-define-global ",," 'write-9))

;; (defun unbind-god-9()
;;   "Unset god-mode binding and restore original binding of the 9 key"
;;   (interactive)
;;   (unbind-key "9" override-global-map)
;;   (global-set-key "9" 'self-insert-command))

;; (defun unbind-god-esc()
;;   "Unset god-mode binding and restore original binding of the <escape> key"
;;   (interactive)
;;   (unbind-key "<escape>" override-global-map))
