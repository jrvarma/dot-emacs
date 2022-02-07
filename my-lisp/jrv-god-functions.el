;; God Mode (emacs without modifier keys)
(provide 'jrv-god)
(defvar original-modeline-bg)
(defvar original-modeline-inactive-bg)
(defvar original-modeline-fg)
(defvar original-modeline-inactive-fg)
(defvar god-global-mode)
(declare-function god-mode-all "god-mode")

(defun jrv/god/set-fg (active-god-fg inactive-god-fg)
   (let* ((active-color (if god-global-mode
                           active-god-fg
                         original-modeline-fg))
         (inactive-color (if god-global-mode
                             inactive-god-fg
                             original-modeline-inactive-fg)))
    (set-face-foreground 'mode-line active-color)
    (set-face-foreground 'mode-line-inactive inactive-color)))


(defun jrv/god/set-bg (active-god-bg inactive-god-bg)
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
(defvar jrv/settings/cursor-color)
(defvar jrv/settings/god-cursor-color)


(defun jrv/god/god-mode-all ()
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
  (if (> (length (defined-colors)) 255)
      (jrv/god/set-bg "#884444" "#FFAAAA")
    (jrv/god/set-fg "red" "white")
    (jrv/god/set-bg "white" "red"))
  (if god-global-mode
      (custom-set-faces
       `(cursor ((t (:background ,jrv/settings/god-cursor-color)))))
    (custom-set-faces
     `(cursor ((t (:background ,jrv/settings/cursor-color)))))))

;; ;; isearch integration
;; (require 'god-mode-isearch)
;; (defvar isearch-mode-map)
;; (defvar god-mode-isearch-map)
;; (define-key isearch-mode-map (kbd jrv/god/god-key) 'god-mode-isearch-activate)
;; (define-key god-mode-isearch-map (kbd jrv/god/god-key) 'god-mode-isearch-disable)


(defun jrv/god/god-mode-self-insert ()
  (interactive)
  (if (and (bolp)
           (eq major-mode 'org-mode))
      (call-interactively 'org-self-insert-command)
    (call-interactively 'god-mode-self-insert)))


(defun jrv/god/insert-backtick ()
  (interactive)
  (insert-char 96))

(declare-function ido-select-window "ido-select-window")
(defun jrv/god/select-window ()
  (interactive)
  (ido-select-window)
  (jrv/god/god-mode-all))

