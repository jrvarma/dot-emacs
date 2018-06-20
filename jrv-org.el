;;; jrv-org.el --- 
(provide 'jrv-org)

(eval-when-compile (require 'subr-x))
(require 'my-settings)          ; my settings 
(require 'jrv-mypaths)          ; define my commonly used paths
(require 'org)

;; Open files with the extension ".org" in org-mode.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(defvar h-org)  ; from jrv-mypaths
(defun jrv-org-prefix (f)
  (interactive)
  (concat h-org "/" f))

(defvar my-encrypt-org-files)    ; from my-settings
(defun jrv-org-suffix (f)
  (interactive)
  (if my-encrypt-org-files
      (concat f ".gpg")
    f))

(defun jrv-org-prefix-and-suffix (f)
  (interactive)
  (jrv-org-prefix (jrv-org-suffix f)))

(defvar h-org-jrv-file (jrv-org-prefix-and-suffix "jrv.org")) 
(defvar org-agenda-files)
(setq org-agenda-files
      (cl-remove-if-not #'file-exists-p
                        (mapcar 'jrv-org-prefix-and-suffix
                                (list "jrv.org" "jrv-calendar.org"))))
(defvar jrv-org-target-files)
(setq jrv-org-target-files
      (mapcar 'jrv-org-suffix
              (list "jrv.org" "jrv-P.org" "jrv-someday.org")))
(defvar jrv-org-target-paths)
(setq jrv-org-target-paths
      (cl-remove-if-not #'file-exists-p
                        (mapcar 'jrv-org-prefix
                                jrv-org-target-files)))
(defvar org-refile-targets)
;; backquote comma combination is for eval of variable inside quote
(setq org-refile-targets `((,jrv-org-target-paths :maxlevel . 2)))

(defvar jrv-org-arch-file (jrv-org-suffix "jrv.arch.org"))
(defvar org-archive-location)
(setq org-archive-location (jrv-org-prefix jrv-org-arch-file))

;; projects are identified by the :proj: tag which should not be inherited
(defvar org-tags-exclude-from-inheritance)
(setq org-tags-exclude-from-inheritance '("proj"))

;; stuck projects are projects without a "next" action
(defvar org-stuck-projects)
(setq org-stuck-projects
      '("+proj" ("TODO" "NEXT" "WAITING") ))

;; always start agenda from today
(defvar org-agenda-start-on-weekday)
(setq org-agenda-start-on-weekday nil)

;; do not reorganize frame to show agenda
(defvar org-agenda-window-setup)
(setq org-agenda-window-setup 'current-window)

;; show breadcrumbs but not categories in agenda view
(defvar org-agenda-prefix-format)
(setq org-agenda-prefix-format ;; removed categories
      '((agenda . " %i %-12:b%?-12t% s")
        (timeline . "  % s")
        (todo . " %i %-12:b")
        (tags . " %i %-12:b")
        (search . " %i %-12:b")))
;; (setq org-agenda-prefix-format ;; original
;;       '((agenda . " %i %-12:c%?-12t% s")
;;         (timeline . "  % s")
;;         (todo . " %i %-12:c")
;;         (tags . " %i %-12:c")
;;         (search . " %i %-12:c")))
;; (setq org-agenda-prefix-format ;; added breadcrumbs
;;       '((agenda . " %i %-12:c%-12:b%?-12t% s")
;;         (timeline . "  % s")
;;         (todo . " %i %-12:c%-12:b")
;;         (tags . " %i %-12:c%-12:b")
;;         (search . " %i %-12:c%-12:b")))

;; use habit style in todo
(add-to-list 'org-modules 'org-habit t)

;; Show only today's time in the mode line when clocking tasks
(defvar org-clock-modeline-total)
(setq org-clock-modeline-total 'today)

;; basic set of keywords 
(defvar org-todo-keywords)
(setq org-todo-keywords '((sequence "TODO" "NEXT" "WAITING" "|" "DONE" "DISCARDED")))

;; color-code task types.
(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "yellow" :background "red" :bold t :weight bold))
        ("TODO" . (:foreground "cyan" :background "steelblue" :bold t :weight bold))
        ("WAITING" . (:foreground "yellow" :background "magenta2" :bold t :weight bold))
        ("DONE" . (:foreground "gray50" :background "gray30"))))

;; use shift keys to select
(defvar org-support-shift-select)
(setq org-support-shift-select 'always)

;; record when to-do was finished
(defvar org-log-done)
(setq org-log-done t)

(defvar org-log-into-drawer)
(setq org-log-into-drawer t)

(defvar org-timer-pause-time)
(defvar my-timelog-categories-file) ; from my-settings
(defun my-org()
  "Open my org agenda."
  (interactive)
  (org-agenda-list)
  (org-timer-start)
  (setq org-timer-pause-time (current-time))
  (org-timer-set-mode-line 'pause))

;; Function for quick interruption of timers and task clocking
;; * clock out of any task being clocked
;; * pause timer if any
;; Similarly function to resume timers and start task clocking

(declare-function org-clocking-p "org")
(defun clock-or-timer-active ()
  (or (org-clocking-p) (and (boundp 'org-timer-pause-time)
                                  (not org-timer-pause-time))))

(declare-function org-clock-out "org")
(declare-function org-timer-set-mode-line "org")
(declare-function org-timer-value-string "org")
(defun clock-out-and-pause-timer ()
  "Clock out and pause timer"
  (interactive)
  (when (clock-or-timer-active) ;; do nothing unless clock-or-timer-active
    ;; clock out without error 
    (org-clock-out nil t nil) 
    ;; pause timer 
    ;; this code modified from 
    ;; source code of org-timer-pause-or-continue
    (when (and (boundp 'org-timer-pause-time)
               (not org-timer-pause-time))
      (setq org-timer-pause-time (current-time))
      (org-timer-set-mode-line 'pause)
      (message "Timer paused at %s" (org-timer-value-string)))
    ;; now make org agenda visible in screen 0
    (when (featurep 'escreen)
      (declare-function escreen-goto-screen-0 "escreen")
      (escreen-goto-screen-0))
    (org-agenda-list)))

(declare-function org-agenda-clock-in "org-agenda")
;; (declare-function org-float-time "org")
(defvar org-timer-start-time)
(defun clock-in-and-resume-timer (arg)
  (interactive "P") 
  (org-agenda-clock-in arg)         ; clock in 
  (cond                             ; resume timer
   ((not org-timer-start-time) (error "No timer is running"))
   (org-timer-pause-time
    ;; timer is paused, continue
    (setq org-timer-start-time
          (seconds-to-time
           (-
            (float-time)
            (- (float-time org-timer-pause-time)
               (float-time org-timer-start-time))))
          org-timer-pause-time nil)
    (org-timer-set-mode-line 'on)
    (run-hooks 'org-timer-continue-hook)
    (message "Timer continues at %s" (org-timer-value-string)))))

(defvar org-agenda-keymap)
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (define-key org-agenda-keymap (kbd "Z")
              (lambda () (interactive) (setq current-prefix-arg '(4))
                (call-interactively 'org-refile)))
            (define-key org-agenda-keymap (kbd "I") 'clock-in-and-resume-timer)
            (define-key org-agenda-keymap (kbd "O") 'clock-out-and-pause-timer)))

;; org capture
(defvar org-default-notes-file)
(setq org-default-notes-file h-org-jrv-file)
(require 'org-notmuch)

(defvar org-catch-invisible-edits)
(setq org-catch-invisible-edits 'smart)

