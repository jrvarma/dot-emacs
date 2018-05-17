;;; jrv-org.el --- 
(provide 'jrv-org)

(eval-when-compile (require 'subr-x))
(require 'my-settings)          ; my settings 
(require 'jrv-mypaths)          ; define my commonly used paths
(require 'org)

;; Open files with the extension ".org" in org-mode.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(defvar h-org-jrv-file)  ; from jrv-mypaths

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

(defvar org-timer-pause-time)
(defvar my-timelog-categories-file) ; from my-settings
(declare-function org-insert-time-stamp "org")
(defvar my-org-buffer) ; defined below
(defun my-org()
  "Open my org file."
  (interactive)
  (if (not h-org-jrv-file)
      (error "h-org-jrv-file not set"))
  (find-file h-org-jrv-file)
  (setq my-org-buffer (current-buffer))
  (when my-timelog-categories-file
    (goto-char (point-min))
    (search-forward "* -- TIME LOG --\n" nil 0)
    (insert "* ")
    (org-insert-time-stamp nil nil t)
    (insert-file-contents my-timelog-categories-file)
    (org-timer-start)
    (setq org-timer-pause-time (current-time))
    (org-timer-set-mode-line 'pause)))

;; Press F9 to perform quick interruption of timers and task clocking
;; 1. shift to org file
;; 2. clock out of any task being clocked
;; 3. pause timer if any
;; Press Shift F9 for reverse: resume timers and start task clocking

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
    ;; shift to org file invisibly and clock out
    (let* ((old-buffer (current-buffer)))
      (set-buffer my-org-buffer)
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
      (set-buffer old-buffer))
    ;; now make org file visible in screen 0
    (when (featurep 'escreen)
      (declare-function escreen-goto-screen-0 "escreen")
      (escreen-goto-screen-0))
    (if (get-buffer-window my-org-buffer)
        ;; if my-org-buffer is in a visible window select that window
        (select-window (get-buffer-window my-org-buffer))
      ;; else switch to my-org-buffer in current window
      (switch-to-buffer my-org-buffer))))

(declare-function org-clock-in "org")
(declare-function org-float-time "org")
(defvar org-timer-start-time)
(defun clock-in-and-resume-timer ()
  (interactive) 
  (if (not h-org-jrv-file)
      (error "h-org-jrv-file not set"))
  (find-file h-org-jrv-file)        ; shift to org file
  (org-clock-in)                    ; clock in 
  (cond                             ; resume timer
   ((not org-timer-start-time) (error "No timer is running"))
   (org-timer-pause-time
    ;; timer is paused, continue
    (setq org-timer-start-time
          (seconds-to-time
           (-
            (org-float-time)
            (- (org-float-time org-timer-pause-time)
               (org-float-time org-timer-start-time))))
          org-timer-pause-time nil)
    (org-timer-set-mode-line 'on)
    (run-hooks 'org-timer-continue-hook)
    (message "Timer continues at %s" (org-timer-value-string)))))

(add-hook 'org-mode-hook
          (lambda () (define-key org-mode-map [(control c) (control x) ?\t]
                       'clock-in-and-resume-timer)))

;; org capture
(defvar org-default-notes-file)
(setq org-default-notes-file h-org-jrv-file)
(require 'org-notmuch)

(defvar org-catch-invisible-edits)
(setq org-catch-invisible-edits 'smart)

