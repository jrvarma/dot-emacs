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

(defvar my-timelog-categories-file) ; from my-settings
(declare-function org-insert-time-stamp "org")
(defun my-org()
  "Open my org file."
  (interactive)
  (find-file h-org-jrv-file)
  (when my-timelog-categories-file
    (goto-char (point-min))
    (search-forward "* -- TIME LOG --\n" nil 0)
    (insert "* ")
    (org-insert-time-stamp nil nil t)
    (insert-file-contents my-timelog-categories-file)
    ))

;; Press F9 to perform quick interruption of timers and task clocking
;; 1. shift to org file
;; 2. clock out of any task being clocked
;; 3. pause timer if any
;; Press Shift F9 for reverse: resume timers and start task clocking

(declare-function org-clock-out "org")
(defvar org-timer-pause-time)
(declare-function org-timer-set-mode-line "org")
(declare-function org-timer-value-string "org")
(defun clock-out-and-pause-timer ()
  "Clock out and pause timer"
  (interactive) 
  ;; shift to org file
  (find-file h-org-jrv-file)
  ;; clock out without error 
  (org-clock-out nil t nil) 
  ;; pause timer 
  ;; this code modified from 
  ;; source code of org-timer-pause-or-continue
  (cond 
   ((not org-timer-pause-time) 
    (setq org-timer-pause-time (current-time))
    (org-timer-set-mode-line 'pause)
    (message "Timer paused at %s" (org-timer-value-string)))))

(declare-function org-clock-in "org")
(declare-function org-float-time "org")
(defvar org-timer-start-time)
(defun clock-in-and-resume-timer ()
  (interactive) 
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

(global-set-key [(f9)] 'clock-out-and-pause-timer)

(global-set-key [(shift f9)] 'clock-in-and-resume-timer)




