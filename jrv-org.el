;;; jrv-org.el --- 
(provide 'jrv-org)

(eval-when-compile (require 'subr-x))
(require 'my-settings)          ; my settings 
(require 'jrv-mypaths)          ; define my commonly used paths
(require 'org)
(require 'org-clock)
(require 'org-timer)

;; save clock history across sessions
;; (defvar org-clock-persist)
;; (setq org-clock-persist t)
;; (declare-function org-clock-persistence-insinuate "org")
;; (org-clock-persistence-insinuate)

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
(setq org-archive-location (concat (jrv-org-prefix jrv-org-arch-file) "::"))

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

;; custom view for Unscheduled TODOs
(defvar org-agenda-custom-commands)
(setq org-agenda-custom-commands
      '(("u" todo "TODO"
         ((org-agenda-todo-ignore-scheduled 'all)
          (org-agenda-todo-ignore-deadlines 'all)
          (org-agenda-overriding-header "Unscheduled TODOs: ")))))

;; use habit style in todo
(add-to-list 'org-modules 'org-habit t)

;; Show only today's time in the mode line when clocking tasks
(defvar org-clock-mode-line-total)
(setq org-clock-mode-line-total 'today)

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
  (jrv/org-clock-populate)
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
    ;; now make org agenda visible in screen 0
    (when (featurep 'escreen)
      (declare-function escreen-goto-screen-0 "escreen")
      (escreen-goto-screen-0))
    (org-agenda-list)))

;; org-timer-pause-or-continue is a toggle
;; we use this function to create separate pause and continue functions
(defun jrv/pause-timer (&rest arg)
  "Pause timer"
  (interactive)
  (when (and (boundp 'org-timer-pause-time)
             (not org-timer-pause-time))
    (org-timer-pause-or-continue)))

(defun jrv/resume-timer (&rest arg)
  (interactive) 
  (when (not org-timer-start-time) (error "No timer is running"))
  (when org-timer-pause-time (org-timer-pause-or-continue)))

(advice-add 'org-clock-in :after #'jrv/resume-timer)
(advice-add 'org-clock-out :after #'jrv/pause-timer)

(defvar org-agenda-keymap)
(add-hook 'org-agenda-mode-hook
          (lambda () (define-key org-agenda-keymap (kbd "Z")
              (lambda () (interactive) (org-refile '(4))))))

;; org capture
(defvar org-default-notes-file)
(setq org-default-notes-file h-org-jrv-file)
(defvar org-capture-templates)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         ;; "* TODO %? %a %i\nSCHEDULED: <%(org-read-date)>\n")))
         "* TODO %? %a %i\nSCHEDULED: %(org-insert-time-stamp (current-time))\n")))
(require 'org-notmuch)

(defvar org-catch-invisible-edits)
(setq org-catch-invisible-edits 'smart)

(defun jrv/org-clock-populate-one-pattern (pattern)
  (let ((buffer (find-file-noselect h-org-jrv-file)))
    (save-current-buffer
      (set-buffer buffer)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (concat "^" pattern "$") nil t)
          (move-beginning-of-line nil)
          (org-clock-history-push (point) buffer))))))

(defvar org-clock-history-length)
(setq org-clock-history-length 10)
(defvar jrv-org-clock-patterns-file
  (jrv-org-prefix-and-suffix "jrv-clock-patterns.txt"))
(defun jrv/org-clock-populate ()
  "Clear org-clock-history and populate from patterns file"
  (interactive)
  (setq org-clock-history nil)
  (mapc 'jrv/org-clock-populate-one-pattern
        (with-temp-buffer
          (insert-file-contents jrv-org-clock-patterns-file)
          (split-string (buffer-string) "\n" t))))
