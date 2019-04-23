(provide 'jrv-org-functions)

(eval-and-compile  ;; suppress compiler warnings
  (require 'package)
  (setq package-enable-at-startup nil)
  (package-initialize)
  (add-to-list 'load-path "~/.emacs.d/my-lisp/")
  (require 'org)
  (require 'org-clock)
  (require 'org-agenda)
  (require 'org-timer)
  (require 'jrv-settings)
  (require 'jrv-mypaths)
  )

(defun jrv/org-open()
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

(defun jrv/org-clock-or-timer-active ()
  "Is clock or timer active?"
  (or (org-clocking-p) (and (boundp 'org-timer-pause-time)
                                  (not org-timer-pause-time))))

(defun jrv/org-clock-out-and-pause-timer ()
  "Clock out and pause timer"
  (interactive)
   ;; act only if clock-or-timer-active
  (when (jrv/org-clock-or-timer-active)
    ;; clock out without error 
    (org-clock-out nil t nil) 
    ;; now make org agenda visible in main frame
    (select-frame-by-name "")
    (org-agenda-list)))

(defun jrv/org-pause-timer (&rest arg)
  "Pause org timer if running"
  ;; org-timer-pause-or-continue is a toggle
  ;; we create separate pause function
  (interactive)
  (when (and (boundp 'org-timer-pause-time)
             (not org-timer-pause-time))
    (org-timer-pause-or-continue)))

(defun jrv/org-resume-timer (&rest arg)
  "Resume org timer if running"
  ;; org-timer-pause-or-continue is a toggle
  ;; we create separate continue function
  (interactive) 
  (when (not org-timer-start-time) (error "No timer is running"))
  (when org-timer-pause-time (org-timer-pause-or-continue)))

(advice-add 'org-clock-in :after #'jrv/org-resume-timer)
(advice-add 'org-clock-out :after #'jrv/org-pause-timer)

(defun jrv/org-clock-populate-one-entry (pattern)
  "Find item in main org file and create clock history entry for it"
  (let ((buffer (find-file-noselect
                 (jrv/mypaths-real-path "main-org-file"))))
    (save-current-buffer
      (set-buffer buffer)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (concat "^" pattern "$") nil t)
          (move-beginning-of-line nil)
          (org-clock-history-push (point) buffer))))))

(defun jrv/org-clock-populate ()
  "Clear org-clock-history and populate from entries file"
  (interactive)
  (setq org-clock-history nil)
  (mapc 'jrv/org-clock-populate-one-entry
        (with-temp-buffer
          (insert-file-contents
           (jrv/mypaths-real-path "jrv-clock-entries"))
          (split-string (buffer-string) "\n" t))))
