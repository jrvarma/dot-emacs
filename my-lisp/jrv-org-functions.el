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

(defun jrv/org/open()
  "Open my org agenda."
  (interactive)
  (if (get-buffer org-agenda-buffer-name)
      (switch-to-buffer org-agenda-buffer-name)
    (org-agenda-list)
    (org-timer-start)
    (jrv/org/clock-populate)
    (setq org-timer-pause-time (current-time))
    (org-timer-set-mode-line 'pause)))

;; Function for quick interruption of timers and task clocking
;; * clock out of any task being clocked
;; * pause timer if any
;; Similarly function to resume timers and start task clocking

(defun jrv/org/clock-or-timer-active ()
  "Is clock or timer active?"
  (or (org-clocking-p) (and (boundp 'org-timer-pause-time)
                                  (not org-timer-pause-time))))

(defun jrv/org/clock-out-and-pause-timer ()
  "Clock out and pause timer"
  (interactive)
   ;; act only if clock-or-timer-active
  (when (jrv/org/clock-or-timer-active)
    ;; clock out without error 
    (org-clock-out nil t nil) 
    ;; now make org agenda visible in main frame
    (select-frame-by-name "Emacs Std")
    (org-agenda-list)))

(defun jrv/org/pause-timer (&rest arg)
  "Pause org timer if running"
  ;; org-timer-pause-or-continue is a toggle
  ;; we create separate pause function
  (interactive)
  (when (and (boundp 'org-timer-pause-time)
             (not org-timer-pause-time))
    (org-timer-pause-or-continue)))

(defun jrv/org/resume-timer (&rest arg)
  "Resume org timer if running"
  ;; org-timer-pause-or-continue is a toggle
  ;; we create separate continue function
  (interactive) 
  (when (not org-timer-start-time) (error "No timer is running"))
  (when org-timer-pause-time (org-timer-pause-or-continue)))

(advice-add 'org-clock-in :after #'jrv/org/resume-timer)
(advice-add 'org-clock-out :after #'jrv/org/pause-timer)

(defun jrv/org/clock-populate-one-entry (pattern)
  "Find item in main org file and create clock history entry for it"
  (let ((buffer (find-file-noselect
                 (concat jrv/settings/symlinks "org-file"))))
    (save-current-buffer
      (set-buffer buffer)
      (save-excursion
        (goto-char (point-min))
        (cond ((re-search-forward (concat "^" pattern " *$") nil t)
               (move-beginning-of-line nil)
               (org-clock-history-push (point) buffer))
              (t (lwarn 'JRVarma :warning
                        "Clockable entry not found for '%s'" pattern)))))))

(defun jrv/org/clock-populate ()
  "Clear org-clock-history and populate from entries file"
  (interactive)
  (setq org-clock-history nil)
  (let ((standard-history
         (with-temp-buffer
           (insert-file-contents
            (concat jrv/settings/symlinks "jrv-clock-entries"))
           (split-string (buffer-string) "\n" t))))
    (setq org-clock-history-length
          (min 35 (max 10 (length standard-history))))
    (mapc 'jrv/org/clock-populate-one-entry standard-history)))

(defun jrv/org/empty-clock-in()
  "Insert an empty (0 minutes) clock entry before the current line"
  (interactive)
  (beginning-of-line)
  (backward-char)
  (insert "\n")
  (insert org-clock-string " ")
  (org-time-stamp-inactive '(16))
  (insert "--")
  (org-time-stamp-inactive '(16))
  (insert " =>  0:00")
  (backward-char)
  (beginning-of-line)
  (indent-for-tab-command))

(defun jrv/org/org-git()
  "Save org buffers, run org-git and revert org buffers"
  (interactive)
  ;; 1. save org buffers
  ;; 2. push/pull/merge changes.
  ;; 3. revert buffers
  (org-save-all-org-buffers)
  (message "Running Org Git in Background")
  (set-process-sentinel 
   (start-process "org-git" "*org-git*" "org-git")
   '(lambda (process event)
      (if (string= (replace-regexp-in-string "[ \t\n]*" "" event)
                   "finished")
          (org-revert-all-org-buffers)
        (message "Org Git Had Some Problems")
        (switch-to-buffer "*org-git*")))))
