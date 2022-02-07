(provide 'jrv-EOD-functions) ;; end-of-day functions

(eval-and-compile  ;; suppress compiler warnings
  (require 'package)
  (setq package-enable-at-startup nil)
  (package-initialize)
  (add-to-list 'load-path "~/.emacs.d/my-lisp/")
  ;; (require 'jrv-mypaths)
  (require 'jrv-settings)
)

;; ;;;;;;;;;;;;;;;;;; suppress compiler warnings ;;;;;;;;;;;;
(declare-function R "ess-r-mode.el")
(declare-function ess-eval-linewise "ess-inf.el")
;; ;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;

;;;; daily windup
(defvar jrv/EOD/reminder-suspended nil)
(defun jrv/EOD/reminder()
  "Announce time as a reminder at end of day"
  (interactive)
  (unless jrv/EOD/reminder-suspended
    (let ((adjtd-time
           ;; emacs seem to run-at-time one second too early. 
           ;; We add 5 sec to offset this
           (seconds-to-time (+ 5 (float-time (current-time))))))
      (call-process "espeak" nil 0 nil "-vm7" "-s 130" "-p 70" "-a 100" 
                    (concat "It is " (format-time-string "%I:%M %p" 
                                                         adjtd-time))))))

(defun jrv/EOD/suspend-reminder()
  "Suspend end of day reminder"
  (interactive)
  (setq jrv/EOD/reminder-suspended t)
  (run-at-time (* 60 (read-number "Suspend reminder for how many minutes? "))
               nil (lambda() (setq jrv/EOD/reminder-suspended nil))))

(defun jrv/EOD/restore-reminder()
  "Restore end of day reminder"
  (interactive)
  (setq jrv/EOD/reminder-suspended nil))

(defun jrv/EOD/timers()
  "Set up timers for EOD reminders"
  (interactive)
  (when jrv/settings/windup-time 
    (let* ((start-of-today 
            (apply
             'encode-time
             (parse-time-string
              (concat (format-time-string "%Y-%m-%d") " 00:00:00"))))
           (hours (nth 2 (parse-time-string jrv/settings/windup-time)))
           (minutes (nth 1 (parse-time-string jrv/settings/windup-time)))
           (run-time
            (time-add start-of-today (+ (* 3600 hours) (* 60 minutes))))
           (cancel-time (time-add run-time (* 2 3600)))
           (stale-time (time-add run-time  (* 15 60))))
      (when (time-less-p (current-time) stale-time)
        (run-at-time run-time (* 15 60) 'jrv/EOD/reminder)
        (run-at-time cancel-time nil
                     '(lambda() (cancel-function-timers
                                 'jrv/EOD/reminder)))))))

;; (defun jrv/EOD/timelog ()
;;   "Run timelog.R at end of day"
;;   (interactive)
;;   (let ((jrv-htimelog (concat jrv/settings/symlinks "h-time"
;;                               "/" "timelog.csv.gpg"))
;;         (jrv-jtimelog (concat jrv/settings/symlinks "j-time"
;;                               "/" "timelog.csv"))
;;         (jrv-jtimelogR (concat jrv/settings/symlinks "j-time"
;;                                "/" "timelog.R")))
;;     (find-file jrv-htimelog)
;;     (write-region nil nil jrv-jtimelog)
;;     (kill-buffer "timelog.csv.gpg")
;;     (R)
;;     (ess-eval-linewise (concat "source('" jrv-jtimelogR "')"))))

(defun jrv/EOD/timelog ()
  "Run timelog script at end of day"
  (interactive)
  (let ((timelog-script (concat jrv/settings/symlinks "timelog"
                                "/" "timelog.R")))
    (R)
    (ess-eval-linewise (concat "source('" timelog-script "')"))))

