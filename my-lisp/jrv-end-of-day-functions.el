(provide 'jrv-end-of-day-functions)

(eval-and-compile  ;; suppress compiler warnings
  (require 'package)
  (setq package-enable-at-startup nil)
  (package-initialize)
  (add-to-list 'load-path "~/.emacs.d/my-lisp/")
  (require 'jrv-mypaths)
  (require 'jrv-settings)
)

;; ;;;;;;;;;;;;;;;;;; suppress compiler warnings ;;;;;;;;;;;;
(declare-function R "ess-r-mode.el")
(declare-function ess-eval-linewise "ess-inf.el")
;; ;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;

;;;; daily windup
(defun jrv/end-of-day-reminder()
  "Announce time as a reminder at end od day"
  (interactive)
  (let ((adjtd-time
         ;; emacs seem to run-at-time one second too early. 
         ;; We add 5 sec to offset this
         (seconds-to-time (+ 5 (float-time (current-time))))))
    (call-process "espeak" nil 0 nil "-s 100" "-a 100" 
                  (concat "It is " (format-time-string "%I:%M %p" 
                                                       adjtd-time)))))

(defun jrv/end-of-day-timers()
  "Set up timers for end-of-day reminders"
  (interactive)
  (when jrv/settings-windup-time 
    (let* ((start-of-today 
            (apply
             'encode-time
             (parse-time-string
              (concat (format-time-string "%Y-%m-%d") " 00:00:00"))))
           (hours (nth 2 (parse-time-string jrv/settings-windup-time)))
           (minutes (nth 1 (parse-time-string jrv/settings-windup-time)))
           (run-time
            (time-add start-of-today (+ (* 3600 hours) (* 60 minutes))))
           (cancel-time (time-add run-time (* 2 3600)))
           (stale-time (time-add run-time  (* 15 60))))
      (when (time-less-p (current-time) stale-time)
        (run-at-time run-time (* 15 60) 'jrv/end-of-day-windup)
        (run-at-time cancel-time nil
                     '(lambda() (cancel-function-timers
                                 'jrv/end-of-day-windup)))))))

(defun jrv/end-of-day-timelog ()
  "Run timelog.R at end of day"
  (interactive)
  (let ((jrv-htimelog (concat (jrv/mypaths-real-path "h-time")
                              "/" "timelog.csv.gpg"))
        (jrv-jtimelog (concat (jrv/mypaths-real-path "j-time")
                              "/" "timelog.csv"))
        (jrv-jtimelogR (concat (jrv/mypaths-real-path "j-time")
                               "/" "timelog.R")))
    (find-file jrv-htimelog)
    (write-region nil nil jrv-jtimelog)
    (kill-buffer "timelog.csv.gpg")
    (R)
    (ess-eval-linewise (concat "source('" jrv-jtimelogR "')"))))

