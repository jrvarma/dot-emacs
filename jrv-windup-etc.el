;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THIS CODE IS HIGHLY SPECIFIC TO MY NEEDS                 ;;
;; AND IS PROBABLY USELESS TO ANYBODY ELSE                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; windup timelog load-jrv-org
(provide 'jrv-windup-etc)
(require 'jrv-mypaths)
(require 'my-settings)
(defvar my-windup-time) ; from my-settings
(declare-function dired-maybe-insert-subdir "dired.el")
(declare-function my-org "jrv-org.el")
(declare-function R "ess-r-d.el")
(declare-function ess-eval-linewise "ess-inf.el")

;;;; daily windup
(defun windup()
  "Announce time as a reminder to wind up"
  (let ((adjtd-time (seconds-to-time (+ 5 (float-time (current-time))))))
    ;; emacs seem to run-at-time one second too early. 
    ;; We add 5 sec to offset this
    (call-process "espeak" nil 0 nil "-s 100" "-a 100" 
                  (concat "It is " (format-time-string "%I:%M %p" 
                                                       adjtd-time)))))
;; schedule windup reminder
(when my-windup-time 
  (let* ((start-of-today 
          (apply
           'encode-time
           (parse-time-string
            (concat (format-time-string "%Y-%m-%d") " 00:00:00"))))
         (hours (nth 2 (parse-time-string my-windup-time)))
         (minutes (nth 1 (parse-time-string my-windup-time)))
         (run-time (time-add start-of-today (+ (* 3600 hours) (* 60 minutes))))
         (stale-time (time-add run-time  (* 15 60))))
    (when (time-less-p (current-time) stale-time)
      (run-at-time run-time (* 15 60) 'windup))))

(defun cancel-windup()
  "Cancel windup timer"
  (interactive)
  (cancel-function-timers 'windup))

(defvar h-time) ; from my-paths
(defvar j-time) ; from my-paths
(defun timelog ()
  "run timelog.R"
  (interactive)
  (let ((jrv-htimelog (concat h-time "/" "timelog.csv.gpg"))
        (jrv-jtimelog (concat j-time "/" "timelog.csv"))
        (jrv-jtimelogR (concat j-time "/" "timelog.R")))
    (find-file jrv-htimelog)
    (write-region nil nil jrv-jtimelog)
    (kill-buffer "timelog.csv.gpg")
    (R)
    (ess-eval-linewise (concat "source('" jrv-jtimelogR "')"))))

