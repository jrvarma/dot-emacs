;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THIS CODE IS HIGHLY SPECIFIC TO MY NEEDS                 ;;
;; AND IS PROBABLY USELESS TO ANYBODY ELSE                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My way of running emacs is as follows:                   ;;
;; Run emacs as daemon. No emacs frame is displayed.        ;;
;; This loads functions in this file but does not run them. ;;
;;       emacs --daemon                                     ;;
;; I use the following command to display the emacs frame   ;;
;; with my set of windows/buffers/screens. The command can  ;;
;; be run again if the emacsclient is killed                ;;
;;    nohup emacsclient -c -e "(run-jrv-finish t t t)" &    ;;
;; Some of my files are in the cloud. After login,          ;;
;; I wait for dropbox to sync, then I process that folder   ;;
;; outside emacs. When the files are ready for use, I run   ;;
;; the command                                              ;;
;;    emacsclient --eval "(after-dropbox-sync)"             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'jrv-finish)
(require 'jrv-org) ;; needed for my-org 
(require 'jrv-git) ;; needed for local-git-copy
(require 'jrv-notmuch)          ; notmuch configuration
(require 'my-settings)

(when (= (buffer-size (get-buffer "*Compile-Log*")) 0)
  (kill-buffer (get-buffer "*Compile-Log*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rest of this file contains only function defuns   
;; after-dropbox-sync and run-jrv-finish are
;; intended to be run using emacsclient --eval
;; other functions are helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare-function local-git-copy "jrv-git")
(declare-function dired-maybe-insert-subdir "dired")

(defvar my-open-org-file)           ; from my-settings
(defvar my-split-window-at-startup) ; from my-settings
(defvar my-no-internet)             ; from my-settings
(defvar my-do-not-wait-for-dropbox) ; from my-settings

(defvar jrv-jtimelog)   ; from jrv-mypaths
(defvar jrv-htimelog)   ; from jrv-mypaths
(defvar j-org-jrv-arch) ; from jrv-mypaths
(defvar h-org-jrv-arch) ; from jrv-mypaths
(defvar j-org-jrv-file) ; from jrv-mypaths
(defvar h-org-jrv-file) ; from jrv-mypaths
(defvar jrv-jtemp)      ; from jrv-mypaths

(defvar jrv-finish-temp-buffer nil)
(defvar jrv-finish-org-buffer nil)
(defvar jrv-finish-startup-complete nil)
(defvar jrv-finish-after-dropbox-sync-waiting nil)


(defun local-git-copies()
  (interactive)
  (local-git-copy jrv-jtimelog jrv-htimelog 3)
  (local-git-copy j-org-jrv-arch h-org-jrv-arch 3)
  (local-git-copy j-org-jrv-file h-org-jrv-file 3))

(defun split-window-maybe()
  (when (and my-split-window-at-startup (> (window-total-width) 20))
    (ignore-errors
      (delete-other-windows)
      (split-window-horizontally)))
  (> (length (window-list)) 1))

(defun my-std-windows()
  "Show temp (with incoming expanded). If jrv-startup-complete show org-jrv-gpg"
  (interactive)
  (when (and my-open-org-file
             (or my-do-not-wait-for-dropbox jrv-finish-startup-complete))
    (my-org)
    (setq jrv-finish-org-buffer (current-buffer)))
  (when (file-exists-p jrv-jtemp) 
    (dired jrv-jtemp)
    (revert-buffer)
    (setq jrv-finish-temp-buffer (current-buffer))
    (goto-char (point-min))
    (search-forward-regexp (concat "[0-9][0-9]:[0-9][0-9] +" "incoming") nil t) 
    (dired-maybe-insert-subdir "incoming")
    (when (and (split-window-maybe) jrv-finish-org-buffer)
      (set-window-buffer (next-window) jrv-finish-org-buffer))))

(declare-function notmuch-tree "notmuch-tree")
(defun my-mail-windows()
    (notmuch-tree "tag:unread"))

(defun after-dropbox-sync()
  "rerun my-std-windows after dropbox synced"
  ;; run by run-jrv-finish or by using emacsclient --eval
  ;; latter method is used in login-3.py and call-emacs-after-dropbox-sync
  (interactive)
  (cond (jrv-finish-startup-complete
         ;; run only if startup complete
         (setq last-nonmenu-event nil)
         ;; force a dialog box not mini buffer
         (when (yes-or-no-p "Run after-dropbox-sync?")
           (escreen-goto-screen-0)
           (message "running local-git-copies")
           (unless my-no-internet (local-git-copies))
           (my-std-windows)))
        (t
         ;; else run ourselves when startup complete
         (message "Queueing after-dropbox-sync to run at end")
         (setq jrv-finish-after-dropbox-sync-waiting t))))


(defun run-jrv-finish(std mail &optional no-ads)
  "Intended to be called by emacsclient --eval"
  (if (condition-case nil
        (select-frame-by-name "Main Emacs")
        (error t))
      ;; if a frame named "Main Emacs" exists the above code switches to it and returns nil so we do not execute the code below. If not, the above code returns true and we execute run-jrv-finish-main in the code below
      ;; By catching the error that select-frame-by-name throws when it can't find a frame and throwing 't' back (since 'nil' is returned when it DOES find the frame and raises it) I can use it as a condition. See http://emacs.stackexchange.com/questions/19035/finding-frames-by-name
      (run-jrv-finish-main std mail no-ads)))

(defun run-jrv-finish-main (std mail &optional no-ads)
  "Intended to be called by run-jrv-finish"
  (set-frame-name "Main Emacs")
  (when (and std
             my-run-timelog
             (< (nth 2 (decode-time)) 17)
             (not (boundp 'timelog-timer)))
    (defvar timelog-timer (run-at-time "5:00pm" nil 'timelog)))
  (declare-function escreen-install "escreen")
  (declare-function escreen-create-screen "escreen")
  (declare-function escreen-goto-screen-0 "escreen")
  (cond ((and std mail)
         (load "escreen")
         (escreen-install)
         (my-std-windows)
         (escreen-create-screen)
         (my-mail-windows)
         (escreen-goto-screen-0))
        (std
         (my-std-windows))
        (mail
         (my-mail-windows)))
  (setq jrv-finish-startup-complete t)
  (message "JRV startup complete")
  (when (and std
             (not my-do-not-wait-for-dropbox)
             (not no-ads))
    (start-process "dropbox-synced" "*dropbox-wait*"
                   "call-emacs-after-dropbox-sync"))
  (when jrv-finish-after-dropbox-sync-waiting (after-dropbox-sync)))


