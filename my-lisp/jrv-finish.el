(provide 'jrv-finish)

(eval-and-compile  ;; suppress compiler warnings
  (require 'package)
  (setq package-enable-at-startup nil)
  (package-initialize)
  (add-to-list 'load-path "~/.emacs.d/my-lisp/")
  (require 'jrv-settings)
  (require 'jrv-mypaths)
)

(defvar jrv/finish/std-frame-name "Emacs Std")
(defvar jrv/finish/notmuch-frame-name "Emacs Notmuch")

(declare-function jrv/org/open "jrv-org-functions")
(declare-function notmuch-tree "notmuch")
(declare-function jrv/EOD/timers "jrv-EOD-functions")

(defun jrv/finish/split-window-maybe()
  "Split windows if necessary/possible"
  (when (and jrv/settings/split-window-at-startup
             (> (window-total-width) 20))
    (ignore-errors
      (delete-other-windows)
      (split-window-horizontally)))
  (> (length (window-list)) 1))

(defun jrv/finish/icalcli()
  "run icalcli in its own (background) buffer "
  (interactive)
  (if (get-buffer-process "icalcli")
      (switch-to-buffer "icalcli")
    (async-shell-command  "j ic" "icalcli")))

(defun jrv/finish/std-windows()
  "Show org agenda and folder (dired) buffer in its frame."
  (interactive)
  (jrv/finish/make-frame jrv/finish/std-frame-name)
  ;; run icalcli in its own (background) buffer
  (jrv/finish/icalcli)
  ;; open org agenda and temp folder in foreground buffers
  (let ((jrv/finish/org-buffer nil)
        (folder (concat jrv/settings/symlinks "t")))
    ;; (when my-open-org-file
    (jrv/org/open)
    (setq jrv/finish/org-buffer (current-buffer))
    (when (file-exists-p folder) 
      (dired folder)
      (revert-buffer)
      (dired-maybe-insert-subdir "incoming")
      (when (and (jrv/finish/split-window-maybe) jrv/finish/org-buffer)
        (set-window-buffer (next-window) jrv/finish/org-buffer))))
  (when jrv/settings/windup-time
    (jrv/EOD/timers))
  (when (and jrv/settings/run-timelog
             (< (nth 2 (decode-time)) 17)
             (not (boundp 'jrv/finish/timelog-timer)))
    ;; (eval-and-compile  ;; suppress compiler warnings
    ;;   (require 'jrv-EOD-functions))
    (defvar jrv/finish/timelog-timer
      (run-at-time "5:00pm" nil 'jrv/EOD/timelog)))
  )

(defun jrv/finish/mail-windows()
  "Set up mail windows in its frame"
  (interactive)
  (jrv/finish/make-frame jrv/finish/notmuch-frame-name)
  (if (get-buffer "*notmuch-tree-tag:unread*")
      (switch-to-buffer "*notmuch-tree-tag:unread*")
    (notmuch-tree "tag:unread")
    (when jrv/settings/run-get-mails
      (let ((delay
             (if (file-exists-p "~/get-my-mails.lock") (* 10 60) nil)))
        (when (not (boundp 'jrv/finish/getmail-timer))
          (defvar jrv/finish/getmail-timer
            (run-at-time
             delay
             (* jrv/settings/get-mails-repeat-minutes 60)
             'jrv/notmuch/get-my-mails)))
      ))))

(defun jrv/finish/mail-window-frame-only()
  "Set up mail window frame without doing anything else"
  (interactive)
  (jrv/finish/make-frame jrv/finish/notmuch-frame-name)
  )

(defun jrv/finish/make-my-frames()
  (interactive)
  (jrv/finish/make-frame jrv/finish/std-frame-name)
  (jrv/finish/make-frame jrv/finish/notmuch-frame-name))

(defun jrv/finish/make-frame (name)
  "Return existing, renamed or new frame with specified name"
  (interactive)
  (condition-case err
      (select-frame-by-name name)
    ;; The handler.
    (error
     (let ((this-frame-name
            (alist-get 'name (frame-parameters (selected-frame)))))
       (if (or (string= this-frame-name jrv/finish/std-frame-name)
               (string= this-frame-name jrv/finish/notmuch-frame-name))
           ;; we cannot remame this frame so make new frame
           (make-frame `((name . ,name)))
         (set-frame-name name))
       (select-frame-by-name name)))))
  

(defun jrv/finish/main (&optional no-std no-mail)
  "Set up std and mail windows"
  (interactive)
  (unless no-mail
    (jrv/finish/mail-windows))
  (unless no-std
    (jrv/finish/std-windows)))
