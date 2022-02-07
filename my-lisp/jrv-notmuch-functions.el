(provide 'jrv-notmuch-functions)
(eval-and-compile  ;; suppress compiler warnings
  (require 'package)
  (setq package-enable-at-startup nil)
  (package-initialize)
  (add-to-list 'load-path "~/.emacs.d/my-lisp/")
  (require 'jrv-mypaths)
  (require 'jrv-settings)
  (require 'notmuch)
  (require 'gnus)
  (require 'message)
  (require 'seq)
  (require 'ol)
  (require 'ol-notmuch)
)

(autoload 'w3m-anchor "w3m-util" nil t t)
(autoload 'w3m-image "w3m-util" nil t t)

(define-minor-mode
  jrv/notmuch/jrvrss
  "Change modeline when viewing feed emails"
  nil
  #("jrvrss" 0 5 (face (background-color . "red"))))

(defun jrv/notmuch/config ()
  "Choose to display main mails or rss feed mails"
  (interactive)
  (let ((choices '("notmuch-jrvrss" "notmuch")))
    ;; notmuch-jrvrss is a shell script that runs notmuch
    ;; with a non standard config file (and different maildir folder)
    ;; notmuch is the standard notmuch executable
    (setq notmuch-command
          (ido-completing-read
           (format "Change notmuch-config from %s to: " notmuch-command)
           choices )))
  ;; we use the jrv/notmuch/jrvrss modeline for feed emails
  ;; Add or remove this mode from the notmuch-tree-mode-hook
  (if (string-equal notmuch-command "notmuch")
      (remove-hook 'notmuch-tree-mode-hook
                   '(lambda () (jrv/notmuch/jrvrss 1)))
    (add-hook 'notmuch-tree-mode-hook
              '(lambda () (jrv/notmuch/jrvrss 1))))
  ;; refresh existing notmuch-tree-mode buffers
  ;; activate/deactivate jrv/notmuch/jrvrss modeline
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (equal major-mode 'notmuch-tree-mode)
        (notmuch-refresh-this-buffer)
        (jrv/notmuch/jrvrss
         (if (string-equal notmuch-command "notmuch") -1 1))))))

(defun jrv/notmuch/feed-msmtp ()
  "Use from address to set smtp sender account"
  ;; core adapted from
  ;; http://www.emacswiki.org/cgi-bin/wiki/GnusMSMTP#toc2
  ;; Choose account label to feed msmtp -a option
  ;; based on From header in Message buffer;
  ;; This function must be added to message-send-mail-hook
  (if (message-mail-p)
      (let* ((from (jrv/notmuch/fetch-header (list "from")))
             (to (jrv/notmuch/fetch-header (list "to" "cc" "bcc")))
               (account
                (seq-find
                 (lambda (x) (string-match (elt x 1) from))
                 jrv/settings/email-addresses nil)))
        (if account
            (setq message-sendmail-extra-arguments
                  (list '"-a" (elt account 1)))
          (error "Invalid sender address %s" from))
        (jrv/notmuch/check-sender-account-ok from to))))

(defun jrv/notmuch/fetch-header (header-list)
  "Returns all headers in header-list as a single string"
  (interactive)
  (save-excursion
    (save-restriction
      (message-narrow-to-headers-or-head)
      (mapconcat (lambda (hdr) (message-fetch-field hdr)) header-list " "))))

(defun jrv/notmuch/confirm-sender-account (msg)
  "Confirm sender account if it appears inconsistent with to"
  (setq last-nonmenu-event nil)
  ;; force a dialog box not mini buffer
  (when (not (yes-or-no-p (format-message "%s. Confirm (y/n) " msg)))
    (error "Please change sender address and try again")))

(defvar jrv/notmuch/official-email-address
  (elt (seq-find
        (lambda (x) (string-match (elt x 0) "i"))
        jrv/settings/email-addresses nil) 1))

(defvar jrv/notmuch/official-email-address-domain
  (elt (split-string jrv/notmuch/official-email-address "@") 1))

(defvar jrv/notmuch/personal-email-address
  (elt (seq-find
        (lambda (x) (string-match (elt x 0) "g"))
        jrv/settings/email-addresses nil) 1))

(defvar jrv/notmuch/vacation-email-address
  (elt (seq-find
        (lambda (x) (string-match (elt x 0) "v"))
        jrv/settings/email-addresses nil) 1))

(defun jrv/notmuch/check-sender-account-ok (from to)
  "Apply various checks to ensure sender account consistent with recipient"
   (when (and (string-match jrv/notmuch/official-email-address from)
              (not (string-match
                    jrv/notmuch/official-email-address-domain to)))
     (jrv/notmuch/confirm-sender-account
      "From official to non official"))
   (when (and (not jrv/settings/am-on-vacation) jrv/notmuch/vacation-email-address
              (string-match jrv/notmuch/vacation-email-address from))
     (jrv/notmuch/confirm-sender-account
      "From vacation while not on vacation."))
   (when (and (string-match
               jrv/notmuch/official-email-address-domain to)
              (string-match jrv/notmuch/personal-email-address from))
     (jrv/notmuch/confirm-sender-account
      "From personal to official"))
   (when (and jrv/settings/am-on-vacation
              (or (not jrv/notmuch/vacation-email-address)
                  (not (string-match
                        jrv/notmuch/vacation-email-address from))))
     (jrv/notmuch/confirm-sender-account
      "Not from vacation while on vacation"))
   (when (not (string-equal notmuch-command "notmuch"))
     (jrv/notmuch/confirm-sender-account
      "Trying to send mail from rss feed account")))


(defun jrv/notmuch/rss-url-browse()
  "View last http link in browser (rss-emacs)" 
  (interactive)
  (let ((this-window (selected-window)))
    (notmuch-tree-show-message nil)
    (select-window notmuch-tree-message-window)
    (goto-char (point-max))
    (search-backward-regexp "URL:\\s-*http") (forward-char 6)
    (message "Loading in browser")
    (browse-url-at-point)
    (select-window this-window)))

(defun jrv/notmuch/open-link ()
  "Opens the current link or image or current page's uri or any url-like text under cursor in external browser."
  ;; http://blog.binchen.org/posts/open-url-in-emacs-with-external-browser.html
  (interactive)
  (let ((url (or (w3m-anchor) (w3m-image))))
    (if url
        (browse-url url)
      (browse-url (thing-at-point 'url 'no-properties)))))

(defun jrv/notmuch/get-my-mails () 
  "Get my mails" 
  (interactive)
  (start-process "get-my-mails"  "*notmuch-get-my-mails*" "get-my-mails"
                 jrv/settings/get-mails-option))

(defun jrv/notmuch/forward-inline()
  "Forward the current message inline." 
  (interactive) 
  (notmuch-tree-close-message-window)
  (let ((original-message-forward-as-mime message-forward-as-mime))
    (setq message-forward-as-mime nil)
    (call-interactively (function notmuch-show-forward-message))
    (setq message-forward-as-mime original-message-forward-as-mime)
    ))

(defun jrv/notmuch/call-mimedown (preview switches)
  "Create html alternative mime-part. mimedown is a shell script"
  (let ((command
         (concat "mimedown " switches (if preview " --preview" ""))))
    (message command)
    (message (if (not preview) "t" "nil"))
    (save-excursion
      (message-goto-body)
      (shell-command-on-region
       (point) (point-max) command nil (not preview)))))

(defun jrv/notmuch/mimedown-ask ()
  "Let user choose to Preview/Send email in Plain Text or HTML format"
  (interactive)
  (let ((choice (read-key-sequence "Choose: {1 Preview Emacs | 2 Preview External | 3 Send Plain | 4 Send HTML}")))
    (if
        (cond
         ((string-equal choice "1")
          (jrv/notmuch/call-mimedown t "--w3m") t)
         ((string-equal choice "2")
          (jrv/notmuch/call-mimedown t "") t)
         ((string-equal choice "3")
          nil)
         ((string-equal choice "4")
          (jrv/notmuch/call-mimedown nil "") nil)
         (t (error "Invalid choice")))
        (error "Sending aborted by preview"))
    (notmuch-mua-send-and-exit)))
    ;; (error "Sending aborted for testing")))

(defun jrv/notmuch/sylpheed ()
  "Open current message in sylpheed email client"
  (interactive)
  (start-process
   "sylpheed"
   "*sylpheed*"
   "bash"
   "-c"
   (format "sylpheed --open `notmuch search --output=files %s`"
           (notmuch-show-get-message-id))))

(defun jrv/notmuch/de-fill ()
  "Remove filling from paragraph or region (mainly in message mode)"
  (interactive)
  (let ((old-fill-column fill-column))
	(set-fill-column most-positive-fixnum)
    (if (use-region-p)
        (fill-region (region-beginning) (region-end))
      (fill-paragraph))
	(set-fill-column old-fill-column))) 

;; redefine notmuch-tree-show-message-in to split window vertically
(defun jrv/notmuch/tree-show-message-in ()
  "Show the current message (in split-pane)."
  (interactive)
  (let ((id (notmuch-tree-get-message-id))
	(inhibit-read-only t)
	buffer)
    (when id
      ;; We close and reopen the window to kill off un-needed buffers
      ;; this might cause flickering but seems ok.
      (notmuch-tree-close-message-window)
      (setq notmuch-tree-message-window
	    ;; Split message pane horizontally in wide windows
	    ;; In narrow windows, split pane horizontally 1:3
	    (if (> (window-total-width) 110)
		(split-window-horizontally)
	      (split-window-vertically (/ (window-height) 4))))
      (with-selected-window notmuch-tree-message-window
	;; Since we are only displaying one message do not indent.
	(let ((notmuch-show-indent-messages-width 0)
	      (notmuch-show-only-matching-messages t))
	  (setq buffer (notmuch-show id))))
      ;; We need the `let' as notmuch-tree-message-window is buffer local.
      (let ((window notmuch-tree-message-window))
	(with-current-buffer buffer
	  (setq notmuch-tree-message-window window)
	  (add-hook 'kill-buffer-hook 'notmuch-tree-message-window-kill-hook)))
      (when notmuch-show-mark-read-tags
	(notmuch-tree-tag-update-display notmuch-show-mark-read-tags))
      (setq notmuch-tree-message-buffer buffer))))

(fset #'notmuch-tree-show-message-in
      #'jrv/notmuch/tree-show-message-in)

(defun jrv/notmuch/choose-plain-or-html (html)
  "Display plain text or (with prefix argument) html"
  (interactive "P")
  (if html
      (setq notmuch-multipart/alternative-discouraged
            '("text/plain" "text/html"))
    (setq notmuch-multipart/alternative-discouraged nil)))

(defun jrv/notmuch/org-follow-link (search)
  "Customized function to follow link in org file to notmuch message

1. Switch to the notmuch frame. 
2. Open tree view for the thread"
  (require 'notmuch)
  (select-frame-by-name "Emacs Notmuch")
  (notmuch-show (org-link-unescape search))
  (notmuch-tree-from-show-current-query))

(customize-set-variable 'org-notmuch-open-function 'jrv/notmuch/org-follow-link)
;; disable org-notmuch-tree-store-link
;; this avoids having to choose store function each time
(org-link-set-parameters "notmuch-tree"
			 :follow #'org-notmuch-tree-open
			 ;; :store #'org-notmuch-tree-store-link)
			 :store nil)
