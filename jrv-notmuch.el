(provide 'jrv-notmuch)
(require 'notmuch)
(require 'notmuch-address)
(require 'my-settings)

(defvar notmuch-search-oldest-first)
(setq notmuch-search-oldest-first nil)
(defvar mail-signature-file)
(setq mail-signature-file "/path/to/signature/file")
(defvar notmuch-crypto-process-mime)
(setq notmuch-crypto-process-mime t)

(add-hook 'message-mode-hook '(lambda () (mail-abbrevs-mode t)))
(add-hook
 'message-mode-hook
 '(lambda () 
    (make-local-variable 'company-backends)
    (setq company-backends '((:separate company-dabbrev company-ispell)))
    (setq-local company-idle-delay 0)))
(defvar message-citation-line-function)
(defvar message-citation-line-format "On %a, %b %d %Y at %r, %f wrote:")
(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "On %a, %b %d %Y at %r, %f wrote:")

(setq mailcap-mime-data '(("*" (".*" (viewer . "xdg-open \"%s\"") (type . "*/*")))))
(mailcap-parse-mailcaps)

(defvar mm-text-html-renderer)
(setq mm-text-html-renderer 'w3m)

(defvar message-auto-save-directory)
(setq message-auto-save-directory "/path/to/drafts/")
(defvar mm-default-directory)
(setq mm-default-directory "/path/to/default/")
(defvar message-send-mail-partially-limit)
(setq message-send-mail-partially-limit nil)
(defvar gnus-inhibit-images)
(setq gnus-inhibit-images t)

(defvar message-sendmail-envelope-from)
(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'cg-feed-msmtp)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
;; use pymsmtpq (https://github.com/sbfnk/pymsmtpq) to queue and send mail in background
(defvar sendmail-program)
(setq sendmail-program "~/bin/pymsmtpq") 
(add-hook 'message-sent-hook
          (lambda ()
            (start-process "pymsmtpq-flush" "*notmuch-pymsmtpq*" "pymsmtpq" "--manage" "s")))
(defvar fill-flowed-display-column)
(setq fill-flowed-display-column nil)
(add-hook 'message-send-hook
  (lambda ()
    (when use-hard-newlines
      (harden-newlines))))

(define-key notmuch-tree-mode-map "g" 'get-my-mails)
(define-key notmuch-search-mode-map "F" 'forward-inline)
(define-key notmuch-tree-mode-map "F" 'forward-inline)

(defvar message-forward-as-mime)
(setq message-forward-as-mime t)

(defvar my-run-get-mails)             ; from my-settings
(defvar my-get-mails-repeat-minutes)  ; from my-settings
(when my-run-get-mails
  (if (file-exists-p "~/get-my-mails.lock")
      (run-at-time (* 10 60) (* my-get-mails-repeat-minutes 60) 'get-my-mails)
    (run-at-time nil (* my-get-mails-repeat-minutes 60) 'get-my-mails)))


;;;;;;;;;;;;;;;;;;;;;;;;;    gnu-alias

;; autoload gnu-alias
(autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
;; Determine identity when message-mode loads
(add-hook 'message-setup-hook 'gnus-alias-determine-identity)
;; set up the identities
(defvar gnus-alias-identity-alist)
(setq gnus-alias-identity-alist
      '(() () ... ()));; personal email information redacted
;; default identity
(defvar gnus-alias-default-identity)
(setq gnus-alias-default-identity "");; personal email information redacted
;; Define rules to match gmail identity
(defvar gnus-alias-identity-rules) 
(setq gnus-alias-identity-rules
      '(() ())) ;; personal email information redacted


;;;;;;;;;;;;; functions 


(defun days-seconds (n)
  "Convert days to seconds"
  (let* ((offset (seconds-to-time (* n 24 60 60)))
         (p (decode-time (time-subtract (current-time) offset))))
         (format-time-string "%s" 
                                (encode-time 0 0 0 (nth 3 p) (nth 4 p) (nth 5 p)))))

(declare-function notmuch-tree "notmuch-tree")
(defun notmuch-yesterday (n)
  "Display mails of last n days in tree buffer"
  (interactive "p")
  (notmuch-tree (concat (days-seconds n) ".." ) nil nil 
                (concat "notmuch-tree-" (number-to-string n) "-day"))) 

(declare-function notmuch-search "notmuch-search")
(defun notmuch-search-yesterday (n)
  "Display mails of last n days in search buffer"
  (interactive "p")
  (notmuch-search (concat (days-seconds n) ".." )))

(add-hook 'message-mode-hook 'my-message-mode-hook)

;; adapted from http://www.emacswiki.org/cgi-bin/wiki/GnusMSMTP#toc2
;; Choose account label to feed msmtp -a option based on From header in Message buffer;
;; This function must be added to message-send-mail-hook for on-the-fly change of From address
;; before sending message since message-send-mail-hook is processed right before sending message.
(declare-function message-mail-p "message")
(declare-function message-narrow-to-headers "message")
(declare-function message-fetch-field "message")
(declare-function message-goto-body "message")
(defvar message-sendmail-extra-arguments)
(defun cg-feed-msmtp ()
  "Use from address to set smtp sender account"
  (if (message-mail-p)
      (save-excursion
        (let* ((from 
                (save-restriction
                  (message-narrow-to-headers)
                  (message-fetch-field "from")))
               (to
                (save-restriction
                  (message-narrow-to-headers)
                  (message-fetch-field "to")))
               (iima "");; personal email information redacted
               (gmail "");; personal email information redacted
               (account 
                (cond
                 ((string-match iima from) iima)
                 ((string-match gmail from) gmail)
                 )))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(defun notmuch-tree-unread ()
    "show unread messages in tree"
    (interactive)
    (notmuch-tree "tag:unread"))

(declare-function notmuch-show-pipe-message "notmuch-show")
(define-key notmuch-tree-mode-map "]"
  (lambda () "View HTML part using vwh" 
    (interactive) (notmuch-show-pipe-message nil "vwh")))

(defvar my-get-mails-option)  ; from my-settings
(defun get-my-mails () 
  "Get my mails. Calls shell script get-my-mails to run offlineimap" 
  (interactive)
  (start-process "get-my-mails"  "*notmuch-get-my-mails*" "get-my-mails" my-get-mails-option))

(declare-function notmuch-tree-close-message-window "notmuch-tree")
(defun forward-inline()
  "Forward the current message inline." 
  (interactive) 
  (notmuch-tree-close-message-window)
  (let ((original-message-forward-as-mime message-forward-as-mime))
    (setq message-forward-as-mime nil)
    (call-interactively (function notmuch-show-forward-message))
    (setq message-forward-as-mime original-message-forward-as-mime)
    ))

(defun mimedown ()
  "Run mimedown on the region to create html alternative mime-part." 
  (interactive)
  (let ((original-shell-command-switch shell-command-switch))
    (setq shell-command-switch "-c")
    (if (use-region-p)
        (shell-command-on-region (region-beginning) (region-end) "mimedown" nil t)
      (save-excursion
        (message-goto-body)
        (shell-command-on-region (point) (point-max) "mimedown" nil t)))
    (setq shell-command-switch original-shell-command-switch)
    ))


;; message-mode key bindings, de-fill, word-wrap, flyspell
(declare-function gnus-alias-use-identity "gnu-alias")
(defun my-message-mode-hook ()
  "Hook to define keys, set wrap, autofill etc in message mode"
  (define-key notmuch-message-mode-map [(control ?c) (?i)]
    (function
     (lambda () "Set Identity to iima." (interactive)
       (gnus-alias-use-identity "ji"))))
  (define-key notmuch-message-mode-map [(control ?c) (?g)]
    (function
     (lambda () "Set Identity to gmail." (interactive)
       (gnus-alias-use-identity "jg"))))
  (define-key notmuch-message-mode-map (kbd "M-q") 'de-fill)
  (turn-off-auto-fill)
  (setq
   truncate-lines nil
   word-wrap t
   use-hard-newlines t)
  (flyspell-mode)
  )

;; format-flowed by default
;; http://article.gmane.org/gmane.emacs.gnus.user/14508
(defun harden-newlines ()
  "Use hard newlines in outgoing messages"
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (put-text-property (1- (point)) (point) 'hard t))))

;; some code redacted as useless for others
