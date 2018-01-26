(provide 'jrv-notmuch)
(require 'notmuch)
(require 'my-settings)
(eval-when-compile
  (require 'my-settings)
  (require 'notmuch)
  (require 'gnus)
  (require 'message))

(defvar gnus-alias-identity-alist)
(defvar gnus-alias-default-identity)
(defvar gnus-alias-identity-rules)
(defvar gnus-inhibit-images)
(defvar sendmail-program)
(defvar fill-flowed-display-column)
(declare-function gnus-alias-use-identity "gnu-alias")

(setq notmuch-search-oldest-first nil)
(setq mail-signature-file my-email-signature-file) ;; for sendmail
(setq notmuch-crypto-process-mime t)

(require 'notmuch-address)
(setq notmuch-address-command "my-nottoomuch-addresses.sh")

(add-hook 'message-mode-hook '(lambda () (mail-abbrevs-mode t)))
(add-hook
 'message-mode-hook
 '(lambda () 
    (make-local-variable 'company-backends)
    (setq company-backends '((:separate company-dabbrev company-ispell)))
    (setq-local company-idle-delay 0)))
(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "On %a, %b %d %Y at %r, %f wrote:")

(setq mailcap-mime-data '(("*" (".*" (viewer . "xdg-open \"%s\"") (type . "*/*")))))
(mailcap-parse-mailcaps)

(setq mm-text-html-renderer 'w3m)

(setq message-auto-save-directory my-message-auto-save-directory)
(setq mm-default-directory my-mm-default-directory)
(setq message-send-mail-partially-limit nil)
(setq gnus-inhibit-images t)

(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'cg-feed-msmtp)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "~/bin/pymsmtpq") 
(add-hook 'message-sent-hook
          (lambda ()
            (start-process "pymsmtpq-flush" "*notmuch-pymsmtpq*" "pymsmtpq" "--manage" "s")))
(setq fill-flowed-display-column nil)
(add-hook 'message-send-hook
  (lambda ()
    (when use-hard-newlines
      (harden-newlines))))

(define-key notmuch-tree-mode-map [(control return)]
  '(lambda ()
     "Show current message in message pane and switch to message pane"
     (interactive)
     (notmuch-tree-show-message-in)
     (select-window notmuch-tree-message-window)))

(define-key notmuch-tree-mode-map "g" 'get-my-mails)
(define-key notmuch-search-mode-map "F" 'forward-inline)
(define-key notmuch-tree-mode-map "F" 'forward-inline)

(setq message-forward-as-mime t)

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
(setq gnus-alias-identity-alist
      ;; the backquote-comma combination forces evaluation of my-email-signature-file
      `(("ji" ;; iima identity
         nil ;; Does not refer to any other identity
         "Prof Jayanth R Varma <___@___.___> " ;; Sender address
         nil ;; No organization header
         nil ;; No extra headers
         nil ;; No extra body text
         ,my-email-signature-file)
        ("jg"  ;; gmail identity
         nil
         "Prof Jayanth R Varma <___@___.___> "
         nil ;; No organization header
         nil ;; No extra headers
         nil ;; No extra body text
         ,my-email-signature-file)))
;; default identity
(setq gnus-alias-default-identity "ji")
;; Define rules to match gmail identity
(setq gnus-alias-identity-rules
      '(("jg" ("any" "___@___.___" both) "jg")
        ;; ("cf" ("any" "chr-fin@iima.ac.in" both) "cf")
        ))


;;;;;;;;;;;;; functions 


(add-hook 'message-mode-hook 'my-message-mode-hook)

;; adapted from http://www.emacswiki.org/cgi-bin/wiki/GnusMSMTP#toc2
;; Choose account label to feed msmtp -a option based on From header in Message buffer;
;; This function must be added to message-send-mail-hook for on-the-fly change of From address
;; before sending message since message-send-mail-hook is processed right before sending message.
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
               (iima "___@___.___")
               (iimahd "___@___.___")
               (gmail "___@___.___")
               ;; (chr-fin-old "chr-fin@iimahd.ernet.in")
               ;; (chr-fin "chr-fin@iima.ac.in")
               (account 
                (cond
                 ((string-match iima from) iima)
                 ((string-match iimahd from) iima)
                 ((string-match gmail from) gmail)
                 )))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(defun vwh()
  "View HTML part using vwh" 
  (interactive)
  (message "Loading in browser using vwh")
  (notmuch-show-pipe-message nil "vwh"))

(define-key notmuch-tree-mode-map "]" 'vwh)
(define-key notmuch-show-mode-map "]" 'vwh)
  
(defvar my-get-mails-option)  ; from my-settings
(defun get-my-mails () 
  "Get my mails" 
  (interactive)
  (start-process "get-my-mails"  "*notmuch-get-my-mails*" "get-my-mails" my-get-mails-option))

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

(define-key notmuch-show-mode-map "\C-c\C-o" 'goto-address-at-point)
(define-key notmuch-tree-mode-map "!"
  (lambda ()
    "toggle unread tag for message"
    (interactive)
    (if (member "unread" (notmuch-tree-get-tags))
        (notmuch-tree-tag '("-unread"))
      (notmuch-tree-tag '("+unread")))))

(setq notmuch-saved-searches '(
        (:name "unread" :query "tag:unread" :key "u" :search-type tree)
        (:name "sent" :query "tag:sent" :key "t" :search-type tree)
        (:name "drafts" :query "tag:draft" :key "d" :search-type tree)
        (:name "1 days" :query "date:1D.." :key "1" :search-type tree)
        (:name "2 days" :query "date:2D.." :key "2" :search-type tree)
        (:name "5 days" :query "date:5D.." :key "5" :search-type tree)))

;; redefine notmuch-tree-show-message-in to split window vertically
(defun notmuch-tree-show-message-in ()
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
	    ;; Split message pane vertically in wide windows (width > 160)
	    ;; Under RFC 2045 line length is 76 characters so plain text emails
	    ;; will be displayed properly in 80 character wide pane
	    ;; HTML mail will anyway reformat to pane width
	    ;; In narrow windows, split pane horizontally 1:3
	    (if (> (window-total-width) 160)
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
