;;; my-settings.el --- 
;;; This file is used to configure my init files
;;; Sets theme, color and other settings
;;; Determine what functions are run
;;; Set file and folder locations

(provide 'my-settings)
(eval-when-compile (require 'subr-x))

(defvar my-hostname (string-trim-right (shell-command-to-string "hostname")))
(defvar my-minimal (not (string-match-p "JRV" my-hostname)))
(defvar my-home-pc (string-equal "JRVLAPTOP" my-hostname))
(defvar my-office-pc (string-equal "JRVARMAPC" my-hostname))
(defvar my-no-internet (file-exists-p "~/no-internet"))
(defvar my-on-vacation (file-exists-p "~/on-vacation"))

(defgroup JRVarma nil
    "JRVarma init.el configuration management"
    :group 'emacs)

;; basic customization

(defcustom my-theme-name "misterioso"
  "*The name of the theme to be used (see jrv-basic-customize.el)"
  :type 'string
  :group 'JRVarma)

(defcustom my-cursor-color "#ffff00" ; yellow
  "*The color of the cursor to be used (see jrv-basic-customize.el)"
  :type 'string
  :group 'JRVarma)

(defcustom my-maximize-at-startup nil
  "*Whether to maximize emacs at startup (see jrv-basic-customize.el)."
  :type 'boolean
  :group 'JRVarma)

(defcustom my-pop-up-windows t
  "*Whether to allow emacs to pop up windows: nil forces reuse of same window (see jrv-basic-customize.el)."
  :type 'boolean
  :group 'JRVarma)

(defcustom my-tap-to-click-toggle my-home-pc
  "*Whether to toggle tap-to-click on focus in/out for laptops with touchpad (see jrv-basic-customize.el)."
  :type 'boolean
  :group 'JRVarma)

;; buffer visibility and protection

(defcustom my-dont-kill-buffers "\\*\\(scratch\\|Messages\\|notmuch\\)"
  "*regex for buffers not to be killed by kill-star-buffers (see jrv-buffer-functions.el)"
  :type 'string
  :group 'JRVarma)

(defcustom my-buffers-on-all-screens "\\*\\(scratch\\|Messages\\)"
  "*regex for buffers to be shown on both standard and notmuch screens (see jrv-buffer-functions.el)"
  :type 'string
  :group 'JRVarma)

(defcustom my-dont-ignore-star-buffers "\\*\\(magit:\\|Locate\\|info\\)"
  "*regex for starred buffers not to be ignored by ido-ignore-buffers (see jrv-buffer-functions.el)"
  :type 'string
  :group 'JRVarma)

;; email (notmuch) settings

(defcustom my-run-get-mails (not my-minimal)
  "*Whether to run get-my-mails on startup and repeatedly thereafter (see jrv-notmuch.el)."
  :type 'boolean
  :group 'JRVarma)

(defcustom my-get-mails-repeat-minutes
  (if my-on-vacation 300 10)
  "*Number of minutes after which to repeat get-my-mails (see jrv-notmuch.el)."
  :type 'integer
  :group 'JRVarma)

(defcustom my-get-mails-option
  "silent"
  "*Option (first argument) to get-my-mails: 'silent' or '' (see jrv-notmuch.el)"
  :type 'string
  :group 'JRVarma)

;; some idiosyncratic stuff

(defcustom my-run-timelog my-office-pc
  "*Whether to run jrv timelog at 5 pm every day (see jrv-finish.el)."
  :type 'boolean
  :group 'JRVarma)

(defcustom my-windup-time
  (if (or my-minimal my-on-vacation)
      nil
    (if my-home-pc "21:00" "17:45"))
  "*When/Whether to run windup every day (see jrv-windup.el)."
  :type 'string
  :group 'JRVarma)

(defcustom my-do-not-wait-for-dropbox my-on-vacation
  "*Whether to open my-org file without waiting for dropbox sync to complete (see jrv-finish.el)."
  :type 'boolean
  :group 'JRVarma)

(defcustom my-open-org-file (not my-minimal)
  "*Whether to open my-org-file  at startup (see jrv-finish.el)."
  :type 'boolean
  :group 'JRVarma)

(defcustom my-split-window-at-startup (not my-minimal)
  "*Whether to split windows horizontally at startup (see jrv-finish.el)."
  :type 'boolean
  :group 'JRVarma)

;; file and folder locations

(defcustom my-dictionary
  (file-truename "~/.emacs.d/scowl-wordlist.txt")
  "*Path to the dictionary (wordlist) to be used for spell check (see jrv-company.el)"
  :type 'string
  :group 'JRVarma)

(defcustom my-timelog-categories-file
  (if my-office-pc
      (file-truename "~/.emacs.d/site-lisp/jrv/timelog-categories")
    nil)
  "*If non nil file from which to insert timelog categories into my-org-file at startup (see jrv-org.el)."
  :type 'string
  :group 'JRVarma)

(defcustom my-backup-directory
  (if my-minimal (file-truename "~/.emacs.saves")
    (file-truename "~/0/t/.emacs.saves"))
  "*Where to save backups (see jrv-basic-customize.el)."
  :type 'string
  :group 'JRVarma)

(defcustom my-savehist-file
  (file-truename "~/.emacs.d/savehist")
  "*Where to save history (see jrv-basic-customize.el)."
  :type 'string
  :group 'JRVarma)

(defcustom my-email-signature-file
  (file-truename "~/0/dj/Signature_New.txt")
  "*File containing email signature (see jrv-notmuch.el)."
  :type 'string
  :group 'JRVarma)

(defcustom my-latex-help-file
  (file-truename "~/0/m/TeX/lshort.pdf")
  "*Path to LaTex help file (see jrv-auctex-config.el)."
  :type 'string
  :group 'JRVarma)

(defcustom my-offline-python-help-file
  (file-truename "~/python-doc-dir/index.html")
  "*Path to Python help file (see jrv-python-elpy.el)."
  :type 'string
  :group 'JRVarma)

(defcustom my-message-auto-save-directory
  (file-truename "~/0/JE/drafts/")
  "*Folder for Message auto-saves (see jrv-notmuch.el)."
  :type 'string
  :group 'JRVarma)

(defcustom my-mm-default-directory
  (file-truename "~/0/t/")
  "*Folder for saving MIME attachments (see jrv-notmuch.el)."
  :type 'string
  :group 'JRVarma)

(defcustom my-yas-snippets-dir
  (file-truename "~/.emacs.d/snippets")
  "*Folder for my yas snippets (see jrv-init.el)."
  :type 'string
  :group 'JRVarma)

;; override many settings when running without internet
(when my-no-internet 
  (setq my-run-timelog nil)
  (setq my-do-not-wait-for-dropbox t)
  (setq my-run-get-mails nil)
  (setq my-open-org-file nil)
  (setq my-timelog-categories-file nil))
