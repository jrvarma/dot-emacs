;;-*- mode: Emacs-Lisp -*-
;;; my-settings.el --- 
;;; these settings are used in various init files
(provide 'my-settings)
(eval-when-compile (require 'subr-x))

(defvar my-hostname (string-trim-right (shell-command-to-string "hostname")))
(defvar my-minimal (not (string-match-p "JRV" my-hostname)))

(defgroup JRVarma nil
    "JRVarma init.el configuration management"
    :group 'emacs)

(defcustom my-theme-name "misterioso"
  "*The name of the theme to be used"
  :type 'string
  :group 'JRVarma)

(defcustom my-cursor-color "#ffff00" ; yellow
  "*The color of the cursor to be used "
  :type 'string
  :group 'JRVarma)

(defcustom my-dictionary "/path/to/my/wordlist"
  "*Path to the dictionary (wordlist) to be used for spell check"
  :type 'string
  :group 'JRVarma)

(defcustom my-do-not-wait-for-dropbox nil
  "*Whether to open my-org file without waiting for dropbox sync to complete."
  :type 'boolean
  :group 'JRVarma)

(defcustom my-run-get-mails (not my-minimal)
  "*Whether to run get-my-mails on startup and repeatedly thereafter."
  :type 'boolean
  :group 'JRVarma)

(defcustom my-get-mails-repeat-minutes 10
  "*Number of minutes after which to repeat get-my-mails."
  :type 'integer
  :group 'JRVarma)

(defcustom my-get-mails-option "silent"
  "*Option (first argument) to get-my-mails ('silent' or '') "
  :type 'string
  :group 'JRVarma)

(defcustom my-open-org-file (not my-minimal)
  "*Whether to open my-org-file  at startup."
  :type 'boolean
  :group 'JRVarma)

(defcustom my-timelog-categories-file "/path/to/timelog-categories"
  "*If non nil file from which to insert timelog categories into my-org-file at startup."
  :type 'string
  :group 'JRVarma)

(defcustom my-split-window-at-startup (not my-minimal)
  "*Whether to split windows horizontally at startup."
  :type 'boolean
  :group 'JRVarma)

(defcustom my-maximize-at-startup nil
  "*Whether to maximize emacs at startup."
  :type 'boolean
  :group 'JRVarma)

(defcustom my-pop-up-windows t
  "*Whether to allow emacs to pop up windows (nil forces reuse of same window)."
  :type 'boolean
  :group 'JRVarma)

(defcustom my-backup-directory "/path/to/my/.emacs.saves"
  "*Where to save backups (backup-directory-alist)."
  :type 'string
  :group 'JRVarma)

(defcustom my-tap-to-click-toggle nil
  "*Whether to toggle tap-to-click on focus in/out (for laptops with touchpad)."
  :type 'boolean
  :group 'JRVarma)

(defcustom my-dont-kill-buffers "\\*\\(scratch\\|Messages\\|notmuch\\)"
  "*regex for buffers not to be killed by kill-star-buffers"
  :type 'string
  :group 'JRVarma)

(defcustom my-buffers-on-all-screens "\\*\\(scratch\\|Messages\\)"
  "*regex for buffers to be shown on both standard and notmuch screens"
  :type 'string
  :group 'JRVarma)

(defcustom my-dont-ignore-star-buffers "\\*\\(magit:\\|Locate\\|info\\)"
  "*regex for starred buffers not to be ignored by ido-ignore-buffers"
  :type 'string
  :group 'JRVarma)

