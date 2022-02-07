;;; Redacted version of jrv-settings.el
;;; Variable values are masked with *******

(defcustom jrv/settings/allow-pop-up-windows *******
  "*Whether to allow emacs to pop up windows: nil forces reuse of same window (see jrv-basic-customize.el)."
  :type 'boolean
  :group 'JRVarma)

(defcustom jrv/settings/am-on-vacation *******
  "*Whether I am on vacation"
  :type 'boolean
  :group 'JRVarma)

(defcustom jrv/settings/backup-directory *******
  "*Where to save backups (see jrv-basic-customize.el)."
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/buffers-on-all-frames *******
  "*regex for buffers to be shown on both standard and notmuch frames (see jrv-buffer-functions.el)"
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/cursor-color *******
  "*The color of the cursor to be used"
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/dictionary *******
  "*Path to the dictionary (wordlist) to be used for spell check (see jrv-company.el)"
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/dont-ignore-star-buffers *******
  "*regex for starred buffers not to be ignored by ido-ignore-buffers (see jrv-buffer-functions.el)"
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/dont-kill-buffers *******
  "*regex for buffers not to be killed by kill-star-buffers (see jrv-buffer-functions.el)"
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/email-addresses *******
  "*email addresses and aliases in order of priority"
  :type 'list
  :group 'JRVarma)

(defcustom jrv/settings/email-real-name *******
  "*Real name to use in From: email address"
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/email-signature-file *******
  "*File containing email signature (see jrv-notmuch.el)."
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/get-mails-option *******
  "*Option (first argument) to get-my-mails: 'silent' or '' (see jrv-notmuch.el)"
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/get-mails-repeat-minutes *******
  "*Number of minutes after which to repeat get-my-mails (see jrv-notmuch.el)."
  :type 'integer
  :group 'JRVarma)

(defcustom jrv/settings/god-cursor-color *******
  "*The color of the cursor to be used in god mode"
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/god-mode-key *******
  "* Locations of the folder where symlinks to all my paths are stored"
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/hostname *******
  "*Hostname environment variable or as returned by the O/S"
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/is-Android-App *******
  "*Whether this is Android Linux App"
  :type 'boolean
  :group 'JRVarma)

(defcustom jrv/settings/maximize-at-startup *******
  "*Whether to maximize emacs at startup (see jrv-basic-customize.el)."
  :type 'boolean
  :group 'JRVarma)

(defcustom jrv/settings/message-auto-save-directory *******
  "*Folder for Message auto-saves (see jrv-notmuch.el)."
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/mm-default-directory *******
  "*Folder for saving MIME attachments (see jrv-notmuch.el)."
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/no-internet *******
  "*Whether running with internet disabled"
  :type 'boolean
  :group 'JRVarma)

(defcustom jrv/settings/notmuch-fcc-dirs *******
  "*FCC setting (notmuch-fcc-dirs): Folders based on from address"
  :type 'list
  :group 'JRVarma)

(defcustom jrv/settings/run-get-mails *******
  "*Whether to run get-my-mails on startup and repeatedly thereafter (see jrv-notmuch.el)."
  :type 'boolean
  :group 'JRVarma)

(defcustom jrv/settings/run-timelog *******
  "*Whether to run jrv timelog at 5 pm every day (see jrv-finish.el)."
  :type 'boolean
  :group 'JRVarma)

(defcustom jrv/settings/savehist-filename *******
  "*Where to save history (see jrv-basic-customize.el)."
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/split-window-at-startup *******
  "*Whether to split windows horizontally at startup (see jrv-finish.el)."
  :type 'boolean
  :group 'JRVarma)

(defcustom jrv/settings/symlinks *******
  "* Locations of the folder where symlinks to all my paths are stored"
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/tap-to-click-toggle *******
  "*Whether to toggle tap-to-click on focus in/out for laptops with touchpad (see jrv-basic-customize.el)."
  :type 'boolean
  :group 'JRVarma)

(defcustom jrv/settings/theme-name *******
  "*The name of the theme to be used (see jrv-basic-customize.el)"
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/windup-time *******
  "*When/Whether to run windup every day (see jrv-windup.el)."
  :type 'string
  :group 'JRVarma)

(defcustom jrv/settings/yas-snippets-dir *******
  "*Folder for my yas snippets (see jrv-init.el)."
  :type 'string
  :group 'JRVarma)
