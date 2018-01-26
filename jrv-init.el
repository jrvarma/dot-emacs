;;;; This is the main init file
;;;; which is loaded by init.el

(provide 'jrv-init)
(eval-when-compile (require 'subr-x))

(eval-when-compile 
  (require 'package)
  (package-initialize)
)

(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'my-settings)                         ; lots of things can be customized here
(defvar my-minimal)                            ; from my-settings
;; Sometimes a minimal emacs init file is desired, e.g., when running emacs in a phone or a server.
;; In this situation, my-settings.el sets my-minimal to t (or any other non nil)
;; Then, only those packages are loaded that are found in the system
;; When 'require' is called with its fourth argument non nil, no error is signalled if the
;; package could not be loaded.
;; In my main machine, I do want to know if some package did not load
;; So I set my-minimal to nil on these machines in my-settings.el using hostname 

(require 'jrv-basic-customize nil my-minimal)  ; theme, history, font-lock
(require 'jrv-key-alias nil my-minimal)        ; my keybindings, aliases
(require 'jrv-mypaths nil my-minimal)          ; my short paths
(require 'jrv-company nil my-minimal)          ; Set up company-mode config
(require 'jrv-buffer-functions nil my-minimal) ; Switch/Delete buffer/window 
(require 'jrv-text-functions nil my-minimal)   ; spell, wc, proper-case etc
(require 'jrv-ibuffer nil my-minimal)          ; ibuffer configuration
(require 'jrv-auctex-config nil my-minimal)    ; Auctex configuration
(require 'dired-x nil my-minimal)              ; extensions to dired
(require 'dired+ nil my-minimal)               ; extensions to dired
(require 'jrv-dired-config nil my-minimal)     ; dired-compare and shell-commands
(require 'jrv-ess-config nil my-minimal)       ; ess and R configuration
(require 'jrv-git nil my-minimal)              ; n-days-old and local-git-copy
(require 'jrv-nxml-config nil my-minimal)      ; xml and html (requires jrv-html)
(require 'jrv-markdown nil my-minimal)         ; markdown (requires jrv-html)
(require 'jrv-org nil my-minimal)              ; load org mode and its customizations
(require 'jrv-python-elpy nil my-minimal)      ; python help, send-line/region
(require 'jrv-pweave nil my-minimal)           ; pweave using ESS and Pweave
(require 'jrv-cpp nil my-minimal)              ; C++ compilation options
(require 'jrv-windup-etc nil my-minimal)       ; windup, load-jrv-org, timelog
(require 'jrv-notmuch nil my-minimal)          ; notmuch configuration
(require 'jrv-csv nil my-minimal)              ; csv mode 
(when (require 'epa nil my-minimal)            ; gpg encryption and decryption
  (epa-file-enable))                           ; enable it
(require 'ess-site nil my-minimal)             ; ESS Support for R language
(require 'ess-rutils nil my-minimal)           ;   ..
(defvar my-yas-snippets-dir) ; from my-settings
(when (require 'yasnippet nil my-minimal)      ; yasnippet
  (setq yas-snippet-dirs (list my-yas-snippets-dir)) ; set snippets folder
  (declare-function yas-global-mode 1 "yasnippet")
  (yas-global-mode 1))                         ; enable it globally
(when (require 'hfyview nil my-minimal)        ; quick print buffer using web browser
  (declare-function hfyview-add-to-files-menu "hfyview")
  (hfyview-add-to-files-menu))                 ; add this to file menu
(require 'jrv-finish nil my-minimal)           ; open files, set up windows etc
