;;; customize and initialize package manager
(eval-and-compile  ;; eval-and-compile helps while batch compiling
  (require 'package)
  (setq package-enable-at-startup nil)
  (setq package-archives
        '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
          ;; ("MELPA Stable" . "http://stable.melpa.org/packages/")
          ("NONGNU ELPA"        . "http://elpa.nongnu.org/nongnu/")
          ("MELPA"        . "http://melpa.org/packages/")))
        ;; package-archive-priorities
        ;; '(("MELPA Stable" . 10)
        ;;   ("GNU ELPA"     . 5)
        ;;   ("MELPA"        . 0)))
  ;; package-initialize is supposedly not necessary in Emacs 27.1
  ;; but without it there is an error
  ;; Cannot open load file: No such file or directory, use-package
  (package-initialize) 
  (add-to-list 'load-path "~/.emacs.d/third-party-lisp/")
  (add-to-list 'load-path "~/.emacs.d/my-lisp/")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/"))

;;; install use-package if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; load personal config settings and custom file
(require 'jrv-settings)

;;; borrowed from better-defaults.el
;;; https://github.com/technomancy/better-defaults
(defvar apropos-do-all) ;; silence compiler warning
(setq load-prefer-newer t
      apropos-do-all t)

;;; load use-package
(eval-and-compile
  (require 'use-package))
(setq use-package-compute-statistics t) ;; enable use-package-report

;;; load file and folder locations
(require 'jrv-mypaths)

;; basic customization
(load-theme (intern jrv/settings/theme-name))  ;; set the theme

(defun jrv/init/set-modeline-colors-for-limited-color-terminals (frame)
  "Use simple mode-line colors when terminal has < 256 colors"
  (unless (> (length (defined-colors frame)) 255)
    (message "Setting mode colors for color limited terminal")
    (set-face-foreground 'mode-line "blue")
    (set-face-foreground 'mode-line-inactive "white")
    (set-face-background 'mode-line "white")
    (set-face-background 'mode-line-inactive "black")))
(add-hook 'after-make-frame-functions
          'jrv/init/set-modeline-colors-for-limited-color-terminals)

;; set cursor color
;; the backquote-comma combination forces evaluation of jrv/settings/cursor-color
(custom-set-faces
 `(cursor ((t (:background ,jrv/settings/cursor-color)))))
;; (add-to-list 'default-frame-alist `(cursor-color . ,jrv/settings/cursor-color))
(when jrv/settings/maximize-at-startup
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))


;;; Splash screen, Initial Buffer, buffer title line and col numbers
(setq inhibit-startup-screen t) ;inhibit flash screen
(setq frame-title-format "%b - emacs") ; buffername in frame title

;; (defvar jrv/settings/allow-pop-up-windows) ; from my-settings
(setq pop-up-windows jrv/settings/allow-pop-up-windows)
(line-number-mode 1) ; Enable line numbers 
(column-number-mode 1); Enable column numbers.
(menu-bar-mode -1) ;; switch menu bar off
(tool-bar-mode -1)  ;; switch tool bar off

(use-package uniquify
  ;; Better uniqify buffer names   
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(require 'keep-buffers)
;; Protect scratch and Messages buffers from accidental deletion
(keep-buffers-mode 1)
(setq keep-buffers-protected-alist      
      '(("\\`\\*scratch\\*\\'" . nil)
        ("\\`\\*Messages\\*\\'" . nil)))

(setq message-log-max 2000) ; make message buffer bigger

(use-package savehist
  :config
  (setq savehist-file jrv/settings/savehist-filename)  ; customize and
  (savehist-mode t))                     ; then activate history

(use-package recentf
  ;; Preserve recent files across sessions
  :config (recentf-mode 1))

;; Manage backups
;; backquote-comma combination forces evaluation of jrv/settings/backup-directory
(setq backup-directory-alist `(("." . ,jrv/settings/backup-directory))) 
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq confirm-kill-emacs 'y-or-n-p) ;; Confirm on exit

(setq-default fill-column most-positive-fixnum) ;; Disable auto-fill

(setq revert-without-query (list "\\.pdf\\'")) ;; auto-revert pdf files

;; visual mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'nxml-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

(show-paren-mode 1)                 ;; Always show matching parantheses 
(setq-default indent-tabs-mode nil) ;; Indent with spaces
(setq-default tab-width 4)          ;; (mainly for python)
(put 'downcase-region 'disabled nil) ;; Enable case change of region
(put 'upcase-region 'disabled nil)   ;; Enable case change of region

;; Ask confirmation for opening file only for size above 100MB
(setq large-file-warning-threshold (* 100 1000 1000))

(defalias 'yes-or-no-p 'y-or-n-p) ;; Replace yes/no questions with y/n

(setq vc-follow-symlinks t) ;; follow symlink to Git-controlled source file

(use-package ediff
  :config
  ;; Let Ediff show files side by side without creating new frames
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-keep-variants nil)
  ;; Always expand files in ediff sessions
  (declare-function outline-show-all "outline")
  (add-hook 'ediff-prepare-buffer-hook #'outline-show-all))

;; force emacs to use xdg-open even if there is no desktop environment
;; xdg-open checks for $BROWSER
;; set $BROWSER in ~/.profile by querying xdg-mime for text/html.
(setq browse-url-browser-function 'browse-url-xdg-open)

;; This has to be rewritten for Emacs 27.1 using after-focus-change-function
;; instead of focus-in and focus-out hooks
;; (when jrv/settings/tap-to-click-toggle
;;   (use-package jrv-tap-to-click-functions
;;     ;; disable mousepad tap-to-click inside emacs & enable it outside
;;     :commands
;;     tap-to-click-disable
;;     tap-to-click-enable
;;     :hook
;;     (focus-in . tap-to-click-disable)
;;     (focus-out . tap-to-click-enable)
;;     ))

;; outline-magic was conflicting with newer versions of org-mode
;; and it was not being used much anyway
;; (use-package outline-magic
;;   :init
;;   (setq outline-minor-mode-prefix "\M-#")
;;   :demand t
;;   :bind (:map outline-minor-mode-map ("<C-tab>" . outline-cycle)))

(use-package which-key
  :demand t
  :diminish ""
  :config
  (which-key-mode))

;; use pdf-tools instead of docview for pdf files
(use-package pdf-tools
  :if (not jrv/settings/is-Android-App)
  :demand t
  :commands
  pdf-view-fit-width-to-window
  :config
  ;; build without query and w/o installing system packages
  (pdf-tools-install t t)
  :bind (:map pdf-view-mode-map
              ("M" . (lambda()
                       (interactive)
                       (delete-other-windows)
                       (pdf-view-fit-width-to-window)))
              ("G" . pdf-view-goto-label)
              ("C-<home>" . pdf-view-first-page)
              ("C-<end>" . pdf-view-last-page))
  :hook
  (pdf-view-mode-hook . (lambda () (auto-revert-mode 1)))
  )

(setq kill-whole-line 'always)          ;; always delete whole line
(if (fboundp 'pending-delete-mode) 
    (pending-delete-mode 1))            ;; typing replaces selection
(when (display-graphic-p)               ;; cut paste into clipboard
  (setq select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

(use-package jrv-buffer-functions
  :demand t
  :bind (("C-x B"   . jrv/buffer/switch-buffer-other-window) 
         ("C-x K"   . jrv/buffer/kill-buffer-other-window)
         ("C-x O"   . jrv/buffer/swap-buffers-in-windows)      
         ("C-x D"   . jrv/buffer/dired-in-other-window)
         ("C-x C-F" . jrv/buffer/find-file-in-other-window)
         ("M-C-!"   . jrv/buffer/shell-command-in-shell)
         )
  )

;; use ido like icomplete for all commands eg notmuch-mua-prompt-for-sender
;; first we disable flex matching in fido mode
;; define function to disable flex matching
(defun jrv/init/fido-completion-setup()
  "Disable flex matching in fido mode"
  (when fido-mode
    ;; completion-styles in fido is '(flex). We change that 
    (setq-local completion-styles '(basic partial-completion emacs22))))
;; add it to minibuffer-setup-hook at the end (after fido setup has run)
(add-hook 'minibuffer-setup-hook #'jrv/init/fido-completion-setup 90)
;; enable fido mode
(fido-mode 1)

;; use ido only for buffers mainly for ignore-buffers
(use-package ido
  :config (ido-mode 'buffers)
  (setq ido-ignore-buffers
        '("\\` " jrv/buffer/ido-ignore-buffers-framewise)))


(use-package jrv-text-functions
  :commands
  jrv/text/wc
  jrv/text/proper-case-region
  jrv/text/decapitalize-word
  jrv/text/space-to-dash
  :bind
  ("M-c" . jrv/text/toggle-capitalize-word)
  )

;; Since I often press C-z by mistake 
;; I rebind it in graphic terminals where there is no need for C-z
;; (when (display-graphic-p)
;;   (global-set-key [(control z)] 'delete-other-windows))

(use-package org-agenda
  :after org
  :diminish ""
  :config
  (setq org-stuck-projects
        '("+proj" ("TODO" "NEXT" "WAITING") ))

  ;; always start agenda from today
  (setq org-agenda-start-on-weekday nil)

  ;; do not show time grid
  (setq org-agenda-use-time-grid nil)

  ;; do not show completed tasks
  (setq org-agenda-skip-scheduled-if-done t)

  ;; do not reorganize frame to show agenda
  (defvar org-agenda-window-setup)
  (setq org-agenda-window-setup 'current-window)

  ;; show breadcrumbs but not categories in agenda view
  ;; (replace 12:c by 12:b)
  (setq org-agenda-prefix-format ;; removed categories
        '((agenda . " %i %-12:b%?-12t% s")
          (timeline . "  % s")
          (todo . " %i %-12:b")
          (tags . " %i %-12:b")
          (search . " %i %-12:b")))

  ;; show log and clock report in agenda
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-start-with-clockreport-mode t)
  
  ;; custom view for Unscheduled TODOs
  (setq org-agenda-custom-commands
        '(("u" todo "TODO"
           ((org-agenda-todo-ignore-scheduled 'all)
            (org-agenda-todo-ignore-deadlines 'all)
            (org-agenda-overriding-header "Unscheduled TODOs: ")))))

)

(use-package org-capture
  :after org
  :config
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline org-default-notes-file "Tasks")
           ;; "* TODO %? %a %i\nSCHEDULED: <%(org-read-date)>\n")))
           "* TODO %? %a %i\nSCHEDULED: %(org-insert-time-stamp (current-time))\n")
          ("i" "issue" entry
           (file+headline "jrv-issues.org" "Issues")
           "* IMPROVEMENT %? %a %i %t\n")))
  (require 'ol-notmuch) ;; capture emails and create email links
  )

(use-package org-clock
  :after org
  :config
  ;; (setq org-clock-persist t) ;; replaced by jrv/org/clock-populate

  ;; Show only today's time in the mode line when clocking tasks
  (setq org-clock-mode-line-total 'today)

  (setq org-clock-history-length 10)
  )

(use-package jrv-org-functions 
  :commands
  jrv/org/open
  jrv/org/clock-populate
  jrv/org/pause-timer
  jrv/org/resume-timer
  jrv/org/clock-out-and-pause-timer
  )

(use-package org
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :commands
  org-clock-in
  org-refile
  :bind (
         ("C-c A" . org-agenda)
         ("C-c a" . (lambda (&optional arg)
                      (interactive "P")(org-agenda arg "a")))
         ("C-c c" . org-capture)
         ("C-c z" . (lambda () (interactive) (org-clock-in '(4))))
         :map org-agenda-keymap
         ("Z" . (lambda () (interactive) (org-refile '(4))))
   )
  :config
  (setq org-agenda-files
        (remove
         nil
         (mapcar #'jrv/mypaths/real-path
                 (list "org-file" "org-file-C" "org-file-R"))))
  (let ((paths
         (remove
          nil
          (mapcar #'jrv/mypaths/real-path
                  (list "org-file" "org-file-C" "org-file-R"
                        "org-target-P" "org-target-S")))))
  ;; backquote comma combination is for eval of variable inside quote
    (setq org-refile-targets `((,paths :maxlevel . 2))))

  (setq org-archive-location
        (concat (jrv/mypaths/real-path "org-file-archive") "::"))

  ;; project tag  :proj: should not be inherited
  (setq org-tags-exclude-from-inheritance '("proj"))

  ;; stuck projects are projects without a "next" action
  ;; use habit style in todo
  (add-to-list 'org-modules 'org-habit t)

  ;; basic set of keywords 
  (defvar org-todo-keywords)
  (setq org-todo-keywords
        '((sequence "TODO" "NEXT" "WAITING" "|" "DONE" "DISCARDED")))

  ;; color-code task types.
  (setq
   org-todo-keyword-faces
   '(("NEXT" .
      (:foreground "yellow" :background "red" :bold t :weight bold))
     ("TODO" .
      (:foreground "cyan" :background "steelblue" :bold t :weight bold))
     ("WAITING" .
      (:foreground "yellow" :background "magenta2" :bold t :weight bold))
     ("DONE" .
      (:foreground "gray50" :background "gray30"))))

  ;; use shift keys to select
  (setq org-support-shift-select 'always)

  ;; record when to-do was finished
  (setq org-log-done t)

  (setq org-log-into-drawer t)

  (setq org-default-notes-file (jrv/mypaths/real-path "org-file"))

  (setq org-catch-invisible-edits 'smart)
  )

;; Killing emacs
;; save-buffers-kill-terminal does not kill daemon (aliased to 1k below)
;; kill-emacs does not offer to save files 
;; save-buffers-kill-emacs kills daemon after offering to save files
(unless jrv/settings/is-Android-App
  ;; on phone keep the default binding (save-buffers-kill-terminal)
  (define-key global-map "\C-x\C-c" 'save-buffers-kill-emacs))


(use-package locate
  :config 
  (setq locate-command "locate -iA")
  (setq locate-prompt-for-command t))

(use-package flyspell
  :defer t
  :diminish ""
  :init
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'nxml-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  :config
  (setq flyspell-issue-welcome-flag nil)
  (setq flyspell-issue-message-flag nil)
  (setq ispell-program-name "aspell")    ; use aspell instead of ispell
)

(use-package jrv-mypaths)

(use-package jrv-aliases-short-keys)

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :diminish ""
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (customize-set-variable 'company-dabbrev-downcase nil)
  (company-auctex-init)
  (global-company-mode)
  ;; (setq company-ispell-dictionary jrv/settings/dictionary)
  (setq ispell-complete-word-dict jrv/settings/dictionary)
  ;; We use dabbrev and ispell in notmuch message (see jrv-notmuch.el).
  ;; If we want dabbrev in all buffers, uncomment the following:
  ;; ;; dabbrev will block ispell (and others) unless we used grouped backends
  ;; ;; so the following will not work
  ;; ;; (add-to-list 'company-backends 'company-ispell t)
  ;; ;; Hence, we delete dabbrev and add a grouped backend
  ;; (defvar company-backends)
  ;; (delete 'company-dabbrev company-backends)
  ;; (add-to-list 'company-backends '(company-dabbrev :with company-ispell) t)
  )

(use-package ibuf-ext ;; Only to silence compiler warning
  :commands ibuffer-switch-to-saved-filter-groups
  )

(use-package ibuffer
  :commands
  ibuffer
  ibuffer-other-window
  :defines ibuffer-saved-filter-groups ibuffer-show-empty-filter-groups
  :bind
  ;;replace default with ibuffer.
  ;; Open in other window, and take me there.
  ("C-x C-b" . ibuffer-other-window)
  :config
  ;;sort on major-mode
  (setq ibuffer-default-sorting-mode 'major-mode)
  ;; configure "default" filter-group
  (setq ibuffer-saved-filter-groups
        (quote (("default"      
                 ("Org" ;; all org-related buffers
                  (or
                   (mode . org-mode) 
                   (mode . org-agenda-mode)))
                 ("Mail"
                  (or  ;; mail-related buffers
                   (mode . message-mode)
                   (mode . mail-mode)
                   (mode . notmuch-tree-mode)
                   (mode . notmuch-search-mode)
                   (mode . notmuch-hello-mode)
                   (mode . notmuch-show-mode)
                   ))
                 ("Dired"
                  (mode . dired-mode))
                 ("Programming"
                  (or
                   (mode . ess-mode)
                   (mode . LaTex-mode)
                   (mode . python-mode)
                   (mode . emacs-lisp-mode)
                   (mode . lisp-interaction-mode)
                   ;; etc
                   )) 
                 ("Documents"
                  (or
                   (mode . latex-mode)
                   (mode . markdown-mode)
                   ;; etc
                   )) 
                 ))))
  ;;Don't show (filter) groups that are empty.
  (setq ibuffer-show-empty-filter-groups nil)
  :hook
  ;; always use "default" filter-group
  (ibuffer-mode .
                (lambda ()
                  (ibuffer-switch-to-saved-filter-groups "default")))
  )

(use-package jrv-auctex-functions
  :commands
  jrv/auctex/texcount
  jrv/auctex/make-handout-pdf
  jrv/auctex/TeX-help
)

(defvar LaTeX-mode-map) ;; suppress compiler warnings
(use-package tex :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-PDF-mode t)
  (setq-default TeX-engine 'xetex)
  (setq TeX-force-default-mode t)
  (setq LaTeX-command-style
        '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout) ")))
  (setq TeX-view-program-selection
        (quote
         (((output-dvistyle-pstricks)  "xdg-open")
          (output-dvi "xdg-open") 
          (output-pdf "PDF Tools")
          (output-html "xdg-open"))))
  :bind (:map LaTeX-mode-map
              ("C-c C-h" . jrv/auctex/TeX-help)
              ("C-c w" . jrv/auctex/texcount)
              ("C-c h" . jrv/auctex/make-handout-pdf))
  ;; outline-magic.el was disabled due to conflict with org-mode
  ;; :hook
  ;; (LaTeX-mode . 
  ;;             (lambda ()
  ;;               (setq outline-promotion-headings
  ;;                     ;; see outline-magic.el
  ;;                     '("\\chapter"
  ;;                       "\\section" "\\subsection" "\\subsubsection"
  ;;                       "\\paragraph" "\\subparagraph"))))
  )

(use-package jrv-dired-functions
  :demand
  :commands
  jrv/dired/search-file-by-start-letter
  jrv/dired/copy-filename-with-backslash-as-kill
  jrv/dired/revert-buffer-clean-slate
  jrv/dired/launch-file 
  jrv/dired/execute-file-in-shell
  jrv/dired/async-shell-command
  jrv/dired/compare-directories
  jrv/dired/dos2unix
  jrv/dired/new-file
  )

(use-package dired
  :config
  (use-package dired-sort-map); s s/x/t/n to sort by Size/eXt/Time/Name
  (use-package dired-x)       ; extensions to dired
  ;; (use-package dired+)        ; extensions to dired

  (setq dired-dwim-target t) ; default to other dired window for copy
  (setq dired-recursive-copies 'always)
  (defvar jrv/dired/dired-listing-switches)
  (setq dired-listing-switches jrv/dired/dired-listing-switches)
  (declare-function dired-hide-details-mode "dired") 
  ;; (setq dired-omit-files
  ;;       (concat dired-omit-files "\\|^\\..+$"))
  ;; add guesses for command to be run on file based on extension
  (setq dired-guess-shell-alist-user
        '(("\\.py\\'" "python")
          ("\\.xlsx?\\'" "my-nohup libreoffice")
          ("\\.docx?\\'" "my-nohup libreoffice")
          ("\\.ipynb?\\'" "jupyter-notebook" "ipnb2md.py" "ipnb2pdf")
          ("\\.md?\\'" "md2beamer" "md2ipnb.py")
          ("\\.pdf\\'" "evince" "xournalpp" "gimagereader-gtk" "okular")))
  ;; enable 'a' to visit folder in same buffer
  (put 'dired-find-alternate-file 'disabled nil)   
  :bind (:map dired-mode-map
              ("," . jrv/dired/search-file-by-start-letter)
              ("," . jrv/dired/search-file-by-start-letter)
              (";" . jrv/dired/launch-file) 
              ("M-C-!" . jrv/dired/execute-file-in-shell)
              ("M-C-&" . jrv/dired/async-shell-command)
              ("C-<F9>" . jrv/dired/compare-directories)
              ;; ("C-=" . dired-diff)
              ("W" . jrv/dired/copy-filename-with-backslash-as-kill)
              ("2" . jrv/dired/decrypt-pdf)
              ("c" . jrv/dired/new-file)
              ("\\" . jrv/dired/revert-buffer-clean-slate)
              (")" . jrv/dired/toggle-dired-classify)
              )
  :hook
   (dired-mode . (lambda () (dired-hide-details-mode 1))))
   ;; This is redundant after simplifying dired-listing-switches?
   ;; (dired-before-readin .
   ;;                     (lambda ()
   ;;                       (when (file-remote-p default-directory)
   ;;                         (setq dired-actual-switches "-al"))))
   ;; the -A listing switch omits . and ... That is enough for now
   ;; (dired-mode . (lambda () (dired-omit-mode 1)))

(use-package dired-subtree
  :demand
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle) 
        ("<C-tab>" . dired-subtree-cycle) 
        ("<S-iso-lefttab>" . dired-subtree-remove)))


(use-package jrv-shell-commands
  :commands
  jrv/shell/command-in-shell
  jrv/shell/send-buffer-file-to-h
  jrv/shell/command-on-buffer-file
  )

(use-package jrv-ess-functions
  :commands
  jrv/ess/r-help
  jrv/ess/bibtex
  )

(use-package ess
  :defines ess-r-mode-map
  :config
  ;; (with-no-warnings (setq ess-smart-S-assign-key nil))
  (setq inferior-R-args "--no-restore-history --no-save")
  (setq ess-ask-for-ess-directory nil)
  ;; (setq ess-swv-processor 'knitr)
  (let ((folder (jrv/mypaths/real-path "t")))
        (when folder
          (setq ess-history-directory (concat folder "/.R/"))))
   ;; scroll R window to bottom after each evaluation
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-scroll-show-maximum-output t)
  :hook
  (ess-r-mode . (lambda ()
                  (interactive)
                  (define-key ess-r-mode-map "\C-c\C-h"
                    'jrv/ess/r-help)))
  )
;; :defines ess-swv-processor
;; :init (require 'ess-site)
;; (setq ess-swv-pdflatex-commands
;;       '("xelatex" "pdflatex" "texi2pdf" "make"))
;; (setq ess-pdf-viewer-pref "emacsclient")
;; setq does not work for TeX-source-correlate-mode so we customize
;; (custom-set-variables '(TeX-source-correlate-mode t))
;; (add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r-mode))
;; In Rnw and Pwv files, define key  "C-c H" to make-handout-pdf (jrv-auctex.el)
  
;; (use-package ess-noweb
;;   :bind
;;   (:map ess-noweb-mode-prefix-map ("H" . jrv/auctex/make-handout-pdf)))

(use-package magit
  :init
  ;; ;;;;;;;; suppress compiler warnings ;;;;;;;;;;;
  (declare-function magit-mode-get-buffers "magit-mode")
  (declare-function magit-restore-window-configuration "magit-mode")
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun jrv/init/magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    ;; borrowed from
    ;; http://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  :bind
  (:map magit-status-mode-map 
        ("q" . jrv/init/magit-kill-buffers)))

(use-package nxml-mode
  :mode "\\.blog\\'"
  )

(use-package xmlpe
  :mode "\\.blog\\'"
  :config (setq xmlpe-invisible-hf t)
  )

(use-package web-mode
  :mode "\\.php\\'"
  )

(use-package jrv-markdown-functions
  :commands
  jrv/markdown/export-blog
  jrv/markdown/to-pdf
  )

(use-package markdown-mode
  :mode ("\\.m[k]d\\'" . markdown-mode)
  :init
  :config
  (setq markdown-command
        "pandoc --ascii -F pantable -f markdown -t html")
  :hook
  (markdown-mode .
                 (lambda ()
                   (when (string-match "\.[pP]md$" (buffer-name))
                     ;; run Pweave before running pandoc
                     (make-local-variable 'markdown-command)
                     (setq markdown-command "pwv-pandoc"))))
  :bind
  (:map markdown-mode-map
        ("C-c C-c b" . jrv/markdown/export-blog)
        ("C-c C-c P" . jrv/markdown/to-pdf)
        ("C--" . (lambda () (interactive) (insert "&ndash;"))))
)
;;; Python mode
;; emacs packages desired are:    elpy, flycheck
;; related emacs packages (not used): py-autopep8, ein
;; python pip packages desired are
;;       rope_py3k, jedi, flake8, importmagic, autopep8, yapf

(use-package poly-R
  :mode ("\\.[Rr]md\\'" . poly-markdown+r-mode)
  )

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--TerminalInteractiveShell.simple_prompt=True")
  
  )

(use-package jrv-python-functions
  :commands
  jrv/python/insert-docstring
  jrv/python/flycheck
  jrv/python/help
  )

(use-package elpy :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config 
  (setq elpy-shell-starting-directory 'current-directory)
  :hook
  (elpy-mode . (lambda ()
                 (when (require 'flycheck nil t)
                   (setq elpy-modules
                         (delq 'elpy-module-flymake elpy-modules))
                   (jrv/python/flycheck))))
  :bind (:map python-mode-map
              ("C-c C-h" . jrv/python/help)
              ("C-c d" . jrv/python/insert-docstring))
  )

(use-package cc-mode
  :defer t
  :bind (:map c-mode-base-map
         ("C-c C-l" . compile))
  :hook
  (c-mode-common .
                 (lambda()
                   ;; if there is no Makefile in the current directory
                   ;; compile-command runs g++ on the current file
                   ;; executable is file name without extension
                   (unless (file-exists-p "Makefile")
                     (set (make-local-variable 'compile-command)
                          (let ((file (file-name-nondirectory
                                       buffer-file-name)))
                            (format
                             "g++ -o %s %s %s"
                             (file-name-sans-extension file)
                             "-ansi -pedantic -Wall -Wno-comment -g "
                             file))))))
  )

(use-package jrv-notmuch-functions
  :commands
  jrv/notmuch/get-my-mails
  jrv/notmuch/forward-inline
  jrv/notmuch/rss-url-browse
  jrv/notmuch/config
  jrv/notmuch/open-link
  jrv/notmuch/mimedown-ask
  jrv/notmuch/de-fill
  jrv/notmuch/choose-plain-or-html
  )

(use-package mailcap ;; Only to silence compiler warning
  :commands
  mailcap-parse-mailcaps
  )

(use-package notmuch
  :defer t
  :commands
  notmuch-hello
  notmuch-tree
  notmuch-search
  notmuch-tree-show-message-in
  notmuch-tree-get-tags
  notmuch-tree-tag
  :config
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-crypto-process-mime t)
  (setq notmuch-multipart/alternative-discouraged
        '("text/plain" "text/html"))
  (setq notmuch-saved-searches '(
        (:name "unread" :query "tag:unread" :key "u" :search-type tree)
        (:name "sent" :query "tag:sent" :key "t" :search-type tree)
        (:name "drafts" :query "tag:draft" :key "d" :search-type tree)
        (:name "1 days" :query "date:1D.." :key "1" :search-type tree)
        (:name "2 days" :query "date:2D.." :key "2" :search-type tree)
        (:name "5 days" :query "date:5D.." :key "5" :search-type tree)))
  (setq mail-signature-file jrv/settings/email-signature-file)
  (setq notmuch-address-command "my-nottoomuch-addresses.sh")
  (setq mailcap-mime-data
        '(("*" (".*" (viewer . "xdg-open \"%s\"") (type . "*/*")))))
  (setq notmuch-fcc-dirs jrv/settings/notmuch-fcc-dirs)
  (mailcap-parse-mailcaps)
  (setq mm-text-html-renderer 'w3m)
  (setq mm-default-directory jrv/settings/mm-default-directory)
  (setq mm-html-inhibit-images t)
  :bind (:map notmuch-tree-mode-map
              ("C-<return>" .
               (lambda ()
                 "Show and switch to current message in message pane"
                 (interactive)
                 (notmuch-tree-show-message-in)
                 (select-window notmuch-tree-message-window)))
              ("g" . jrv/notmuch/get-my-mails)
              ("F" . jrv/notmuch/forward-inline)
              ("]" . jrv/notmuch/rss-url-browse)
              ("~" . jrv/notmuch/config)
              ("!" . (lambda ()
                       "toggle unread tag for message"
                       (interactive)
                       (if (member "unread" (notmuch-tree-get-tags))
                           (notmuch-tree-tag '("-unread"))
                         (notmuch-tree-tag '("+unread")))))
              :map notmuch-show-mode-map
              ("F" . jrv/notmuch/forward-inline)
              ("]" . jrv/notmuch/open-link)
              ("~" . jrv/notmuch/config)
              ("C-c C-o" . goto-address-at-point)
              )
  :bind (:map notmuch-message-mode-map
              ("C-c i" .
               (lambda () "Choose Identity Interactively." (interactive)
                 (gnus-alias-use-identity
                  (ido-completing-read
                   "Choose Identity: "
                   (mapcar 'car jrv/settings/email-addresses)))))
              ("M-q" . jrv/notmuch/de-fill)
              ("C-c C-c" . jrv/notmuch/mimedown-ask)
              )

  )

(use-package message
  :after notmuch
  :commands
  message-forward-subject-fwd
  :hook
  (message-mode .
                (lambda ()
                  (mail-abbrevs-mode t)
                  (make-local-variable 'company-backends)
                  (setq company-backends
                        '((:separate
                           company-dabbrev company-ispell)))
                  (setq-local company-idle-delay 0)
                  (turn-off-auto-fill)
                  (setq
                   truncate-lines nil
                   word-wrap t)
                   ;; use-hard-newlines t)
                  (flyspell-mode)))
  :config
  (setq message-citation-line-function
        'message-insert-formatted-citation-line)
  (setq message-citation-line-format
        "On %a, %b %d %Y at %r, %f wrote:")
  (setq message-make-forward-subject-function
        #'message-forward-subject-fwd)
  (setq message-auto-save-directory jrv/settings/message-auto-save-directory)
  (setq message-send-mail-partially-limit nil)
  (setq message-sendmail-envelope-from 'header)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-forward-as-mime t)
  :hook
  (message-send-mail . jrv/notmuch/feed-msmtp)
  (message-sent .
                (lambda () (start-process
                            "pymsmtpq-flush" "*notmuch-pymsmtpq*"
                            "pymsmtpq" "--manage" "s")))
  )

(use-package gnus-alias
  :after notmuch
  :hook
  (message-setup . gnus-alias-determine-identity)
  :config
  (autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
  ;; Determine identity when message-mode loads
  ;; set up the identities
  (defun jrv/init/make-identity-alist (x)
    "Helper function to make gnus-alias-identity-alist

     Use (mapcar 'make-identity-alist jrv/settings/email-addresses)
     to make gnus-alias-identity-alist"
    (list (elt x 0)    ;; the identity alias
          nil          ;; Does not refer to any other identity
          (concat jrv/settings/email-real-name " <" (elt x 1) "> ")   ;; Sender address
          nil          ;; No organization header
          nil          ;; No extra headers
          nil          ;; No extra body text
          jrv/settings/email-signature-file
          ))
  (setq gnus-alias-identity-alist
        (mapcar 'jrv/init/make-identity-alist
                jrv/settings/email-addresses))

  ;; default identity
  (setq gnus-alias-default-identity "i")
  ;; Define rules to match gmail identity
  (defun jrv/init/make-identity-rules (x)
    "Helper function to make gnus-alias-identity-rules

     Use (mapcar 'make-identity-rules jrv/settings/email-addresses)
     to make gnus-alias-identity-rules"
    (list
     ;; Rule name/description
     (elt x 0)
     ;; Rule: search for this address in all headers
     ;; in both original message and reply
     (list "any" (elt x 1)  'both)
     ;; Identity: if rule matched, choose this identity
     (elt x 0)
     ))
  (setq gnus-alias-identity-rules
        (mapcar 'jrv/init/make-identity-rules
                jrv/settings/email-addresses))
  )

(use-package sendmail
  :after notmuch
  :config
  (setq sendmail-program "~/bin/pymsmtpq") 
  )

(use-package csv-mode
  ;; align/delete/insert columns in csv files
  :mode "\\.[Cc][Ss][Vv]\\'"
  )

(use-package epa
  ;; Setup automatic encryption and decryption of gpg files
  :config
  (epa-file-enable)
  ;; (when jrv/settings/is-Android-App
  ;;   (setq epa-pinentry-mode 'loopback))
  )

(use-package jrv-pweave
  ;; pweave python+tex and python+markdown files
  :mode 
  ("\\.Plw\\'" . poly-Plw-mode)
  ("\\.pmd\\'" . poly-pmd-mode)
  )

(use-package yasnippet
  ;; mode wise snippets
  :diminish (yas-minor-mode . "")
  :config
  (setq yas-snippet-dirs
        (list jrv/settings/yas-snippets-dir)) ; set snippets fold
  (yas-global-mode 1)
  )

(use-package hfyview
  ;; quick print buffer using web browser
  :config
  (declare-function hfyview-add-to-files-menu
                    "hfyview") ;; suppress compiler warning
  (hfyview-add-to-files-menu)   ; add this to file menu
  )

(use-package jrv-finish
  ;; commands executed while launching main emacsclient
  :commands
  jrv/finish/main
  jrv/finish/std-windows
  jrv/finish/mail-windows
  )

(use-package jrv-EOD-functions
  ;; end of day commands
  :commands
  jrv/EOD/reminder
  jrv/EOD/timers
  jrv/EOD/timelog
  )

;; (use-package-report)

(use-package jrv-god-functions
  :commands
  jrv/god/god-mode-all
  jrv/god/god-mode-self-insert
  jrv/god/insert-backtick
  jrv/god/select-window
  )

(use-package god-mode
  ;; modal editor
  :ensure t
  :config
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  :bind
  (("M-`" . 'jrv/god/insert-backtick)
  ("C-`" . 'jrv/god/insert-backtick)
  :map god-local-mode-map
  ("." . 'repeat)
  ("i" . 'jrv/god/select-window)
  ([remap self-insert-command] . #'jrv/god/god-mode-self-insert))
  )

(bind-key jrv/settings/god-mode-key 'jrv/god/god-mode-all)
(bind-key jrv/settings/god-mode-key 'jrv/god/god-mode-all
          god-local-mode-map)

(use-package dired-async
  ;; dired copy and move happens synchronously
  ;; :config
  ;; (dired-async-mode 1)
  )

(use-package expand-region
  ;; C-= expands selection to word quote sentence etc
  :bind
  ("C-=" . 'er/expand-region))

(use-package anzu
  ;; show number of matches in modeline
  :diminish ""
  :config
  (global-anzu-mode +1))

(use-package browse-kill-ring
  ;; M-y shows kill ring in a buffer
  :config
  (browse-kill-ring-default-keybindings))

(use-package sdcv
  ;; access sdcv dictionary
  :demand t
  :commands
  sdcv-next-dictionary
  sdcv-previous-dictionary
  :bind (:map sdcv-mode-map
  ("f" . 'sdcv-next-dictionary)
  ("b". 'sdcv-previous-dictionary)))

(use-package amx
  ;; replace M-x with better completion
  :config
  (setq amx-backend 'standard)
  (amx-mode 1))

(use-package iedit
  ;; refactoring minor-mode (edit all occurences of string)
  ;; C-; to toggle mode. C-' to toggle hide non matching lines
  )

(use-package goto-chg
  ;; goto-last-change and goto-last-change-reverse
  ;; no keys defined yet for these commands use M-x instead
  )

;;; Putting it here for want of a better place
;; old ess-noweb used M-nx to insert inline r expression
;; after shifting to poly-noweb+r, we replicate this functionality 
(add-hook 'poly-noweb+r-mode-hook
          '(lambda () (define-key poly-noweb+r-mode-map "\M-nx"
                        '(lambda () (interactive)
                           (insert "\\Sexpr{}") (backward-char 1)))))
