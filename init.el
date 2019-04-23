;;; customize and initialize package manager
(eval-and-compile  ;; eval-and-compile helps while batch compiling
  (require 'package)
  (setq package-enable-at-startup nil)
  (setq package-archives
        '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
          ("MELPA Stable" . "http://stable.melpa.org/packages/")
          ("MELPA"        . "http://melpa.org/packages/"))
        package-archive-priorities
        '(("MELPA Stable" . 10)
          ("GNU ELPA"     . 5)
          ("MELPA"        . 0)))
  (package-initialize)
  (add-to-list 'load-path "~/.emacs.d/third-party-lisp/")
  (add-to-list 'load-path "~/.emacs.d/my-lisp/")
)

;;; install use-package if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq custom-file "~/.emacs.d/jrv-custom.el")
(load custom-file)

;;; load use-package
(eval-and-compile
  (require 'use-package))
(setq use-package-compute-statistics t) ;; enable use-package-report

;;; load personal config settings
(require 'jrv-settings)

;;; load file and folder locations
(require 'jrv-mypaths)

;; basic customization
(load-theme (intern jrv/settings-theme-name))  ;; set the theme

(defvar jrv-limited-color-terminal nil)
(defun jrv/color-capability (frame)
  "Use simple mode-line colors when terminal has < 256 colors" 
  (unless (> (length (defined-colors frame)) 255)
    (setq jrv-limited-color-terminal t)
    (message "Setting mode colors for color limited terminal")
    (set-face-foreground 'mode-line "blue")
    (set-face-foreground 'mode-line-inactive "white")
    (set-face-background 'mode-line "white")
    (set-face-background 'mode-line-inactive "black")))
(add-hook 'after-make-frame-functions 'jrv/color-capability)

;; set cursor color
;; the backquote-comma combination forces evaluation of jrv/settings-cursor-color
(add-to-list 'default-frame-alist `(cursor-color . ,jrv/settings-cursor-color))
(when jrv/settings-maximize-at-startup
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

;;; Splash screen, Initial Buffer, buffer title line and col numbers
(setq inhibit-startup-screen t) ;inhibit flash screen
(setq frame-title-format "%b - emacs") ; buffername in frame title

;; (defvar jrv/settings-allow-pop-up-windows) ; from my-settings
(setq pop-up-windows jrv/settings-allow-pop-up-windows)
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
  (setq savehist-file jrv/settings-savehist-filename)  ; customize and
  (savehist-mode t))                     ; then activate history

(use-package recentf
  ;; Preserve recent files across sessions
  :config (recentf-mode 1))

;; Manage backups
;; backquote-comma combination forces evaluation of jrv/settings-backup-directory
(setq backup-directory-alist `(("." . ,jrv/settings-backup-directory))) 
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq confirm-kill-emacs 'y-or-n-p) ;; Confirm on exit

(setq-default fill-column most-positive-fixnum) ;; Disable auto-fill

;; visual mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'nxml-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

(show-paren-mode 1)                 ;; Always show matching parantheses 
(setq-default indent-tabs-mode nil) ;; Indent with spaces
(setq-default tab-width 4)          ;; (mainly for python)
(put 'downcase-region 'disabled nil) ;; Enable case change of region
(put 'upcase-region 'disabled nil)   ;; Enable case change of region

(defalias 'yes-or-no-p 'y-or-n-p) ;; Replace yes/no questions with y/n

(use-package ediff
  :config
  ;; Let Ediff show files side by side without creating new frames
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-keep-variants nil))

;; force emacs to use xdg-open even if there is no desktop environment
;; xdg-open checks for $BROWSER
;; set $BROWSER in ~/.profile by querying xdg-mime for text/html.
(setq browse-url-browser-function 'browse-url-xdg-open)

(when jrv/settings-tap-to-click-toggle
  (use-package jrv-tap-to-click-functions
    ;; disable mousepad tap-to-click inside emacs & enable it outside
    :commands
    tap-to-click-disable
    tap-to-click-enable
    :hook
    (focus-in . tap-to-click-disable)
    (focus-out . tap-to-click-enable)
    ))

(use-package outline-magic
  :after (outline)
  :bind (:map outline-minor-mode-map ("<C-tab>" . outline-cycle)))

;; use pdf-tools instead of docview for pdf files
(use-package pdf-tools
  :if (not jrv/settings-is-Android-App)
  :demand t
  :config
  (pdf-tools-install t)
  :bind (:map pdf-view-mode-map
              ("M" . (lambda()
                       (interactive)
                       (delete-other-windows)
                       (pdf-view-fit-width-to-window)))
              ("G" . pdf-view-goto-label)
              ("C-<home>" . pdf-view-first-page)
              ("C-<end>" . pdf-view-last-page))
  )

(setq kill-whole-line 'always)          ;; always delete whole line
(if (fboundp 'pending-delete-mode) 
    (pending-delete-mode 1))            ;; typing replaces selection
(when (display-graphic-p)               ;; cut paste into clipboard
  (setq select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

(use-package jrv-buffer-functions
  :demand t
  :bind (("C-x B"   . jrv/buffer-switch-buffer-other-window) 
         ("C-x K"   . jrv/buffer-kill-buffer-other-window)
         ("C-x O"   . jrv/buffer-swap-buffers-in-windows)      
         ("C-x D"   . jrv/buffer-dired-in-other-window)
         ("C-x C-F" . jrv/buffer-find-file-in-other-window)
         ("M-C-!"   . jrv/buffer-shell-command-in-shell)
         )
  )

(use-package ido
  :config (ido-mode 'buffers)
  (setq ido-ignore-buffers
        '("\\` " jrv/buffer-ido-ignore-buffers-framewise)))

(use-package jrv-text-functions
  :commands
  jrv/text-wc
  jrv/text-proper-case-region
  jrv/text-decapitalize-word
  jrv/text-space-to-dash
  :bind
  ("C-x C-x" . jrv/text-decapitalize-word)
  )

;; Since I often press C-z by mistake 
;; I rebind it in graphic terminals where there is no need for C-z
(when (display-graphic-p)
  (global-set-key [(control z)] 'delete-other-windows))

(use-package org-agenda
  :after org
  :config
  (setq org-stuck-projects
        '("+proj" ("TODO" "NEXT" "WAITING") ))

  ;; always start agenda from today
  (setq org-agenda-start-on-weekday nil)

  ;; do not show time grid
  (setq org-agenda-use-time-grid nil)

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
           "* TODO %? %a %i\nSCHEDULED: %(org-insert-time-stamp (current-time))\n")))
  (require 'org-notmuch) ;; capture emails and create email links
  )

(use-package org-clock
  :after org
  :config
  ;; (setq org-clock-persist t) ;; replaced by jrv/org-clock-populate

  ;; Show only today's time in the mode line when clocking tasks
  (setq org-clock-mode-line-total 'today)

  (setq org-clock-history-length 10)
  )

(use-package jrv-org-functions 
  :commands
  jrv/org-path
  jrv/org-open
  jrv/org-clock-populate
  jrv/org-pause-timer
  jrv/org-resume-timer
  jrv/org-clock-out-and-pause-timer
  )

(use-package org
  :defer t
  :mode ("\\.org\\'" . org-mode)
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
         (mapcar #'jrv/mypaths-real-path
                 (list "main-org-file" "additional-org-file"))))
  (let ((paths
         (remove
          nil
          (mapcar #'jrv/mypaths-real-path
                  (list "main-org-file"
                        "additional-org-file"
                        "additional-org-target-P"
                        "additional-org-target-S")))))
  ;; backquote comma combination is for eval of variable inside quote
    (setq org-refile-targets `((,paths :maxlevel . 2))))

  (setq org-archive-location
        (concat (jrv/mypaths-real-path "main-org-archive-file") "::"))

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

  (setq org-default-notes-file (jrv/mypaths-real-path "main-org-file"))

  (setq org-catch-invisible-edits 'smart)
  )

;; Killing emacs
;; save-buffers-kill-terminal does not kill daemon (aliased to 1k below)
;; kill-emacs does not offer to save files 
;; save-buffers-kill-emacs kills daemon after offering to save files
(unless jrv/settings-is-Android-App
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
  (company-auctex-init)
  (global-company-mode)
  ;; (setq company-ispell-dictionary jrv/settings-dictionary)
  (setq ispell-complete-word-dict jrv/settings-dictionary)
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
  jrv/auctex-texcount
  jrv/auctex-make-handout-pdf
  jrv/auctex-TeX-help
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
              ("C-c C-h" . jrv/auctex-TeX-help)
              ("C-c w" . jrv/auctex-texcount)
              ("C-c h" . jrv/auctex-make-handout-pdf))
  :hook
  (LaTeX-mode . 
              (lambda ()
                (setq outline-promotion-headings
                      ;; see outline-magic.el
                      '("\\chapter"
                        "\\section" "\\subsection" "\\subsubsection"
                        "\\paragraph" "\\subparagraph"))))
  )

(use-package jrv-dired-functions
  :commands
  jrv/dired-search-file-by-start-letter
  jrv/dired-copy-filename-with-backslash-as-kill
  jrv/dired-revert-buffer-clean-slate
  jrv/dired-launch-file 
  jrv/dired-execute-file-in-shell
  jrv/dired-async-shell-command
  jrv/dired-compare-directories
  jrv/dired-dos2unix
  jrv/dired-new-file
  )

(use-package dired
  :config
  (use-package dired-sort-map); s s/x/t/n to sort by Size/eXt/Time/Name
  (use-package dired-x)       ; extensions to dired
  (use-package dired+)        ; extensions to dired

  (setq dired-dwim-target t) ; default to other dired window for copy
  (setq dired-recursive-copies 'always)
  (defvar jrv-dired-listing-switches
    "-ahl --time-style=+%e%b%y%t%H:%M --group-directories-first")
  (setq dired-listing-switches jrv-dired-listing-switches)
  (setq-default dired-omit-files-p t) ; this is buffer-local variable
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..+$"))
  ;; add guesses for command to be run on file based on extension
  (setq dired-guess-shell-alist-user
        '(("\\.py\\'" "python")
          ("\\.xlsx?\\'" "my-nohup libreoffice")
          ("\\.docx?\\'" "my-nohup libreoffice")
          ("\\.ipynb?\\'" "jupyter-notebook" "ipnb2md.py" "ipnb2pdf")
          ("\\.md?\\'" "md2ipnb.py" "md2beamer")
          ("\\.pdf\\'" "evince" "xournal" "gimagereader-gtk" "okular")))
  ;; enable 'a' to visit folder in same buffer
  (put 'dired-find-alternate-file 'disabled nil)   
  :bind (:map dired-mode-map
              ("," . jrv/dired-search-file-by-start-letter)
              (";" . jrv/dired-launch-file) 
              ("M-C-!" . jrv/dired-execute-file-in-shell)
              ("M-C-&" . jrv/dired-async-shell-command)
              ("C-<F9>" . jrv/dired-compare-directories)
              ("C-=" . dired-diff)
              ("W" . jrv/dired-copy-filename-with-backslash-as-kill)
              ("2" . jrv/dired-dos2unix)
              ("c" . jrv/dired-new-file)
              ("\\" . jrv/dired-revert-buffer-clean-slate)
              )
  )

(use-package jrv-shell-commands
  :commands
  jrv/shell-command-in-shell
  jrv/shell-send-buffer-file-to-h
  jrv/shell-command-on-buffer-file
  )

(use-package jrv-ess-functions
  :commands
  jrv/ess-r-help
  jrv/ess-bibtex
  )

(use-package ess
  :defines ess-swv-processor
  :init (require 'ess-site)
  :config
  (setq inferior-R-args "--no-restore-history --no-save")
  (setq ess-ask-for-ess-directory nil)
  (setq ess-swv-processor 'knitr)
  (let ((folder (jrv/mypaths-real-path "t")))
        (when folder
          (setq ess-history-directory (concat folder "/.R/"))))
   ;; scroll R window to bottom after each evaluation
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-scroll-show-maximum-output t)
  (setq ess-swv-pdflatex-commands
        '("xelatex" "pdflatex" "texi2pdf" "make"))
  (setq ess-pdf-viewer-pref "emacsclient")
  ;; setq does not work for TeX-source-correlate-mode so we customize
  ;; (custom-set-variables '(TeX-source-correlate-mode t))
  :bind
  (:map ess-mode-map ("C-c C-h" . jrv/ess-r-help))
  )

  ;; (add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r-mode))
  ;; In Rnw and Pwv files, define key  "C-c H" to make-handout-pdf (jrv-auctex.el)
  
(use-package ess-noweb
  :bind
  (:map ess-noweb-mode-prefix-map ("H" . jrv/auctex-make-handout-pdf)))

(use-package magit
  :init
  ;; ;;;;;;;; suppress compiler warnings ;;;;;;;;;;;
  (declare-function magit-mode-get-buffers "magit-mode")
  (declare-function magit-restore-window-configuration "magit-mode")
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun jrv/magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    ;; borrowed from
    ;; http://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  :bind
  (:map magit-status-mode-map 
        ("q" . jrv/-magit-kill-buffers)))

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
  jrv/markdown-export-blog
  jrv/markdown-to-pdf
  )

(use-package markdown-mode
  :mode ("\\.m[k]d\\'" . markdown-mode)
  :init
  ;; I get compiler warning if I write:
  ;; (make-variable-buffer-local 'markdown-command)
  ;; so do the same thing in a circuitous way
  :config
  (setq markdown-command "pandoc --ascii -f markdown -t html")
  :hook
  (markdown-mode .
                 (lambda ()
                   (when (string-match "\.[pP]md$" (buffer-name))
                     ;; run Pweave before running pandoc
                     (make-local-variable 'markdown-command)
                     (setq markdown-command "pwv-pandoc"))))
  :bind
  (:map markdown-mode-map
        ("C-c C-c b" . jrv/markdown-export-blog)
        ("C-c C-c P" . jrv/markdown-to-pdf)
        ("C--" . (lambda () (interactive) (insert "&ndash;"))))
)
;;; Python mode
;; emacs packages desired are:    elpy, flycheck
;; related emacs packages (not used): py-autopep8, ein
;; python pip packages desired are
;;       rope_py3k, jedi, flake8, importmagic, autopep8, yapf

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--TerminalInteractiveShell.simple_prompt=True")
  
  )

(use-package jrv-python-functions
  :commands
  jrv/python-insert-docstring
  jrv/python-flycheck
  jrv/python-help
  )

(use-package elpy :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config 
  (setq elpy-shell-use-project-root nil)
  :hook
  (elpy-mode . (lambda ()
                 (when (require 'flycheck nil t)
                   (setq elpy-modules
                         (delq 'elpy-module-flymake elpy-modules))
                   (jrv/python-flycheck))))
  :bind (:map python-mode-map
              ("C-c C-h" . jrv/python-help)
              ("C-c d" . jrv/python-insert-docstring))
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
  jrv/notmuch-get-my-mails
  jrv/notmuch-forward-inline
  jrv/notmuch-rss-url-browse
  jrv/notmuch-config
  jrv/notmuch-open-link
  jrv/notmuch-mimedown-ask
  jrv/notmuch-de-fill
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
  (setq mail-signature-file jrv/settings-email-signature-file)
  (setq notmuch-address-command "my-nottoomuch-addresses.sh")
  (setq mailcap-mime-data
        '(("*" (".*" (viewer . "xdg-open \"%s\"") (type . "*/*")))))
  (setq notmuch-fcc-dirs jrv/settings-notmuch-fcc-dirs)
  (mailcap-parse-mailcaps)
  (setq mm-text-html-renderer 'w3m)
  (setq mm-default-directory jrv/settings-mm-default-directory)
  (setq mm-html-inhibit-images t)
  :bind (:map notmuch-tree-mode-map
              ("C-<return>" .
               (lambda ()
                 "Show and switch to current message in message pane"
                 (interactive)
                 (notmuch-tree-show-message-in)
                 (select-window notmuch-tree-message-window)))
              ("g" . jrv/notmuch-get-my-mails)
              ("F" . jrv/notmuch-forward-inline)
              ("]" . jrv/notmuch-rss-url-browse)
              ("~" . jrv/notmuch-config)
              ("!" . (lambda ()
                       "toggle unread tag for message"
                       (interactive)
                       (if (member "unread" (notmuch-tree-get-tags))
                           (notmuch-tree-tag '("-unread"))
                         (notmuch-tree-tag '("+unread")))))
              :map notmuch-show-mode-map
              ("F" . jrv/notmuch-forward-inline)
              ("]" . jrv/notmuch-open-link)
              ("~" . jrv/notmuch-config)
              ("C-c C-o" . goto-address-at-point)
              )
  :bind (:map notmuch-message-mode-map
              ("C-c i" .
               (lambda () "Choose Identity Interactively." (interactive)
                 (gnus-alias-use-identity
                  (ido-completing-read
                   "Choose Identity: "
                   (mapcar 'car jrv/settings-email-addresses)))))
              ("M-q" . jrv/notmuch-de-fill)
              ("C-c C-c" . jrv/notmuch-mimedown-ask)
              )

  )

(use-package message
  :after notmuch
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
  (setq message-auto-save-directory jrv/settings-message-auto-save-directory)
  (setq message-send-mail-partially-limit nil)
  (setq message-sendmail-envelope-from 'header)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-forward-as-mime t)
  :hook
  (message-send-mail . jrv/notmuch-feed-msmtp)
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
  (defun make-identity-alist (x)
    "Helper function to make gnus-alias-identity-alist

     Use (mapcar 'make-identity-alist jrv/settings-email-addresses)
     to make gnus-alias-identity-alist"
    (list (elt x 0)    ;; the identity alias
          nil          ;; Does not refer to any other identity
          (concat jrv/settings-email-real-name " <" (elt x 1) "> ")   ;; Sender address
          nil          ;; No organization header
          nil          ;; No extra headers
          nil          ;; No extra body text
          jrv/settings-email-signature-file
          ))
  (setq gnus-alias-identity-alist
        (mapcar 'make-identity-alist jrv/settings-email-addresses))

  ;; default identity
  (setq gnus-alias-default-identity "i")
  ;; Define rules to match gmail identity
  (defun make-identity-rules (x)
    "Helper function to make gnus-alias-identity-rules

     Use (mapcar 'make-identity-rules jrv/settings-email-addresses)
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
        (mapcar 'make-identity-rules jrv/settings-email-addresses))
  )

(use-package sendmail
  :after notmuch
  :config
  (setq sendmail-program "~/bin/pymsmtpq") 
  )

(use-package csv-mode
  :mode "\\.[Cc][Ss][Vv]\\'"
  )

(use-package epa
  :config
  (epa-file-enable)
  )

(use-package jrv-pweave
  :mode 
  ("\\.Plw\\'" . Plw-mode)
  ("\\.pmd\\'" . pmd-mode)
  )

(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :config
  (setq yas-snippet-dirs
        (list jrv/settings-yas-snippets-dir)) ; set snippets fold
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
  :commands
  jrv/finish-main
  jrv/finish-std-windows
  jrv/finish-mail-windows
  )

;; (use-package-report)




