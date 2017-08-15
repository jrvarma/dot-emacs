(provide 'jrv-company)
(require 'my-settings)

(add-hook 'after-init-hook 'global-company-mode)
(defvar company-idle-delay)
(setq company-idle-delay 0)
(defvar company-minimum-prefix-length)
(setq company-minimum-prefix-length 2)
(defvar company-selection-wrap-around)
(setq company-selection-wrap-around t)
(declare-function company-auctex-init "company-auctex.el")
(company-auctex-init)
(declare-function global-company-mode "company.el")
(global-company-mode)
(defvar my-dictionary) ; from my-settings
(defvar company-ispell-dictionary)
(setq company-ispell-dictionary my-dictionary)
(defvar ispell-complete-word-dict)
(setq ispell-complete-word-dict my-dictionary)
;; We use dabbrev and ispell in notmuch message (see jrv-notmuch.el).
;; If we want dabbrev in all buffers, uncomment the following:
;; ;; dabbrev will block ispell (and others) unless we used grouped backends
;; ;; so the following will not work
;; ;; (add-to-list 'company-backends 'company-ispell t)
;; ;; Hence, we delete dabbrev and add a grouped backend
;; (defvar company-backends)
;; (delete 'company-dabbrev company-backends)
;; (add-to-list 'company-backends '(company-dabbrev :with company-ispell) t)
