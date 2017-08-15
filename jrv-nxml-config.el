;;; NXML and XMLPE mode
 
(provide 'jrv-nxml-config)
(require 'my-settings)
(defvar my-minimal) ; from my-settings
(require 'xmlpe)
(defvar nxml-mode-map)

;; Default file extensions for nxml mode
(setq auto-mode-alist 
	(cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|html\\|htm\\)\\'" . nxml-mode) 
		auto-mode-alist)) 
;; (unify-8859-on-decoding-mode) 
(setq magic-mode-alist 
	(cons '("<???xml " . nxml-mode)
	 magic-mode-alist)) 
(fset 'xml-mode 'nxml-mode)

;; Default file extensions for xmlpe mode (minor mode of nxml mode)
(add-to-list 'auto-mode-alist  '("\\.blog" . xmlpe-auto-mode))
(add-to-list 'xmlpe-mode-alist '("\\.blog" . nxml-mode))
;; (add-to-list 'auto-mode-alist  '("\\.php" . xmlpe-auto-mode))
;; (add-to-list 'xmlpe-mode-alist '("\\.php" . nxml-mode))


(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

(setq void-text-area-pointer nil)
(setq xmlpe-invisible-hf t) ; xmlpe artificial header and footer are invisible

;; Some html key bindings in nxml and xmlpe mode
(defun html-key-bindings()
  (define-key nxml-mode-map [(meta f3)] 'xmlpe-view)
  (define-key nxml-mode-map [(control >)] 'a-href)
  (when (require 'jrv-html nil my-minimal)
    (define-key nxml-mode-map [(control \")] 'html-double-quote)
    (define-key nxml-mode-map [(control \')] 'html-single-quote)
    (define-key nxml-mode-map [(control <)] 'html-comment))
)
(add-hook 'nxml-mode-hook  'html-key-bindings)
(add-hook 'xmlpe-auto-mode-hook  'html-key-bindings)

