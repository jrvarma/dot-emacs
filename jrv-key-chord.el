(provide 'jrv-key-chord)

;; use key-chord for alternate bindings for function keys
(declare-function key-chord-mode "key-chord")
(eval-when-compile 
  (require 'package)
  (package-initialize))
(declare-function key-chord-mode "key-chord")
(key-chord-mode 1)
(defvar key-chord-one-key-delay)
(defvar key-chord-two-keys-delay)
(setq key-chord-one-key-delay 0.2)
(setq key-chord-two-keys-delay 0.5)
(declare-function key-chord-define-global "key-chord")
(key-chord-define-global "qq" 'ido-switch-buffer)
(key-chord-define-global "QQ" 'switch-buffer-other-window)
(key-chord-define-global "Q!" 'get-real-path)
(key-chord-define-global "ww" 'ido-select-window)

(declare-function read-file-name "minibuffer")
(declare-function confirm-nonexistent-file-or-buffer "files")
(declare-function get-real-path "jrv-mypaths")
(defun jrv-find-file-real-path ()
  (interactive)
  (let* ((path (get-real-path t))
         (dir (file-name-directory path))
         (file (or (file-name-nondirectory path) "")))
    (message "%s    %s" dir file)
    (find-file
     (read-file-name "Find file: " dir nil (confirm-nonexistent-file-or-buffer) file))))
(key-chord-define-global "q1" 'jrv-find-file-real-path)

(defvar key-chord-mode)
(defun toggle-key-chord-mode()
  "Toggle key chord mode"
  (interactive)
  (if key-chord-mode
      (key-chord-mode -1)
    (key-chord-mode 1)))
(defalias 'tkc 'toggle-key-chord-mode)
