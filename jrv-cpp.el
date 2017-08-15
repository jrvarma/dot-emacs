(require 'compile)
(provide 'jrv-cpp)

;; if there is no Makefile in the current directory
;; compile-command is changed to running g++ on the current file
;; with the executable being the file name without extension instead of a.out
(defvar c-mode-base-map)
(add-hook 'c++-mode-hook
  (lambda ()
    (define-key c-mode-base-map (kbd "C-c C-l") 'compile)
    (unless (file-exists-p "Makefile")
      (set (make-local-variable 'compile-command)
           (let ((file (file-name-nondirectory buffer-file-name)))
             (format "g++ -o %s -ansi -pedantic -Wall -Wno-comment -g %s"
                     (file-name-sans-extension file)
                     file))))))
