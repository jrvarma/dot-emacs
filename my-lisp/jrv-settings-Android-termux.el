;; <escape> key does not work in termux but <home> translates into "M-[ h"
;; so we use as the god-mode-key
(setq jrv/settings/god-mode-key "M-[ h")

;; in termux, we do not have permissions to create /0/ so we use ~/0/
(setq jrv/settings/symlinks "~/0/")
