;;; setup-omarchy.el --- Wire up the omarchy.el package  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Glue: configures defaults and runs `omarchy-init' to sync Emacs with
;; the current Omarchy system theme and font.  `omarchy.el' itself lives
;; under `lisp/omarchy/' at the config root and is on the load-path via
;; `init.el'.

;;; Code:

(use-package omarchy
  :ensure nil
  :demand t
  :commands (omarchy-theme-pick omarchy-font-pick
             omarchy-apply-theme omarchy-apply-font
             omarchy-toggle-nightlight omarchy-toggle-waybar
             omarchy-screenshot omarchy-lock-screen
             omarchy-terminal-at-cwd)
  :init
  (setq omarchy-default-theme 'default-black
        omarchy-default-font  "Iosevka Nerd Font Mono")
  :config
  (omarchy-init))

(provide 'setup-omarchy)
;;; setup-omarchy.el ends here
