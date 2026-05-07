;;; setup-omarchy.el --- Wire up the omarchy.el package  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Glue: configures defaults and runs `omarchy-init' to sync Emacs with
;; the current Omarchy system theme and font.  The package lives at
;; `~/Work/omarchy.el' (local checkout) and is on the load-path via
;; `init.el'.  `omarchy-themes' is required so the bundled themes
;; (rose-pine, osaka-jade, flexoki-light, catppuccin-mocha, etc.)
;; register their directory on `custom-theme-load-path'.

;;; Code:

(use-package omarchy
  :ensure nil
  :demand t
  :commands (omarchy-theme-pick omarchy-font-pick
             omarchy-apply-theme omarchy-apply-font
             omarchy-toggle-nightlight omarchy-toggle-waybar
             omarchy-screenshot omarchy-lock-screen
             omarchy-terminal-at-cwd omarchy-install-hooks)
  :init
  (setq omarchy-default-theme 'default-black
        omarchy-default-font  "Iosevka Nerd Font Mono")
  :config
  (require 'omarchy-themes)
  (omarchy-init))

(provide 'setup-omarchy)
;;; setup-omarchy.el ends here
