;;; setup-omarchy.el --- Wire up the omarchy.el package  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Glue: configures defaults and runs `omarchy-init' to sync Emacs with
;; the current Omarchy system theme and font.  Installed from GitHub via
;; package-vc (`https://github.com/ovistoica/omarchy.el') so it works the
;; same on Linux and macOS — on non-Omarchy systems the package loads
;; cleanly and its interactive commands simply no-op with a message.
;; `omarchy-themes' is required so the bundled themes register their
;; directory on `custom-theme-load-path'.

;;; Code:

(use-package omarchy
  :vc (:url "https://github.com/ovistoica/omarchy.el" :rev :newest)
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
