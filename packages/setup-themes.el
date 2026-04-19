;;; setup-themes --- Theme packages -*- lexical-binding: t -*-

;;; Commentary:
;; Install modus-themes and doom-themes for use with omarchy theme mapping.

;;; Code:

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-disable-other-themes t

        ;; Must be set before `modus-themes-theme' runs: `modus-themes--slant'
        ;; reads this variable at face-spec-evaluation time to decide whether
        ;; comments/docstrings get italic.  Bamboo italicizes comments.
        modus-themes-italic-constructs t))

(use-package doom-themes
  :ensure t)

(use-package catppuccin-theme
  :ensure t)

(provide 'setup-themes)
;;; setup-themes.el ends here
