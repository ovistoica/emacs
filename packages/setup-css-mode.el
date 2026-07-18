;;; setup-css-mode.el --- CSS + Tailwind LSP config -*- lexical-binding: t; -*-

;;; Commentary:

;;; CSS mode tweaks and the Tailwind CSS LSP client (bundled with lsp-mode).

;;; Code:

(add-hook 'css-mode-hook 'lsp)

(setq css-fontify-colors nil)

;; lsp-tailwindcss is bundled with lsp-mode since 10.0.1 (clients/lsp-tailwindcss.el);
;; the standalone MELPA/GitHub package is archived in emacsattic.
(use-package lsp-tailwindcss
  :ensure nil
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  (setq lsp-css-lint-unknown-at-rules "ignore") ;; ignore @tailwind and @apply (and all other such errors, oops)
  )

(provide 'setup-css-mode)
;;; setup-css-mode.el ends here
