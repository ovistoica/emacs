(add-hook 'css-mode-hook 'lsp)

(setq css-fontify-colors nil)

;; lsp-tailwindcss was removed from MELPA; install it from GitHub via package-vc.
(use-package lsp-tailwindcss
  :vc (:url "https://github.com/merrickluo/lsp-tailwindcss" :rev :newest)
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  (setq lsp-css-lint-unknown-at-rules "ignore") ;; ignore @tailwind and @apply (and all other such errors, oops)
  )

(require 'lsp-tailwindcss)

(provide 'setup-css-mode)
