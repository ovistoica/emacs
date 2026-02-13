;;; setup-markdown-mode --- Markdown mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Native code block fontification with clj and other modes

;;; Code:

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-fontify-code-blocks-natively t)
  :config
  ;; Add 'clj' as an alias for 'clojure' in markdown code blocks
  (add-to-list 'markdown-code-lang-modes '("clj" . clojure-mode)))

(provide 'setup-markdown-mode)
;;; setup-markdown-mode.el ends here
