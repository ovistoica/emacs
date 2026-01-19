;;; setup-avy.el --- Avy configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Jump to visible text using a char-based decision tree.

;;; Code:

(use-package avy
  :bind (("C-'" . avy-goto-char-timer)
         ("M-g g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :config
  (setq avy-timeout-seconds 0.3)
  (setq avy-style 'at-full)
  (setq avy-background t))

(provide 'setup-avy)
;;; setup-avy.el ends here
