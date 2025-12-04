;;; setup-linting --- Lint setup for my emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Linting for various modes is done through flychecheck. LSP mode is
;;; configured to use the same

;;; Code:
(use-package flycheck
  :diminish flycheck-mode
  :defer 2
  :config (setq-default flycheck-disabled-checkers '(html-tidy))
  :init
  ;; Eagerly re-check whenever there are errors.
  ;; When there are no errors, we're happy to wait for a save.
  (add-hook 'flycheck-after-syntax-check-hook
            (lambda ()
              (setq-local flycheck-check-syntax-automatically
                          (if flycheck-current-errors
                              '(save idle-change mode-enabled)
                            '(save mode-enabled)))))

  (global-flycheck-mode))

(provide 'setup-linting)
;;; setup-linting.el ends here
