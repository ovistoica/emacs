;;; setup-linting --- Lint setup for my emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Linting for various modes is done through flychecheck. LSP mode is
;;; configured to use the same

;;; Code:
;; Flycheck's Emacs Lisp checkers only run in `trusted-content' (Emacs 30+);
;; trust this config so byte-compile diagnostics keep working here.
(add-to-list 'trusted-content (abbreviate-file-name user-emacs-directory))

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

  (global-flycheck-mode)
  (add-hook 'org-mode-hook (lambda () (flycheck-mode -1))))

(provide 'setup-linting)
;;; setup-linting.el ends here
