;;; setup-languages.el --- Config about programming langauges -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(use-package common-lisp-modes
  :straight '(common-lisp-modes :type git :host gitlab :repo  "andreyorst/common-lisp-modes.el"))

(use-package python-ts-mode
  :preface (defun os/setup-python-environment ()
             "Custom configuration for Python mode."
             (yas-minor-mode 1)
             ;; Set the `python-shell-interpreter' to the python in PATH.
             ;; At this moment `envrc' should succesfully configure environment.
             (setq-local python-shell-interpreter (executable-find "python")))
  :hook (python-ts-mode . os/setup-python-environment))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind ( :map markdown-mode-map
          ("M-Q" . split-pararagraph-into-lines))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "pandoc")
  (markdown-hr-display-char nil)
  (markdown-list-item-bullets '("-")))

(use-package elisp-mode
  :hook ((emacs-lisp-mode . eldoc-mode)
         (emacs-lisp-mode . common-lisp-modes-mode)))

(use-package csv-mode
  :ensure t
  :hook ((csv-mode . csv-guess-set-separator))
  :custom
  (csv-align-max-width most-positive-fixnum))

(use-package cc-mode
  :hook (c-mode-common . cc-mode-setup)
  :custom
  (c-basic-offset 4)
  (c-default-style "linux")
  :config
  (defun cc-mode-setup ()
    (c-set-offset 'case-label '+)
    (setq-local comment-start "//"
                comment-end ""
                tab-width 4)))

(use-package yaml-mode
  :ensure t
  :defer t
  :custom
  (yaml-indent-offset 4))

(use-package docker
  :ensure t)

(use-package dockerfile-mode
  :ensure t)


(use-package csv-mode
  :ensure t
  :hook ((csv-mode . csv-guess-set-separator))
  :custom
  (csv-align-max-width most-positive-fixnum))

(use-package lisp-mode
  :hook ((lisp-mode lisp-data-mode) . common-lisp-modes-mode))

(use-package sql-indent
  :defer t
  :ensure t)

(use-package sqlite-mode-extras
  :straight '(sqlite-mode-extras :type git :host github :repo "xenodium/sqlite-mode-extras")
  :defines sqlite-mode-map
  :hook ((sqlite-mode . sqlite-extras-minor-mode))
  :bind (:map
         sqlite-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("<backtab>" . sqlite-mode-extras-backtab-dwim)
         ("<tab>" . sqlite-mode-extras-tab-dwim)
         ("RET" . sqlite-mode-extras-ret-dwim)))


(use-package groovy-mode
  :straight '(groovy-mode :type git :host github :repo "Groovy-Emacs-Modes/groovy-emacs-modes")
  :mode "\\.gradle\\'"                ; if you want this mode to be auto-enabled
  )


(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))


(provide 'setup-languages)
;;; setup-languages.el ends here
