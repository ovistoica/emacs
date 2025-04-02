;;; setup-web.el --- Packages and configuration for web stuff -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

(use-package sgml-mode
  :hook (sgml-mode . my/setup-html-mode))

(use-package tagedit
  :ensure t
  :hook sgml-mode
  :config
  (tagedit-add-paredit-like-keybindings))

(use-package emmet-mode
  :ensure t
  :hook (sgml-mode css-mode tsx-ts-mode))

(use-package jtsx
  :ensure t
  :config
  (add-hook 'tsx-ts-mode-hook #'my/setup-tsx-mode))

(use-package nodejs-repl
  :ensure nodejs-repl
  :commands
  nodejs-repl)

(use-package nvm
  :ensure nvm
  :commands
  nvm-use
  :functions
  nvm--installed-versions
  :custom
  ;; this bit depends on pulling this in from exec-shell,
  ;; which is done in init.el.
  (nvm-dir (getenv "NVM_DIR")))


(use-package js-pkg-mode
  :straight (:type git :local-repo "~/workspace/js-pkg-mode")
  :init (js-pkg-global-mode 1))

(defvar my/ts-other-file-alist
  '(("\\.test\\.ts$" (".ts" ".tsx"))
    ("\\.test\\.tsx$" (".tsx" ".ts"))
    ("\\.ts$" (".test.ts"))
    ("\\.tsx$" (".test.tsx" ".test.ts"))))

(defun my/setup-html-mode ()
  (sgml-electric-tag-pair-mode 1)
  (apheleia-mode 1))

(defun my/setup-json-mode ()
  (setq js-indent-level 2)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (apheleia-mode 1))

(defun my/setup-css-mode ()
  (setq css-indent-offset 2)
  (electric-indent-mode 1)
  (apheleia-mode 1))

(defun my/setup-ts-base (mode-map)
  (setq ff-other-file-alist #'my/ts-other-file-alist)
  (electric-pair-mode 1)
  (hs-minor-mode 1)
  (apheleia-mode 1)
  )

(defun my/setup-js-mode ()
  (my/setup-ts-base js-ts-mode-map))

(defun my/setup-ts-mode ()
  (my/setup-ts-base typescript-ts-mode-map)
  (setq-local electric-pair-pairs (append electric-pair-pairs '((?\< . ?\>)))))

(defun my/setup-tsx-mode ()
  (my/setup-ts-base tsx-ts-mode-map)
  ;; Cherry-pick features from jtsx-mode
  ;; - autoedit matching tag pairs
  ;; - comment jsx dwim
  ;; - hs-mode extension
  (setq jtsx-enable-jsx-element-tags-auto-sync t)
  (setq-local forward-sexp-function 'jtsx-forward-sexp)
  (keymap-local-set "<remap> <comment-dwim>" 'jtsx-comment-dwim)
  (add-hook 'pre-command-hook 'jtsx-save-buffer-chars-modified-tick nil t)
  (add-hook 'post-command-hook 'jtsx-synchronize-jsx-element-tags -1 t)
  (add-to-list 'hs-special-modes-alist
               `(tsx-ts-mode
                 "{\\|(\\|[[]\\|\\(?:<>\\)\\|<[^/>][^>]*>"
                 "}\\|)\\|[]]\\|</[^>]*>"
                 ;; "/[*/]"
                 "\\({/[/*]\\)\\|\\(/[/*]\\)"
                 jtsx-forward-sexp
                 nil
                 jtsx-hs-find-block-beginning
                 nil
                 jtsx-hs-looking-at-block-start-p)))

(add-hook 'css-ts-mode-hook #'my/setup-css-mode)
(add-hook 'js-json-mode-hook #'my/setup-json-mode)
(add-hook 'js-ts-mode-hook #'my/setup-js-mode)
(add-hook 'typescript-ts-mode-hook #'my/setup-ts-mode)

(provide 'setup-web)
;;; setup-web.el ends here
