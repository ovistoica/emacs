;;; setup-lsp --- Config for LSP providers -*- lexical-binding: t -*-

;;; Commentary:

;;; I use LSP mode instead of eglot as it supports multiple LSP providers in the
;;; same buffer - See more comparisons of LSP here:
;;; https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
;;;
;;; Most of the features of lsp-mode are silenced to match the emacs way.
;;;
;;; lsp-booster is used to convert json packets to native elisp bytecode so
;;; emacs can offer faster suggestions.

;;; Code:

(require 'dash)

(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurescript-mode . lsp)
         (clojurec-mode . lsp)
         (python-mode . lsp)
         ;; TypeScript modes
         (typescript-mode . lsp)
         (typescript-ts-mode . lsp)
         (tsx-ts-mode . lsp)
         ;; JavaScript modes
         (js-mode . lsp)
         (js2-mode . lsp)
         (js-ts-mode . lsp)
         ;; Java
         (java-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :diminish " lsp"

  :bind ((:map lsp-mode-map
               ("s-l w l" . lsp-workspace-show-log)))

  :custom
  (lsp-completion-provider :none)  ;; Skip company-mode
  :init
  (setq lsp-headerline-breadcrumb-enable nil) ;; Don't need file path in my buffer
  (setq lsp-lens-enable t) ;; Show reference and test counts
  (setq lsp-enable-indentation nil) ;; use clojure-mode indentation
  (setq lsp-eldoc-enable-hover nil) ;; use CIDER eldoc
  (setq lsp-modeline-code-actions-enable nil) ;; Don't clutter modeline
  (setq lsp-modeline-diagnostics-enable nil) ;; Don't clutter modeline, jeez
  (setq lsp-enable-symbol-highlighting nil) ;; Don't highlight symbol - s l a h for when needed

  (setq lsp-apply-edits-after-file-operations nil) ;; Disable broken lsp feature: https://github.com/clojure-lsp/clojure-lsp/issues/1813

  (setq lsp-signature-doc-lines 1)      ; Don't raise the echo area. It's distracting


  ;; To consider
  ;;
  ;; (setq lsp-enable-completion-at-point nil) ;; CIDER vs LSP?
  ;; (remove-hook 'completion-at-point-functions #'cider-complete-at-point t)

  :config

  (advice-add 'lsp--info :around #'my/silence-some-lsp-info-messages)
  (add-hook 'lsp-completion-mode-hook 'my/use-lsp-completion-only-as-fallback)
  ;; (setq lsp-use-plists t) - disabled as currently getting errors
  )

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-use-childframe t)    ; Show docs for symbol at point
  )

(use-package lsp-ui-doc
  :ensure nil
  :after lsp-mode
  :custom
  (lsp-ui-doc-position 'at-point)  ; Show doc childframe at point, not top-right
  :bind ((:map lsp-command-map
               ("d" . lsp-ui-doc-glance))))

(use-package lsp-pyright
  :preface
  (defun lsp-pyright-hook ()
    (require 'lsp-pyright)
    (lsp))
  (defun os/locate-python-virtualenv ()
    "Find the Python executable based on the VIRTUAL_ENV environment variable."
    (when-let ((venv (getenv "VIRTUAL_ENV")))
      (let ((python-path (expand-file-name "bin/python" venv)))
        (when (file-executable-p python-path)
          python-path))))
  :vc (:url "https://github.com/emacs-lsp/lsp-pyright")
  :config (add-to-list 'lsp-pyright-python-search-functions #'os/locate-python-virtualenv)
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook ((python-mode . lsp-pyright-hook)
         (python-ts-mode . lsp-pyright-hook)))

(use-package lsp-java
  :ensure t
  :defer t)

(defun my/use-lsp-completion-only-as-fallback ()
  "Fallback on lsp completion."
  (when (-contains? completion-at-point-functions #'lsp-completion-at-point)
    (remove-hook 'completion-at-point-functions #'tags-completion-at-point-function t)
    (remove-hook 'completion-at-point-functions #'lsp-completion-at-point t)
    (remove-hook 'completion-at-point-functions t t)
    (add-to-list 'completion-at-point-functions #'lsp-completion-at-point t)
    (add-to-list 'completion-at-point-functions t t)))

(defun my/silence-some-lsp-info-messages (orig-fn &rest args)
  "Apply ORIG-FN LSP messages unless ARGS refer to connection actions."
  (unless (or (string-equal (car args) "Connected to %s.")
              (string-equal (car args) "Disconnected"))
    (apply orig-fn args)))

(provide 'setup-lsp)
;;; setup-lsp.el ends here
