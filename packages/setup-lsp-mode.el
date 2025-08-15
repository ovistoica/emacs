(use-package lsp-mode
  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)

       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?) ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection)) ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :hook ((clojure-mode . lsp)
         (clojurescript-mode . lsp)
         (clojurec-mode . lsp)
         (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :diminish " lsp"

  :bind ((:map lsp-mode-map
               ("s-l w l" . lsp-workspace-show-log)))

  :init
  (setq lsp-headerline-breadcrumb-enable nil) ;; Don't need file path in my buffer
  (setq lsp-lens-enable nil) ;; Hide clutter (reference and test counts)
  (setq lsp-enable-indentation nil) ;; use clojure-mode indentation
  (setq lsp-eldoc-enable-hover nil) ;; use CIDER eldoc
  (setq lsp-modeline-code-actions-enable nil) ;; Don't clutter modeline
  (setq lsp-modeline-diagnostics-enable nil) ;; Don't clutter modeline, jeez
  (setq lsp-completion-provider :none) ;; Skip company-mode
  (setq lsp-enable-symbol-highlighting nil) ;; Don't highlight current symbol

  (setq lsp-apply-edits-after-file-operations nil) ;; Disable broken lsp feature: https://github.com/clojure-lsp/clojure-lsp/issues/1813

  (setq lsp-signature-doc-lines 1)      ; Don't raise the echo area. It's distracting
  (setq lsp-ui-doc-use-childframe t)    ; Show docs for symbol at point

  ;; To consider
  ;;
  ;; (setq lsp-enable-completion-at-point nil) ;; CIDER vs LSP?
  ;; (remove-hook 'completion-at-point-functions #'cider-complete-at-point t)

  :config
  (define-key lsp-command-map (kbd "d") #'lsp-ui-doc-glance)
  (advice-add 'lsp--info :around #'my/silence-some-lsp-info-messages)
  (add-hook 'lsp-completion-mode-hook 'my/use-lsp-completion-only-as-fallback)
  ;; (setq lsp-use-plists t)


  )

(use-package lsp-ui
  :after lsp-mode
  :defer t)

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
  :straight '(lsp-pyright :type git :host github :repo "emacs-lsp/lsp-pyright")
  :config (add-to-list 'lsp-pyright-python-search-functions #'os/locate-python-virtualenv)
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook ((python-mode . lsp-pyright-hook)
         (python-ts-mode . lsp-pyright-hook)))


(defun my/use-lsp-completion-only-as-fallback ()
  (when (-contains? completion-at-point-functions #'lsp-completion-at-point)
    (remove-hook 'completion-at-point-functions #'tags-completion-at-point-function t)
    (remove-hook 'completion-at-point-functions #'lsp-completion-at-point t)
    (remove-hook 'completion-at-point-functions t t)
    (add-to-list 'completion-at-point-functions #'lsp-completion-at-point t)
    (add-to-list 'completion-at-point-functions t t)))

(defun my/silence-some-lsp-info-messages (orig-fn &rest args)
  (unless (or (string-equal (car args) "Connected to %s.")
              (string-equal (car args) "Disconnected"))
    (apply orig-fn args)))

(provide 'setup-lsp-mode)
