;;; setup-lsp.el --- LSP configuration -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

(use-package lsp-mode
  :ensure t
  :diminish "LSP"
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
  (defun os/lsp-gopls-hook ()
    (lsp-register-custom-settings '(("gopls.completeUnimported" t t)
                                    ("gopls.staticcheck" t t))))

  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           json-ts-mode
           js-ts-mode
           java-ts-mode
           go-ts-mode
           python-ts-mode
           prisma-ts-mode
           clojure-mode
           clojurec-mode
           clojurescript-mode
           go-ts-mode) . lsp)
         (go-ts-mode . os/lsp-gopls-hook))
  :config

  (define-key lsp-command-map (kbd "d") #'lsp-ui-doc-glance)
  (setenv "PATH" (concat
                  "/usr/local/bin" path-separator
                  (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-clojure-server-command '("/opt/homebrew/bin/clojure-lsp"))


  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :none) ;; we use Corfu
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil) ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil) ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)           ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)           ; Use xref to find references
  (lsp-auto-configure t)        ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)    ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)        ; This basically means, if you change
                                        ; the branch, should LSP reload
                                        ; instantly - it's a big performance
                                        ; overhead
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)           ; I use prettier
  (lsp-enable-links nil)                 ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)    ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t) ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)  ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)              ; Important to provide full JSX completion
  (lsp-completion-show-kind t)        ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)      ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)    ; Show docs for symbol at point
  (lsp-eldoc-render-all nil) ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)            ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil) ; Related to highlighting, and we defer to treesitter

  :init
  (setq lsp-use-plists t)
  ;; Initiate https://github.com/blahgeek/emacs-lsp-booster for performance
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  :bind
  (:map lsp-mode-map
        ([remap lsp-treemacs-errors-list] . consult-lsp-diagnostics)
        ([remap consult-imenu] . consult-lsp-file-symbols)
        ([remap xref-find-apropos] . consult-lsp-symbols)))

(use-package lsp-tailwindcss
  :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (setq lsp-tailwindcss-major-modes '(clojure-mode tsx-ts-mode css-ts-mode clojurescript-mode clojurec-mode)
        lsp-tailwindcss-experimental-class-regex
        [":class\\s+\"([^\"]*)\""
         ":[\\w-.#>]+\\.([\\w-]*)"
         "tw|yourModule\\(([^)]*)\\)"
         "[\"'`]([^\"'`]*).*?[\"'`]"
         ]
        lsp-tailwindcss-class-attributes ["class" "className" "ngClass" ":class"]))


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

(use-package lsp-eslint
  :demand t
  :after lsp-mode
  :config
  ;; Use latest LSP from VSCode installed
  (setq lsp-eslint-server-command (list "node"
                                        (os/path-with-dynamic-user "/Users/%s/.vscode/extensions/dbaeumer.vscode-eslint-3.0.10/server/out/eslintServer.js")
                                        "--stdio")))

(use-package lsp-java
  :ensure t)

(provide 'setup-lsp)
;;; setup-lsp.el ends here
