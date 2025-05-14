;; -*- lexical-binding: t -*-
;; #+options: prop:t
;;; init.el --- My personal emacs configuration.
;;; Author: Ovidiu Stoica
;;; Commentary:
;;; The entire configuration is kept here.
;;; Currently written for Emacs 30.
;;; Code:

;; * PACKAGE MANAGEMENT

(defmacro comment (&rest body)
  "Ignore forms in BODY, returning nil. Used for rich comments."
  nil)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; DEBUG
;; (setq debug-on-error t)
;; (setq debug-on-quit t)


;; ** ELPA
(require 'package)


(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Don't show byte compilation warnings when installing packages
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(require 'use-package)

(use-package use-package
  :no-require
  :custom
  (use-package-enable-imenu-support t))

(defvar os/emacs-tmp-dir (concat user-emacs-directory "tmp/")
  "Scratch space for stuff...")

(setq package-user-dir (concat user-emacs-directory "elpa"))
(setq package-native-compile t)

;; ** STRAIGHT
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq config-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path config-dir)

;; * PRIVATE CONFIG

(defvar os/private-config-file (concat user-emacs-directory "emacs-private.el")
  "File with configuration info that can't be in public repository.")
(if (file-readable-p os/private-config-file)
    (progn
      (load-library os/private-config-file)
      (message "Loaded private config")))

;; * BUILTIN


(use-package early-init
  :no-require
  :unless (featurep 'early-init)
  :config
  (load-file (locate-user-emacs-file "early-init.el")))







(use-package emacs
  :config
  (ffap-bindings)
  (push (expand-file-name "plugins/" user-emacs-directory) load-path)
  (push (expand-file-name "lisp/" user-emacs-directory)  load-path))

;; ** MAC Stuff
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))




;; ** FUNCTIONS

(require 'setup-functions)

;; ** DEFAULTS


;; * WINDOW MANAGEMENT
;; ** WINDMOVE



;; ** POPPER
(defvar os/occur-grep-modes-list '(occur-mode
                                   grep-mode
                                   xref--xref-buffer-mode
                                   ivy-occur-grep-mode
                                   ivy-occur-mode
                                   locate-mode
                                   flymake-diagnostics-buffer-mode
                                   rg-mode)
  "List of major-modes used in occur-type buffers.")

;; This does not work at buffer creation since the major-mode for
;; REPLs is not yet set when `display-buffer' is called, but is
;; useful afterwards
(defvar os/repl-modes-list '(matlab-shell-mode
                             eshell-mode
                             geiser-repl-mode
                             shell-mode
                             eat-mode
                             vterm-mode
                             inferior-python-mode
                             cider-repl-mode
                             fennel-repl-mode
                             jupyter-repl-mode
                             inferior-ess-julia-mode)
  "List of major-modes used in REPL buffers.")

(defvar os/repl-names-list
  '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
    "\\*.*REPL.*\\*"
    "\\*MATLAB\\*"
    "\\*Python\\*"
    "^\\*jupyter-repl.*?\\(\\*\\|<[[:digit:]]>\\)$"
    "\\*Inferior .*\\*$"
    "^\\*julia.*\\*$"
    "\\*cider-repl.*\\*$"
    "\\*ielm\\*"
    "\\*nodejs\\*"
    "\\*edebug\\*")
  "List of buffer names used in REPL buffers.")

(defvar os/help-modes-list '(helpful-mode
                             help-mode
                             pydoc-mode
                             TeX-special-mode)
  "List of major-modes used in documentation buffers.")

(defvar os/man-modes-list '(Man-mode woman-mode)
  "List of major-modes used in Man-type buffers.")

(defvar os/message-modes-list '(compilation-mode
                                edebug-eval-mode)
  "List of major-modes used in message buffers.")

(use-package popper
  :straight '(:type git :host github :repo "karthink/popper")
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        (append os/help-modes-list
                os/man-modes-list
                os/repl-modes-list
                os/repl-names-list
                os/occur-grep-modes-list
                ;; my/man-modes-list
                '(Custom-mode
                  compilation-mode
                  messages-buffer-mode)
                '(("^\\*Warnings\\*$" . hide)
                  ("^\\*Compile-Log\\*$" . hide)
                  "^\\*Matlab Help.*\\*$"
                  ;; "^\\*Messages\\*$"
                  "^\\*Backtrace\\*"
                  "^\\*evil-registers\\*"
                  "^\\*Apropos"
                  "^Calc:"
                  "^\\*eldoc\\*"
                  "^\\*TeX errors\\*"
                  "^\\*ielm\\*"
                  "^\\*TeX Help\\*"
                  "^\\*ChatGPT\\*"
                  "^\\*Input History\\*"
                  "^\\*gptel-ask\\*"
                  "^\\*clojure-compilation\\*"
                  "\\*Shell Command Output\\*"
                  "\\*Async Shell Command\\*"
                  "\\*[cC]ausal-[^*]+\\*"
                  ("\\*Detached Shell Command\\*" . hide)
                  "\\*Completions\\*"
                  ;; "\\*scratch.*\\*$"
                  "[Oo]utput\\*")))
  (add-to-list 'display-buffer-alist '("\\*ielm\\*"
                                       (display-buffer-in-side-window)
                                       (side . left)
                                       (window-width . 50)))
  (add-to-list 'display-buffer-alist '("\\*cider-repl.*\\*$"
                                       (display-buffer-in-side-window)
                                       (side . left)
                                       (window-width . 50)))
  (add-to-list 'display-buffer-alist
               '("^\\*cider-repl .+\\*"
                 (display-buffer-in-side-window)
                 (side . left)
                 (window-width . 70)))
  (add-to-list 'display-buffer-alist
               '("^\\*cider-repl .*\\*"
                 (display-buffer-in-side-window)
                 (side . left)
                 (window-width . 120)))
  (popper-mode +1)
  (popper-echo-mode +1))

;; ** CORE PACKAGES

(require 'setup-core)




(use-package json-hs-extra
  :after json
  :hook (json-ts-mode . json-hs-extra-setup)
  :preface
  (defun json-hs-extra-create-overlays (overlay)
    "Creates overlays for block beginning, hiding whitespace.
Sets OVERLAY `json-hs-extra-overlays' property to the list of created
overlays."
    (let ((end (point)))
      (save-excursion
        (forward-sexp -1)
        (when-let ((overlays (ov-regexp "{[[:space:]\n]*" (point) end)))
          (mapc (lambda (ov) (overlay-put ov 'display "{")) overlays)
          (overlay-put overlay 'json-hs-extra-overlays overlays)))))
  (defun json-hs-extra-delete-overlays (fn overlay)
    "Deletes overlays for block beginning created earlier.
Deletes overlays in the `json-hs-extra-overlays' property of OVERLAY,
created with `json-hs-extra-create-overlays'."
    (mapc #'delete-overlay (overlay-get overlay 'json-hs-extra-overlays))
    (funcall fn overlay))
  (defun json-hs-extra-setup ()
    "Special settings for JSON buffers."
    (setq-local hs-block-start-regexp "\\(?:{[[:space:]\n]*\\|\\[\\)"
                hs-set-up-overlay #'json-hs-extra-create-overlays))
  (provide 'json-hs-extra)
  :config
  (advice-add 'delete-overlay :around #'json-hs-extra-delete-overlays))



(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

(use-package flyspell
  :ensure t
  :when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
  :hook ((org-mode git-commit-mode markdown-mode) . flyspell-mode))




;; TODO Document on this further, possibly disable LSP on very long files
(use-package so-long
  :init (global-so-long-mode 1))


;; * COMPLETION

(require 'setup-completions)

(use-package consult
  :ensure t
  :defines
  consult-customize
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)            ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)         ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)    ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline)     ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find) ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)  ;; orig. next-matching-history-element
         ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package consult-lsp
  :after consult
  :straight '(consult-lsp :type git :host github :repo "gagbo/consult-lsp"))



;; * FORMATTING

;;; APHELEIA
;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure apheleia
  :diminish ""                          ; Don't show in modeline
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "-i=4" "-sr" "-kp"))
  (setq apheleia-log-debug-info t)
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'standard-clojure apheleia-formatters)
        '("standard-clj" "fix" "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'clojure-mode apheleia-mode-alist) 'standard-clojure)
  (setf (alist-get 'clojurec-mode apheleia-mode-alist) 'standard-clojure)
  (setf (alist-get 'clojurescript-mode apheleia-mode-alist) 'standard-clojure)
  (setf (alist-get 'json-mode apheleia-mode-alist) 'prettier-json)
  (setf (alist-get 'prettier-css apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'css-mode apheleia-mode-alist) 'prettier-css)
  (setf (alist-get 'css-ts-mode apheleia-mode-alist) 'prettier-css)

  (apheleia-global-mode +1))

;; * NERD ICONS

(use-package nerd-icons
  :straight '(nerd-icons :type git :host github :repo "rainstormstudio/nerd-icons.el"))

(use-package nerd-icons-dired
  :straight '(nerd-icons-dired :type git :host github :repo "rainstormstudio/nerd-icons-dired")
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :straight '(nerd-icons-completion :type git :host github :repo "rainstormstudio/nerd-icons-completion")
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :straight '(nerd-icons-corfu :type git :host github :repo "LuigiPiucco/nerd-icons-corfu")
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))



;; * ORG
(require 'setup-org)

;; * WRITING

(use-package writegood-mode
  :ensure t
  :hook ((markdown-mode nroff-mode org-mode
                        mail-mode
                        git-commit-mode)
         . writegood-mode))

;; * LANGUAGES

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


;; ** CLOJURE

(require 'setup-clojure)

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

(use-package highlight-indent-guides
  :diminish ""
  :straight '(highlight-indent-guides :type git :host github :repo "DarthFennec/highlight-indent-guides")
  :defines
  highlight-indent-guides-method
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character))


(use-package hl-todo
  :straight '(hl-todo :type git :host github :repo "tarsius/hl-todo")
  :init (global-hl-todo-mode))

(use-package envrc
  :straight '(envrc :type git :host github :repo "purcell/envrc")
  :init (envrc-global-mode))


(require 'setup-treesitter)
(require 'setup-lsp)
(require 'setup-navigation-editing)
(require 'setup-web)
(require 'setup-project)
(require 'setup-notetaking)



(use-package helpful
  :ensure helpful
  :bind
  ("C-c h" . helpful-at-point)
  ("C-h f" . helpful-callable)
  ("C-h k" . helpful-key)
  ("C-h v" . helpful-variable)
  ("C-h x" . helpful-command))

;; * GIT
(use-package ediff
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (advice-add 'ediff-window-display-p :override #'ignore))

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :hook ((git-commit-mode . flyspell-mode)
         (git-commit-mode . magit-git-commit-insert-branch))
  :bind ( :map project-prefix-map
          ("m" . magit-project-status))
  :custom
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  :preface
  (defun magit-extract-branch-tag (branch-name)
    "Extract branch tag from BRANCH-NAME."
    (let ((ticket-pattern "\\([[:alpha:]]+-[[:digit:]]+\\)"))
      (when (string-match-p ticket-pattern branch-name)
        (upcase (replace-regexp-in-string ticket-pattern "\\1: \n" branch-name)))))
  (defun magit-git-commit-insert-branch ()
    "Insert the branch tag in the commit buffer if feasible."
    (when-let ((tag (magit-extract-branch-tag (magit-get-current-branch))))
      (insert tag)
      (forward-char -1))))

(use-package magit
  :after project
  :config
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit") t))

;; ** GIT-GUTTER
(use-package git-gutter
  :ensure git-gutter
  :diminish ""
  :functions
  global-git-gutter-mode
  :config
  (global-git-gutter-mode t))

(use-package server
  :commands (server-running-p)
  :init
  (unless (server-running-p)
    (server-start)))




(use-package recentf
  :hook (after-init . recentf-mode)
  :defines (recentf-exclude)
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :config
  (add-to-list 'recentf-exclude "\\.gpg\\")
  (dolist (dir (list (locate-user-emacs-file ".cache/")
                     (locate-user-emacs-file "workspace/.cache/")))
    (add-to-list 'recentf-exclude (concat (regexp-quote dir) ".*"))))

;; * COMPILE

(use-package compile
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-scroll-output 'first-error)
  :commands (define-compilation-mode)
  :preface
  (cl-defun compile-add-error-syntax
      (mode name regexp &key file line col (level 'error) hyperlink highlight)
    "Register new compilation error syntax.

Add NAME symbol to `compilation-error-regexp-alist', and then add
REGEXP FILE LINE and optional COL LEVEL info to
`compilation-error-regexp-alist-alist'."
    (or file (error "Missing value for :file keyword"))
    (or line (error "Missing value for :line keyword"))
    (let ((faces '(compilation-info-face
                   compilation-warning-face
                   compilation-error-face))
          (level (cond ((eq level 'info) 0)
                       ((eq level 'warn) 1)
                       ((eq level 'error) 2)
                       (t (error "Mnsupported level type: %S" level))))
          (mode (symbol-name (or mode 'compilation))))
      (add-to-list (intern (concat mode "-error-regexp-alist")) name)
      (add-to-list (intern (concat mode "-error-regexp-alist-alist"))
                   (list name regexp file line col level hyperlink
                         (list highlight (nth level faces))))))
  (defmacro define-project-compilation-mode (base-name &rest body)
    (declare (indent 1))
    (let* ((name (symbol-name base-name))
           (doc-name (capitalize (replace-regexp-in-string "-compilation$" "" name)))
           (current-project-root (intern (concat name "-current-project")))
           (current-project-files (intern (concat name "-current-project-files")))
           (compilation-mode-name (intern (concat name "-mode"))))
      `(progn
         (defvar ,(intern (concat name "-error-regexp-alist")) nil
           ,(concat "Alist that specifies how to match errors in " doc-name " compiler output.
See `compilation-error-regexp-alist' for more information."))
         (defvar ,(intern (concat name "-error-regexp-alist-alist")) nil
           ,(concat "Alist of values for `" (downcase doc-name) "-compilation-error-regexp-alist'.
See `compilation-error-regexp-alist-alist' for more information."))
         (defvar-local ,current-project-root nil
           ,(concat "Current root of the project being compiled.
Set automatically by the `" (symbol-name compilation-mode-name) "'."))
         (defvar-local ,current-project-files nil
           ,(concat "Current list of files belonging to the project being compiled.
Set automatically by the `" (symbol-name compilation-mode-name) "'."))
         (define-compilation-mode ,compilation-mode-name
           ,(concat doc-name " Compilation")
           ,(concat "Compilation mode for " doc-name " output.")
           (setq-local ,current-project-root (project-current t))
           (setq-local ,current-project-files (project-files ,current-project-root))
           ,@body)
         (provide ',compilation-mode-name)))))

(use-package clojure-compilation-mode
  :no-require
  :after compile
  :config
  (defun clojure-compilation-p ()
    (and (require 'clojure-mode nil 'noerror)
         (clojure-project-root-path)
         'clojure-compilation-mode))
  (add-to-list 'project-compilation-modes 'clojure-compilation-p)
  (defun clojure-compilation--split-classpath (classpath)
    "Split the CLASSPATH string."
    (split-string classpath ":" t "[[:space:]\n]+"))
  (defmemo clojure-compilation--get-project-dependencies-memo
      (command _deps-file _mod-time)
    "Call COMMAND to obtain the classpath string.
DEPS-FILE and MOD-TIME are used for memoization."
    (thread-last
      command
      shell-command-to-string
      clojure-compilation--split-classpath
      (seq-filter (lambda (s) (string-suffix-p ".jar" s)))))
  (defun clojure-compilation--get-lein-project-dependencies (root)
    "Obtain classpath from lein for ROOT."
    (let* ((project-file (expand-file-name "project.clj" root))
           (mod-time (file-attribute-modification-time (file-attributes project-file))))
      (clojure-compilation--get-project-dependencies-memo
       "lein classpath" project-file mod-time)))
  (defun clojure-compilation--get-deps-project-dependencies (root)
    "Obtain classpath from deps for ROOT."
    (let* ((project-file (expand-file-name "deps.edn" root))
           (mod-time (file-attribute-modification-time (file-attributes project-file))))
      (clojure-compilation--get-project-dependencies-memo
       "clojure -Spath" project-file mod-time)))
  (defun clojure-compilation-get-project-dependencies (project)
    "Get dependencies of the given PROJECT.
Returns a list of all jar archives."
    (when (bound-and-true-p tramp-gvfs-enabled)
      (let ((root (project-root project)))
        (cond ((file-exists-p (expand-file-name "deps.edn" root))
               (clojure-compilation--get-deps-project-dependencies root))
              ((file-exists-p (expand-file-name "project.clj" root))
               (clojure-compilation--get-lein-project-dependencies root))))))
  (defvar-local clojure-compilation-project-deps nil
    "List of project's dependencies")
  (defvar-local clojure-compilation-project-deps-mod-time nil
    "Accumulated modification time of all project's libraries")
  (define-project-compilation-mode clojure-compilation
    (require 'tramp-gvfs)
    (setq-local clojure-compilation-project-deps
                (clojure-compilation-get-project-dependencies
                 clojure-compilation-current-project))
    (setq-local clojure-compilation-project-deps-mod-time
                (seq-reduce #'+ (mapcar (lambda (f)
                                          (time-to-seconds
                                           (file-attribute-modification-time
                                            (file-attributes f))))
                                        clojure-compilation-project-deps)
                            0)))
  (defun clojure-compilation--find-file-in-project (file)
    "Check if FILE is part of the currently compiled project."
    (if (file-name-absolute-p file)
        file
      (seq-find
       (lambda (s) (string-suffix-p file s))
       clojure-compilation-current-project-files)))
  (defun clojure-compilation--file-exists-jar-p (jar file)
    "Check if FILE is present in the JAR archive."
    (with-temp-buffer
      (when (zerop (call-process "jar" nil (current-buffer) nil "-tf" jar))
        (goto-char (point-min))
        (save-match-data
          (re-search-forward (format "^%s$" (regexp-quote file)) nil t)))))
  (defmemo clojure-compilation--find-dep-memo
      (file _project _deps-mod-time)
    "Find FILE in current project dependency list.
PROJECT and DEPS-MOD-TIME are used for memoizing the call."
    (when (not (string-empty-p file))
      (seq-find (lambda (d)
                  (clojure-compilation--file-exists-jar-p d file))
                clojure-compilation-project-deps)))
  (defun clojure-compilation--find-dep (file)
    "Find FILE in current project dependency list."
    (clojure-compilation--find-dep-memo
     file
     clojure-compilation-current-project
     clojure-compilation-project-deps-mod-time))
  (defun clojure-compilation-filename ()
    "Function that gets filename from the error message.
If the filename comes from a dependency, try to guess the
dependency artifact based on the project's dependencies."
    (when-let ((filename (substring-no-properties (match-string 1))))
      (or (clojure-compilation--find-file-in-project filename)
          (when-let ((dep (clojure-compilation--find-dep filename)))
            (concat (expand-file-name dep) "/" filename)))))
  (compile-add-error-syntax
   'clojure-compilation 'some-warning
   "^\\([^:[:space:]]+\\):\\([0-9]+\\) "
   :file #'clojure-compilation-filename
   :line 2 :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clj-kondo-warning
   "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): warning"
   :file 1 :line 2 :col 3 :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clj-kondo-error
   "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): error"
   :file 1 :line 2 :col 3 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'kaocha-tap
   "^not ok.*(\\([^:]*\\):\\([0-9]*\\))"
   :file #'clojure-compilation-filename
   :line 2 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-fail
   "^.*\\(?:FAIL\\|ERROR\\) in.*(\\([^:]*\\):\\([0-9]*\\))"
   :file #'clojure-compilation-filename
   :line 2 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-reflection-warning
   "^Reflection warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
   :file #'clojure-compilation-filename
   :line 2 :col 3
   :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-performance-warning
   "^Performance warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
   :file #'clojure-compilation-filename
   :line 2 :col 3
   :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-syntax-error
   "^Syntax error .* at (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\))"
   :file #'clojure-compilation-filename
   :line 2 :col 3)
  (compile-add-error-syntax
   'clojure-compilation 'kaocha-unit-error
   "^ERROR in unit (\\([^:]+\\):\\([0-9]+\\))"
   :file #'clojure-compilation-filename
   :line 2 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'eastwood-warning
   "^\\([^:[:space:]]+\\):\\([0-9]+\\):\\([0-9]+\\):"
   :file #'clojure-compilation-filename
   :line 2 :col 3 :level 'warn :hyperlink 1 :highlight 1))


(use-package jdecomp
  :ensure t
  :mode ("\\.class\\'" . jdecomp-mode)
  :preface
  (defvar cfr-path
    (file-truename "~/.local/lib/cfr.jar")
    "Path to the cfr Java decompiler library.")
  (defvar fernflower-path
    (file-truename "~/.local/lib/fernflower.jar")
    "Path to the FernFlower library.")
  :when (or (file-exists-p cfr-path)
            (file-exists-p fernflower-path))
  :custom
  (jdecomp-decompiler-type
   (cond ((file-exists-p cfr-path) 'cfr)
         ((file-exists-p fernflower-path) 'fernflower)))
  (jdecomp-decompiler-paths
   `((cfr . ,cfr-path)
     (fernflower . ,fernflower-path))))



(use-package vterm
  :ensure vterm
  :config
  (setq shell-file-name "/bin/zsh"
        vterm-max-scrollback 5000))


;;;;; Stuff

(setenv "PLENV_ROOT" "/opt/plenv")
(use-package exec-path-from-shell
  :ensure exec-path-from-shell
  :demand
  :functions
  exec-path-from-shell-initialize
  :init
  ;; FIXME seeing if this does anything... (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-variables
   '(
     "CARGO_HOME"
     "GOPATH"
     "GOROOT"
     "MANPATH"
     "NVM_DIR"
     "PATH"
     "PLENV_ROOT"
     "RUSTUP_HOME"
     "SSH_AGENT_PID"
     "SSH_AUTH_SOCK"
     )))

;; * NOTE TAKING



(use-package elfeed
  :straight '(elfeed :type git :host github :repo "skeeto/elfeed")
  :config
  (setq elfeed-feeds
        '(("https://www.daily.co/blog/rss/" blog ai voice-ai)
          ("https://jackrusher.com/feed.xml" blog clojure))))


;; * AI STUFF
;;;----------------------------------------------------------------

(load (expand-file-name "lisp/setup-ai.el" user-emacs-directory))
(load (expand-file-name "plugins/ai-project-agent.el" user-emacs-directory))

;;;----------------------------------------------------------------


;; * MONITORING

(use-package wakatime-mode
  :ensure t
  :diminish ""
  :custom (setq wakatime-api-key os-secret-wakatime-api-key
                wakatime-cli-path "~/.wakatime/wakatime-cli")
  :init (global-wakatime-mode))

(use-package keycast
  :ensure t)


;; * THEMING
(require 'setup-theme)

;; * PACKAGE LINT
(use-package package-lint
  :straight '(package-lint :type git :host github :repo "purcell/package-lint"))

;; * LIFE HACKS
(use-package water-reminder
  :ensure nil
  :hook (after-init . water-reminder-mode))

(provide 'init)
;;; init.el ends here

;; This is not a literate config tangled from an Org-mode document! So I include
;; some file-specific settings to make it easier to parse. Specifically, the
;; outline that you see in this document is represented in the Lisp files as
;; Org-style collapsible outline headings. See [[*OUTLINE MODE][Outline Mode]].

;; Local Variables:
;; outline-regexp: ";; \\*+"
;; page-delimiter: ";; \\**"
;; eval:(outline-minor-mode 1)
;; eval:(outline-hide-sublevels 5)
;; End:
