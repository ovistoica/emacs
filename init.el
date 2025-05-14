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





;; * ORG





(require 'setup-treesitter)
(require 'setup-lsp)
(require 'setup-org)
(require 'setup-navigation-editing)
(require 'setup-web)
(require 'setup-project)
(require 'setup-clojure)
(require 'setup-notetaking)
(require 'setup-shell)
(require 'setup-git)
(require 'setup-languages)
(require 'setup-icons)
(require 'setup-ai)





;; * AI STUFF
;;;----------------------------------------------------------------

(load (expand-file-name "lisp/setup-ai.el" user-emacs-directory))
(load (expand-file-name "plugins/ai-project-agent.el" user-emacs-directory))

;;;----------------------------------------------------------------


;; * MONITORING






;; * THEMING
(require 'setup-theme)

;; * PACKAGE LINT


;; * LIFE HACKS


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
