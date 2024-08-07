;; misc-packages.el --- various customizations and additions of packages  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; This is for packages that _aren't_ built-in to Emacs but aren't
;;;; complex enough to justify a distinct file

;;; Code:
;;; PROJECTILE
;;;; this is at the top because things below require it.
(use-package projectile
  :ensure projectile
  :diminish
  :defines
  projectile-mode-map
  :functions
  projectile-cleanup-known-projects
  projectile-find-file
  projectile-mode
  projectile-parent
  projectile-project-p
  projectile-project-root
  :hook
  (projectile-after-switch-project . os/node-project-setup)
  :bind
  ("C-c a"   . projectile-ag)
  ("C-c C-o" . projectile-multi-occur)
  (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :bind*
  ("C-c C-a" . projectile-ag) ;; fuck you js2-mode
  :init
  (projectile-mode +1)
  :custom
  (projectile-cache-file
   (expand-file-name ".projectile.cache" os/emacs-tmp-dir))
  (projectile-globally-ignored-files '("TAGS" ".git" ".DS_Store"))
  (projectile-known-projects-file
   (expand-file-name "projectile-bookmarks.eld" os/emacs-tmp-dir))
  (projectile-switch-project-action 'projectile-dired)
  :config
  (projectile-cleanup-known-projects))

(use-package whole-line-or-region
  :ensure t
  :init
  (whole-line-or-region-global-mode 1))

(use-package keycast
  :ensure t)


;;; AG
(use-package ag
  :ensure ag
  :commands
  ag
  :bind
  ("C-c C-A" . ag-regexp-project-regexp)
  :custom
  (ag-highlight-search t)
  (ag-reuse-buffers t))



;;; ALLLLLLL THE ICONS
(use-package all-the-icons
  :ensure all-the-icons)

(use-package all-the-icons-completion
  :ensure all-the-icons-completion
  :after (marginalia all-the-icons)
  :functions
  all-the-icons-completion-mode
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :ensure all-the-icons-dired
  :diminish)

(use-package all-the-icons-ibuffer
  :ensure all-the-icons-ibuffer
  :after (ibuffer)
  :functions
  all-the-icons-ibuffer-mode
  :config
  (all-the-icons-ibuffer-mode 1))


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
  ;; https://git.genehack.net/os/emacs/issues/2
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (apheleia-global-mode +1))


;;; API BLUEPRINT
(use-package apib-mode
  :ensure apib-mode
  :mode
  "\\.apib\\'")


;;; BROWSE-KILL-RING
(use-package browse-kill-ring
  :ensure browse-kill-ring
  :commands
  browse-kill-ring
  browse-kill-ring-default-keybindings
  :init
  (browse-kill-ring-default-keybindings))


;;; DIMINISH
;;;; from http://whattheemacsd.com/init.el-04.html
(use-package diminish
  :ensure diminish)



;;; EDITORCONFIG
(use-package editorconfig
  :ensure editorconfig
  :diminish
  :functions
  editorconfig-mode
  :config
  (editorconfig-mode 1))


;;; EMOJIFY
(use-package emojify
  :ensure emojify
  :hook (after-init . global-emojify-mode)
  :custom
  (emojify-display-style 'unicode)
  (emojify-emoji-styles '(unicode))
  (emojify-show-help nil)
  :config
  (when (member "Apple Color Emoji" (font-family-list))
    (message "Loading apple emojis")
    (set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend)
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)))

;;; EXPAND-REGION
(use-package expand-region
  :ensure expand-region
  :bind
  ("C-=" . er/expand-region))


;;; FANCY-COMPILATION MODE
(use-package fancy-compilation
  :ensure fancy-compilation
  :commands
  fancy-compilation-mode)
(with-eval-after-load 'compile (fancy-compilation-mode))


;;; FILLADAPT -- WTF isn't this part of emacs by default by now?!
(use-package filladapt
  :ensure filladapt
  :diminish "fa"
  :functions
  filladapt-mode
  :config
  (filladapt-mode 1))


;;; GIT COMMIT MODE
(use-package git-commit
  :ensure git-commit
  :commands
  git-commit)


;;; GIT-GUTTER
(use-package git-gutter
  :ensure git-gutter
  :diminish ""
  :functions
  global-git-gutter-mode
  :config
  (global-git-gutter-mode t))


;;; HANDLEBARS
(use-package handlebars-mode
  :ensure handlebars-mode
  :commands
  handlebars-mode)


;;; HELPFUL
(use-package helpful
  :ensure helpful
  :bind
  ("C-c h" . helpful-at-point))


;;; JSON
(use-package json-mode
  :ensure json-mode
  :after (add-node-modules-path)
  :commands
  json-mode
  json-ts-mode
  :hook
  (json-mode . add-node-moules-path)
  (json-ts-mode . add-node-moules-path)
  :mode
  "\\.json\\'"
  :config
  (add-to-list 'safe-local-variable-values '(json-mode-indent-level . 2))
  (add-to-list 'safe-local-variable-values '(json-mode-indent-level . 4))
  (setq-default json-mode-indent-level 2))

(use-package json-navigator
  :ensure json-navigator
  :after json-mode)

(use-package jq-format
  :ensure jq-format
  :after json-mode)

;;; JAVASCRIPT / NODE / TYPESCRIPT PACKAGES
(use-package add-node-modules-path
  :ensure add-node-modules-path
  :commands
  add-node-modules-path)

;;; KOLON-MODE
(use-package kolon-mode
  :ensure kolon-mode
  :commands
  kolon-mode)

;;; LIGATURES
(defvar os/path-to-ligature-repo (expand-file-name "~/src/ligature.el")
  "Path to ligature repo.")
(use-package ligature
  :if (file-exists-p os/path-to-ligature-repo)
  :load-path os/path-to-ligature-repo
  :defines
  global-ligature-mode
  ligature-set-ligatures
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package magit
  :ensure t)

;;; MARKDOWN
(use-package markdown-mode
  :ensure markdown-mode
  :commands
  markdown-mode
  :defines
  markdown-mode-map
  :magic
  "\\.mr?kd")

(use-package html-to-markdown
  :ensure html-to-markdown
  :commands
  html-to-markdown-string)


;;; MOVE TEXT
(use-package move-text
  :ensure move-text
  :functions
  move-text-default-bindings
  :config
  (move-text-default-bindings))

;;; NODEJS-REPL
(use-package nodejs-repl
  :ensure nodejs-repl
  :commands
  nodejs-repl)


;;; NVM
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



(use-package vterm
  :ensure vterm
  :config
  (setq shell-file-name "/bin/zsh"
        vterm-max-scrollback 5000))

;;; SASS-MODE
(use-package sass-mode
  :ensure sass-mode
  :mode "\\.s[ac]ss\\'")



;;; SVELTE-MODE
(use-package svelte-mode
  :ensure svelte-mode
  :commands
  svelte-mode
  :hook
  (svelte-mode . add-node-modules-path))


;;; TEMPLATE
(defun os/enable-template-minor-mode ()
  "Turn on `template-minor-mode' in *.tt files."
  (if (string-match "\\.tt2?\\'" buffer-file-name)
      (template-minor-mode 1)))

(use-package template-mode
  :ensure genehack-perl-elisp
  :commands
  template-minor-mode
  :hook
  (html-mode .  os/enable-template-minor-mode))


;;; TERRAFORM
(declare-function terraform-format-on-save-mode "terraform-mode")
(use-package terraform-mode
  :ensure terraform-mode
  :commands
  terraform-mode
  terraform-format-on-save-mode
  :hook
  (terraform-mode . terraform-format-on-save-mode))

(use-package ansi-color
  :defines ansi-color-compilation-filter)

(use-package compile
  :defines
  compilation-filter-hook)

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;;; UNICODE-FONTS
(use-package unicode-fonts
  :ensure unicode-fonts
  :commands
  unicode-fonts-setup
  :config
  (unicode-fonts-setup))


;;; WEB-BEAUTIFY
(use-package web-beautify
  :ensure web-beautify
  :commands
  web-beautify-css
  web-beautify-js
  web-beautify-html
  :config
  (defvar css-mode-map)
  (defvar js2-mode-map)
  (defvar json-mode-map)
  (defvar sgml-mode-map)
  (defvar web-mode-map)
  (eval-after-load 'css-mode  '(define-key css-mode-map  (kbd "C-c b") 'web-beautify-css))
  (eval-after-load 'js2-mode  '(define-key js2-mode-map  (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'json-mode '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'sgml-mode '(define-key sgml-mode-map (kbd "C-c b") 'web-beautify-html))
  (eval-after-load 'web-mode  '(define-key web-mode-map  (kbd "C-c b") 'web-beautify-html)))


;;; WEB-MODE
(use-package web-mode
  :ensure web-mode
  :mode
  "\\.\\(html\\|tx\\|liquid\\)\\'"
  :hook
  ((web-mode . add-node-modules-path)
   (web-mode . (lambda () (turn-off-smartparens-mode)))
   (web-mode . (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))
  :init
  (add-to-list 'safe-local-variable-values '(web-mode-code-indent-offset   . 2))
  (add-to-list 'safe-local-variable-values '(web-mode-css-indent-offset    . 2))
  (add-to-list 'safe-local-variable-values '(web-mode-markup-indent-offset . 2))
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-comment-interpolation t)
  (web-mode-markup-indent-offset 2))

;; in order to be able to use web-mode for *.vue files, but only run
;; the Vetur vls server for *.vue files and not _all_ web-mode files,
;; i made this new mode that's just a copy of web-mode, and then i map
;; the mode to vls in eglot. this is somewhat convoluted, but it works.
(define-derived-mode genehack-vue-mode web-mode "ghVue"
  "A major mode derived from `web-mode', for editing .vue files with LSP support.")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . genehack-vue-mode))


;;; WHICH-KEY
(use-package which-key
  :ensure which-key
  :diminish
  :functions
  which-key-mode
  :config
  (which-key-mode))


;;; YAML-MODE
(use-package yaml-mode
  :ensure t
  :commands
  yaml-mode
  :defines
  yaml-mode-map
  yaml-ts-mode-map
  :mode
  "\\.ya?ml\\'"
  :bind
  (:map yaml-mode-map ("RET" . newline-and-indent))
  (:map yaml-ts-mode-map ("RET" . newline-and-indent))
  :config
  (add-to-list 'safe-local-variable-values '(yaml-indent-offset . 4)))

;;; Rest client
(use-package restclient
  :ensure restclient)

(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :diminish t
  :straight '(highlight-indent-guides :type git :host github :repo "DarthFennec/highlight-indent-guides")
  :defines
  highlight-indent-guides-method
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;;; SMARTPARENS
(use-package smartparens
  :ensure smartparens
  :diminish
  :functions
  show-smartparens-global-mode
  smartparens-global-mode
  turn-off-smartparens-mode
  :bind

  ("M-{" . sp-wrap-curly)
  ("M-[" . sp-wrap-square)
  ("M-'" . sp-raise-sexp)
  ("M-(" . sp-wrap-round)
  ("M-J" . sp-forward-barf-sexp)
  ("M-K" . sp-forward-slurp-sexp)
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode 1)
  (show-smartparens-global-mode t))

(use-package sicp
  :ensure t)

(use-package logview
  :ensure t
  :mode ("\\.log\\'" . logview-mode))

(use-package gif-screencast
  :straight '(emacs-gif-screencast :type git :host gitlab :repo "Ambrevar/emacs-gif-screencast")
  :config
  (setq gif-screencast-args '("-x")    ; To shut up th eshutter sound of `screencapture' (see `gif-screencast-command')
        gif-screencast-cropping-program "mogrify" ;; Optional: Used to crop the capture to the Emacs frame.
        gif-screencast-capture-format "ppm" ;; Optional: Required to crop captured images.
        gif-screencast-output-directory "~/Desktop"
        ))


(use-package npm-mode
  :straight '(npm-mode :type git :host github :repo "mojochao/npm-mode")
  :init (npm-global-mode))

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

(provide 'misc-packages)
;;; misc-packages.el ends here
