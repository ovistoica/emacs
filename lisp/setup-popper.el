;;; setup-popper.el --- Popper configuration -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:


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


(provide 'setup-popper)
;;; setup-popper.el ends here
