;;; setup-navigation-editing.el ---  Setup for navigation & editing -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package isearch
  :bind ( :map isearch-mode-map
          ("<backspace>" . isearch-del-char)
          ("<left>" . isearch-edit-string)
          ("<right>" . isearch-edit-string)
          :map minibuffer-local-isearch-map
          ("<left>" . backward-char)
          ("<right>" . forward-char))
  :custom
  (isearch-lazy-highlight t))

(use-package phi-search
  :ensure t
  :defer t)

(use-package avy
  :ensure t
  :bind
  ( :map avy-map
    ("M-w l" . avy-kill-ring-save-whole-line)
    ("M-k l" . avy-kill-whole-line)
    ("M-w r" . avy-kill-ring-save-region)
    ("M-k r" . avy-kill-region)
    ("c" . avy-goto-char-timer)
    ("l" . avy-goto-line)
    ("n" . avy-next)
    ("p" . avy-prev))
  :preface
  (defalias 'avy-map-prefix (make-sparse-keymap))
  (defvar avy-map (symbol-function 'avy-map-prefix)
    "Keymap for characters following \\`M-a'.")
  (define-key global-map (kbd "M-a") 'avy-map-prefix))


(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(bind-key "M-o"         #'other-window)
(bind-key "M-i"         #'consult-imenu)

(use-package isayt
  :straight '(isayt :type git :host gitlab :repo "andreyorst/isayt.el")
  :delight isayt-mode
  :hook (common-lisp-modes-mode . isayt-mode))



(use-package vundo
  :ensure t
  :bind ( :map mode-specific-map
          ("u" . vundo))
  :custom
  (vundo-roll-back-on-quit nil)
  (vundo--window-max-height 10))

;; Predictable undo/redo
(use-package undo-fu
  :ensure t
  :config
  ;; Source: https://github.com/magnars/emacsd-reboot/blob/main/packages/setup-undo-fu.el
  (setq undo-limit 400000               ; 400kb (default is 160kb)
        undo-strong-limit 3000000       ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)      ; 48mb  (default is 24mb)

  :bind (("M-z" . undo-fu-only-undo)
         ("M-Z" . undo-fu-only-redo)
         ("C-M-z" . undo-fu-only-redo-all)))

(use-package goto-chg
  :ensure t
  :disabled t
  :bind ("M-`" . goto-last-change))

(use-package yasnippet
  :ensure t
  :delight yas-minor-mode
  :init (yas-global-mode 1))

(use-package common-lisp-modes
  :delight common-lisp-modes-mode
  :preface
  (defun indent-sexp-or-fill ()
    "Indent an s-expression or fill string/comment."
    (interactive)
    (let ((ppss (syntax-ppss)))
      (if (or (nth 3 ppss)
              (nth 4 ppss))
          (fill-paragraph)
        (save-excursion
          (mark-sexp)
          (indent-region (point) (mark))))))
  :bind ( :map common-lisp-modes-mode-map
          ("M-q" . indent-sexp-or-fill)))

(use-package easy-kill
  :disabled t
  :ensure t
  :bind (([remap mark-sexp] .'mark-sexp)))

(use-package whole-line-or-region
  :ensure t
  :diminish ""
  :init
  (whole-line-or-region-global-mode 1))

(use-package region-bindings
  :straight '(region-bindings :type git :host gitlab :repo "andreyorst/region-bindings.el")
  :commands (region-bindings-mode)
  :preface
  (defun region-bindings-off ()
    (region-bindings-mode -1))
  :hook ((after-init . global-region-bindings-mode)
         ((elfeed-search-mode magit-mode mu4e-headers-mode)
          . region-bindings-off)))

(use-package puni
  :ensure t
  :hook (((common-lisp-modes-mode nxml-mode json-ts-mode prog-mode org-mode) . puni-mode)
         (puni-mode . electric-pair-local-mode))
  :bind ( :map region-bindings-mode-map
          ("(" . puni-wrap-round)
          ("[" . puni-wrap-square)
          ("{" . puni-wrap-curly)
          ("<" . puni-wrap-angle)
          ;; paredit-like keys
          :map puni-mode-map
          ("C-M-f" . puni-forward-sexp-or-up-list)
          ("C-M-b" . puni-backward-sexp-or-up-list)
          ("C-M-t" . puni-transpose)
          ;; slurping & barfing
          ("C-<left>" . puni-barf-forward)
          ("C-}" . puni-barf-forward)
          ("C-<right>" . puni-slurp-forward)
          ("C-)" . puni-slurp-forward)
          ("C-(" . puni-slurp-backward)
          ("C-M-<left>" . puni-slurp-backward)
          ("C-{" . puni-barf-backward)
          ("C-M-<right>" . puni-barf-backward)
          ;; depth chaning
          ("M-r" . puni-raise)
          ("M-<up>" . puni-splice-killing-backward)
          ("M-<down>" . puni-splice-killing-forward)
          ("M-(" . puni-wrap-round)
          ("M-{" . puni-wrap-curly)
          ("M-[" . puni-wrap-square)
          ("M-?" . puni-convolute)
          ("M-R" . puni-splice)
          ("M-S" . puni-split)))

(use-package multiple-cursors
  :ensure t
  :bind
  (("S-<mouse-1>" . mc/add-cursor-on-click)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   :map region-bindings-mode-map
   ("n" . mc/mark-next-symbol-like-this)
   ("N" . mc/mark-next-like-this)
   ("p" . mc/mark-previous-symbol-like-this)
   ("P" . mc/mark-previous-like-this)
   ("a" . mc/mark-all-symbols-like-this)
   ("A" . mc/mark-all-like-this)
   ("s" . mc/mark-all-in-region-regexp)
   ("l" . mc/edit-ends-of-lines)))

(use-package multiple-cursors-core
  :bind
  (( :map mc/keymap
     ("<return>" . nil)
     ("C-&" . mc/vertical-align-with-space)
     ("C-#" . mc/insert-numbers))))

(use-package separedit
  :ensure t
  :hook (separedit-buffer-creation . separedit-header-line-setup)
  :bind ( ("C-c '" . separedit)
          :map separedit-mode-map
          ("C-c C-c" . separedit-commit)
          :map edit-indirect-mode-map
          ("C-c '" . separedit))
  :custom
  (separedit-default-mode 'markdown-mode)
  :config
  (nconc (assoc '(";+") separedit-comment-delimiter-alist)
         '(clojure-mode clojurec-mode clojure-script-mode))
  (defun separedit-header-line-setup ()
    (setq-local
     header-line-format
     (substitute-command-keys
      "Edit, then exit with `\\[separedit-commit]' or abort with \\<edit-indirect-mode-map>`\\[edit-indirect-abort]'"))))


(provide 'setup-navigation-editing)
;;; setup-navigation-editing.el ends here
