;;; setup-completions.el --- Completions setup -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     orderless-flex)))

(use-package vertico
  :ensure t
  :custom
  (vertico-count 20)                    ; Number of candidates to display
  (vertico-cycle t) ; Go from last to first candidate and first to last (cycle)?
  (vertico-resize t)
  :bind ( :map vertico-map
          ("M-RET" . vertico-exit-input))
  :hook (after-init . vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))


(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map
              ("<tab>" . corfu-complete)
              ([remap completion-at-point] . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)
  (setq corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0
        corfu-cycle t
        corfu-on-exact-match nil) ;; Don't auto insert

  (corfu-popupinfo-mode 1)   ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))


(use-package corfu-popupinfo
  :bind ( :map corfu-popupinfo-map
          ("M-p" . corfu-popupinfo-scroll-down)
          ("M-n" . corfu-popupinfo-scroll-up))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config (setq corfu-popupinfo-delay '(0.5 . 0.2))
  :custom-face
  (corfu-popupinfo ((t :height 1.0))))

(use-package corfu-terminal
  :ensure t
  :unless (display-graphic-p)
  :commands (corfu-terminal-mode)
  :hook (after-init . corfu-terminal-mode))

(use-package cape
  :ensure t
  :after corfu
  :config
  (setq completion-at-point-functions '(cape-file)))

(use-package ov
  :ensure t
  :commands (ov-regexp))

(use-package abbrev
  :delight abbrev-mode
  :custom
  (save-abbrevs nil))

(use-package which-key
  :ensure which-key
  :diminish
  :functions
  which-key-mode
  :config
  (which-key-mode))

(provide 'setup-completions)
;;; setup-completions.el ends here
