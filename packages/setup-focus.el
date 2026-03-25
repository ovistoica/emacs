;;; setup-focus.el --- Focus utilities -*- lexical-binding: t; -*-

;;; Commentary:

;;; This package handles centering text to make it easy to read and removing
;;; distractions from the current buffer in focus.

;;; Code:

(use-package olivetti)

(use-package logos
  :config
  ;; If you want to use outlines instead of page breaks (the ^L):
  (setq logos-outlines-are-pages t)

  ;; This is the default value for the outlines:
  (setq logos-outline-regexp-alist
        `((emacs-lisp-mode . "^;;;+ ")
          (org-mode . "^\\*+ +")
          (markdown-mode . "^\\#+ +")))

  (setq-default logos-hide-cursor nil
                logos-hide-mode-line t
                logos-hide-header-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch nil
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti nil)

  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)
    (define-key map (kbd "<f9>") #'logos-focus-mode))
  )

(provide 'setup-focus)
;;; setup-focus.el ends here
