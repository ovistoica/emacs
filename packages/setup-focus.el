;;; setup-focus.el --- Focus utilities -*- lexical-binding: t; -*-

;;; Commentary:

;;; This package handles centering text to make it easy to read and removing
;;; distractions from the current buffer in focus.

;;; Code:

(use-package olivetti
  :config
  (setq olivetti-body-width 0.7
        olivetti-minimum-body-width 80
        olivetti-recall-visual-line-mode-entry-state t))

;; place point at the top when changing pages
(defun my/logos-recenter-top ()
  "Use `recenter' to reposition the view at the top."
  (recenter 0))



(use-package logos
  :commands (logos-update-fringe-in-buffers logos-set-mode-arg)

  :preface
  (defun my/logos-focus-hook ()
    (when logos-focus-mode
      (logos-set-mode-arg 'org-indent-mode -1)
      (logos-set-mode-arg 'display-line-numbers-mode -1)))

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
                logos-olivetti t)

  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)
    (define-key map (kbd "<f9>") #'logos-focus-mode))

  (add-hook 'logos-page-motion-hook #'my/logos-recenter-top)
  (add-hook 'enable-theme-functions #'logos-update-fringe-in-buffers)
  (add-hook 'logos-focus-mode-hook #'my/logos-focus-hook))

(provide 'setup-focus)
;;; setup-focus.el ends here
