;;; setup-org.el --- Org setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(use-package org
  :preface


  :hook ((org-babel-after-execute . org-redisplay-inline-images)
         (org-mode . org-indent-mode))
  :bind (("C-c A" . org-agenda)
         ("C-c C-l" . org-store-link)
         :map org-mode-map
         ("C-c l" . org-store-link))
  :custom
  ;; Your existing custom settings remain unchanged
  (org-M-RET-may-split-line '((default . nil)))
  (org-support-shift-select t)
  (org-highlight-latex-and-related '(latex))
  (org-preview-latex-default-process 'dvisvgm)
  (org-src-fontify-natively t)
  (org-confirm-babel-evaluate nil)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-image-actual-width nil)
  (org-src-preserve-indentation t)
  (org-agenda-files (list
                     "~/org/todo_agenda.org"
                     "~/workspace/voice-fn/TODO.org"
                     "~/Dropbox/todo/todo.org"))
  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)
  (add-to-list 'org-src-lang-modes '("typescript" . typescript-ts))
  (add-to-list 'org-src-lang-modes '("ts" . typescript-ts))
  (setq org-special-ctrl-a/e t)
  (setq org-insert-heading-respect-content t)

  ;; Org styling, hide markup etc
  (setq org-hide-emphasis-markers t)
  (setq org-pretty-entities t)
  (setq org-agenda-tags-column 0)
  ;; Ellipsis styling
  (setq org-ellipsis "...")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  :config
  (defun org-babel-edit-prep:emacs-lisp (_)
    "Setup Emacs Lisp buffer for Org Babel."
    (setq lexical-binding t))
  (unless (version<= org-version "9.1.9")
    (add-to-list 'org-modules 'org-tempo)))

(use-package ox-gfm
  :after org
  :ensure t)

(use-package org-modern
  :disabled t
  :init (global-org-modern-mode)
  :config
  (setq org-modern-star 'replace)
  (setq org-modern-timestamp nil))

(use-package org-pomodoro
  :ensure t)

(use-package ob-shell :after org)

(use-package org-capture
  :bind ( :map mode-specific-map
          ("o c" . org-capture)))

(use-package dslide
  :straight '(dslide :type git :host github
                     :repo "positron-solutions/dslide"))

(use-package ol
  :after org-capture
  :functions (org-link-set-parameters)
  :preface
  (defun blog-follow-html-link (path arg)
    (funcall browse-url-browser-function path arg))
  (defun blog-export-hmtl-link (path description _backend _properties)
    "Export link directly to HTML."
    (format "<a href=\"%s\">%s</a>" path (or description path)))
  (defun blog-create-html-link (&optional _)
    "Create a file link using completion."
    (let ((link (read-string "Link: ")))
      (concat "blog-html:" link)))
  :config
  (org-link-set-parameters
   "org"
   :export #'blog-export-static-org-link
   :complete #'blog-create-static-org-link)
  (org-link-set-parameters
   "blog-html"
   :follow #'blog-follow-html-link
   :export #'blog-export-hmtl-link
   :complete #'blog-create-html-link))


(use-package ox-latex
  :after ox)

(provide 'setup-org)
;;; setup-org.el ends here
