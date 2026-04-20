;;; setup-hl-todo.el --- Config for highlighting TODO keywords -*- lexical-binding: t; -*-

;;; Commentary:
;; Capture comments with TODO kws

;;; Code:

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode +1))

(provide 'setup-hl-todo)
;;; setup-hl-todo.el ends here
