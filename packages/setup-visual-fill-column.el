;;; setup-visual-fill-column.el --- Visual fill column configuration -*- lexical-binding: t; -*-

;; This is a replacement for auto-fill-mode so it provides a visual guide and
;; not a hard breakline Useful for reading long lines as you would with
;; auto-filled ones however I don't like using it for writing.

(use-package visual-fill-column
  :ensure t
  :defer t)
