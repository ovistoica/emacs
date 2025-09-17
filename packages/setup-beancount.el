;;; setup-beancount.el --- Configuration for beancount -*- lexical-binding: t -*-

;;; Commentary:
;; Beancount is a text based accounting system that works with double entry bookkeeping

(use-package beancount
  :straight '(:type git :host github :repo "beancount/beancount-mode")
  :mode ("\\.beancount\\'" . beancount-mode))

(provide 'setup-beancount)
