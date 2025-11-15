;;; setup-beancount.el --- Configuration for beancount -*- lexical-binding: t -*-

;;; Commentary:
;; Beancount is a text based accounting system that works with double entry bookkeeping

(use-package beancount
  :vc (:url "https://github.com/beancount/beancount-mode")
  :mode ("\\.beancount\\'" . beancount-mode)
  :bind ((:map beancount-mode-map
               ("C-c C-n" . beancount-goto-next-transaction)
               ("C-c C-p" . beancount-goto-previous-transaction))))

(provide 'setup-beancount)
