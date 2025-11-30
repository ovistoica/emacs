;;; setup-beancount.el --- Configuration for beancount -*- lexical-binding: t -*-

;;; Commentary:
;; Beancount is a text based accounting system that works with double entry bookkeeping

(use-package beancount
  :vc (:url "https://github.com/beancount/beancount-mode")
  :mode ("\\.beancount\\'" . beancount-mode)
  :bind ((:map beancount-mode-map
               ("C-c C-n" . beancount-goto-next-transaction)
               ("C-c C-p" . beancount-goto-previous-transaction)))
  :config
  (setq beancount-use-ido nil))

(defun beancount-rev-pockets ()
  (beancount-collect-unique beancount-account-regexp 0)
  (seq-filter (lambda (account)
                (string-match-p "Assets:RO:Revolut:Pocket:.*" account))
              my-list))

(defconst my/beancount-rev-checking-accounts-regexp
  (rx "Assets:"
      (? (+ (not ":")) ":")  ; optional country
      (? (+ (not ":")) ":")  ; optional owner
      "Revolut:Checking"
      (? "Joint")
      (? ":" (or "RON" "EUR" "USD"))))

(defconst my/beancount-rev-pocket-accounts-regexp
  (rx "Assets:"
      (? (+ (not ":")) ":")
      (? (+ (not ":")) ":")
      "Revolut:Pocket:"
      (one-or-more alpha)
      (? ":" (or "RON" "EUR" "USD"))))

(defconst my/beancount-expenses-regexp
  (rx "Expenses:"
      (or "Home" "Business")
      ":"
      (one-or-more alpha)
      (? ":" (one-or-more alpha))))


(provide 'setup-beancount)
