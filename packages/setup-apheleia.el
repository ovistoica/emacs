;;; setup-apheleia.el --- Config about formatting -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;;; APHELEIA
;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure apheleia
  :diminish ""                          ; Don't show in modeline
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "-i=4" "-sr" "-kp"))
  (setq apheleia-log-debug-info t)
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'standard-clojure apheleia-formatters)
        '("standard-clj" "fix" "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'clojure-mode apheleia-mode-alist) 'standard-clojure)
  (setf (alist-get 'clojurec-mode apheleia-mode-alist) 'standard-clojure)
  (setf (alist-get 'clojurescript-mode apheleia-mode-alist) 'standard-clojure)
  (setf (alist-get 'json-mode apheleia-mode-alist) 'prettier-json)
  (setf (alist-get 'prettier-css apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'css-mode apheleia-mode-alist) 'prettier-css)
  (setf (alist-get 'css-ts-mode apheleia-mode-alist) 'prettier-css)

  (apheleia-global-mode +1))

(provide 'setup-apheleia)
;;; setup-formatting.el ends here
