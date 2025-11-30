;;; Config about formatting -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'tooling)

;;; APHELEIA
;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure t
  :diminish ""                          ; Don't show in modeline
  :bind ((:map global-map
               ("C-c p" . my/format-buffer)))

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
  (setf (alist-get 'css-ts-mode apheleia-mode-alist) 'prettier-css))

(defun apheleia-mode-formatter (&optional mode)
  "Return the formatter for the current major mode, or nil. Special case
for elisp where we don't want it handled by apheleia."
  (require 'apheleia-formatters)
  (let ((mode (or mode major-mode)))
    (when (not (eq mode 'emacs-lisp-mode))
      (or apheleia-formatter (alist-get mode apheleia-mode-alist)))))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max)))

(defun my/format-buffer ()
  "Use apheleia for formatting the buffer if a formatter for apheleia is defined, otherwise use basic formatting."
  (interactive)
  (if-let ((formatter (apheleia-mode-formatter)))
      (apheleia-format-buffer formatter)
    (cleanup-buffer)))

(defun my/format-w-cljfmt ()
  (interactive)
  (let* ((command (concat "cljfmt fix " (buffer-name)))
         (cmd-buffer-name (concat "*Command: " command "*")))
    (when (get-buffer cmd-buffer-name)
      (kill-buffer))
    (quittable-async-shell-command command t)))

(provide 'setup-formatting)
;;; setup-formatting.el ends here
