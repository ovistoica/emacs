;; standard-clojure.el

;; Uses `standard-clojure' to apply formatting to clojure buffers


(defun standard-clojure--format-text (text)
  "Format TEXT using standard-clj and return the formatted result."
  (with-temp-buffer
    (insert text)
    (let ((result (shell-command-on-region (point-min) (point-max) "standard-clj fix -" t t)))
      (if (zerop result)
          (let ((formatted-text (buffer-string)))
            formatted-text)
        ;; If formatting fails, return original text
        text))))

(defun standard-clojure--indent-line ()
  "Indent current line using standard-clj formatting."
  (interactive)
  (let* ((original-pos (point))
         (line-start (line-beginning-position))
         (line-end (line-end-position)))
    ;; We need to include some context around the line for proper formatting
    ;; Find the beginning of the current top-level form
    (save-excursion
      (let* ((form-start (progn
                           (goto-char line-start)
                           (beginning-of-defun)
                           (point)))
             (form-end (progn
                         (end-of-defun)
                         (point)))
             (form-text (buffer-substring-no-properties form-start form-end))

             (formatted-form (standard-clojure--format-text form-text)))
        (goto-char form-start)
        (delete-region form-start form-end)
        (insert formatted-form)))
    (goto-char original-pos)))

(defun standard-clojure--indent-region (beg end)
  "Indent region from BEG to END using standard-clj formatting."
  (let* ((original-pos (point)))
    ;; We need to include some context around the region for proper formatting
    ;; Find the beginning of the current top-level form
    (save-excursion
      (let* ((form-start (progn
                           (goto-char beg)
                           (beginning-of-defun)
                           (point)))
             (form-end (progn
                         (goto-char end)
                         (end-of-defun)
                         (point)))
             (form-text (buffer-substring-no-properties form-start form-end))
             (formatted-form (standard-clojure--format-text form-text)))
        (goto-char form-start)
        (delete-region form-start form-end)
        (insert formatted-form)))
    (goto-char original-pos)))

(defun standard-clojure-hook ()
  "Sets `indent-line-function' and `indent-region-function' to use
`standard-clojure' equivalents."
  (setq indent-region-function #'standard-clojure--indent-region))

(provide 'standard-clojure)
