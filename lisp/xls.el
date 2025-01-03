(defun no-xls (&optional filename)
  "Run xlhtml and w3m -dump on the entire buffer.
    Optional FILENAME says what filename to use.
    This is only necessary for buffers without
    proper `buffer-file-name'.  FILENAME should
    be a real filename, not a path."
  (interactive "fExcel File: ")
  (when (and filename
	     (not (buffer-file-name)))
    (write-file (make-temp-file filename)))
  (erase-buffer)
  (shell-command
   (format "xlhtml -nc -te %s | w3m -dump -T text/html" (buffer-file-name))
   (current-buffer))
  (setq buffer-file-name nil)
  (set-buffer-modified-p nil))

(add-to-list 'auto-mode-alist '("\\.xls\\'" . no-xls))
