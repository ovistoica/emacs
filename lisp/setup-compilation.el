;;; setup-compilation.el --- Compilation config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package compile
  :preface
  (defun os/compile-autoclose (buffer string)
    "Hide successful builds window with BUFFER and STRING."
    (if (string-match "finished" string)
        (progn
          (message "Build finished: ")
          (run-with-timer 3 nil
                          (lambda ()
                            (when-let* ((multi-window (> (count-windows) 1))
                                        (live (buffer-live-p buffer))
                                        (window (get-buffer-window buffer t)))
                              (delete-window window))))
          (message "Compilation %s" string))))
  :config
  (setq compilation-scroll-output t)
  (setq compilation-auto-jump-to-first-error t
        compilation-max-output-line-length nil
        compilation-finish-functions (list #'os/compile-autoclose)))


(provide 'setup-compilation)
;;; setup-compilation.el ends here
