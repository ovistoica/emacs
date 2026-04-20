;; Kaocha Runner
;;
;; An emacs package for running Kaocha tests via CIDER.

(use-package kaocha-runner
  :after (cider-mode)
  :commands (kaocha-runner--run-tests)
  :bind (:map clojure-mode-map
              ("C-c k t" . kaocha-runner-run-test-at-point)
              ("C-c k r" . kaocha-runner-run-tests)
              ("C-c k a" . kaocha-runner-run-all-tests)
              ("C-c k w" . kaocha-runner-show-warnings)
              ("C-c k h" . kaocha-runner-hide-windows)))

(defun kaocha-runner--is-test? (s)
  (string-match-p "/test/.+\.clj" s))

(defun kaocha-runner--project-has-kaocha? ()
  "Return non-nil when the current project declares kaocha as a dep.

Like `cljr--project-depends-on-p' but also checks `deps.local.edn'
alongside `deps.edn', so kaocha declared only in a personal
`deps.local.edn' still enables the on-load test runner."
  (or (cljr--project-depends-on-p "kaocha")
      (when-let* ((project-dir (cljr--project-dir))
                  (local (expand-file-name "deps.local.edn" project-dir)))
        (and (file-exists-p local)
             (with-temp-buffer
               (insert-file-contents local)
               (goto-char (point-min))
               (search-forward "kaocha" nil t))))))

(defun kaocha-runner--significant-other-find-existing-test ()
  (car (significant-other-find-tests)))

(defun kaocha-runner--should-run-tests? ()
  "Check if tests should run based on buffer comment."
  (save-excursion
    (goto-char (point-min))
    (not (re-search-forward ";; {:kaocha-runner-run-tests-on-file-loaded\\? false}" nil t))))

(defun kaocha-runner-run-relevant-tests ()
  (interactive)
  (when (kaocha-runner--project-has-kaocha?)
    (if (kaocha-runner--is-test? (buffer-file-name))
        (when (kaocha-runner--should-run-tests?)
          (kaocha-runner--run-tests
           (kaocha-runner--testable-sym (cider-current-ns) nil nil)
           nil t))
      (let ((original-buffer (current-buffer)))
        (when (kaocha-runner--should-run-tests?)
          (save-window-excursion
            (when-let ((file (kaocha-runner--significant-other-find-existing-test)))
              (find-file file)
              (when (kaocha-runner--should-run-tests?)
                (kaocha-runner--run-tests
                 (kaocha-runner--testable-sym (cider-current-ns) nil nil)
                 nil t original-buffer)))))))))

(add-hook 'cider-file-loaded-hook #'kaocha-runner-run-relevant-tests)

(provide 'setup-kaocha-runner)
;;; setup-kaocha-runner.el ends here
