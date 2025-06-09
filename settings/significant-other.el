;; Significant Other
;;
;; Many files come in pairs, like tests and source files, header files and
;; implementations, components and their devcards.
;;
;; This impromptu package-to-be helps set up functions to jump between
;; significant other files.
;;
;; See setup-clojure-mode for example usage.

(require 'dash)

(setq significant-other-find-fn
      (lambda ()
        (message "Significant other not configured for this mode.")
        nil))

(defun significant-other-find-existing ()
  (-first 'file-exists-p (funcall significant-other-find-fn)))

(defun significant-other-find-all-existing ()
  (-filter 'file-exists-p (funcall significant-other-find-fn)))

(defun significant-other-find-tests ()
  "Find existing significant other files that are test files."
  (-filter (lambda (file) 
             (and (file-exists-p file)
                  (string-match-p "/test/.+\\.clj" file)))
           (funcall significant-other-find-fn)))

(defun significant-other-jump (arg)
  (interactive "P")
  (let ((existing-files (significant-other-find-all-existing)))
    (cond
     ((= (length existing-files) 1)
      ;; Only one file exists, jump to it
      (find-file (car existing-files)))
     ((> (length existing-files) 1)
      ;; Multiple files exist, let user choose
      (let ((file (completing-read "Choose significant other: " 
                                   existing-files nil t)))
        (find-file file)))
     (t
      ;; No existing files, offer to create one
      (when-let (file (car (funcall significant-other-find-fn)))
        (if arg
            (progn (find-file file) (save-buffer))
          (ido-find-file-in-dir (file-name-directory file))))))))

(defmacro with-significant-others (binding &rest mappings)
  (declare (indent 1))
  `(setq-local
    significant-other-find-fn
    (lambda ()
      (let ((,binding (buffer-file-name)))
        (cond
         ,@(--map
            `((string-match-p ,(car it) ,binding)
              ,(cadr it))
            mappings))))))

(global-set-key (kbd "s-j") 'significant-other-jump)

(provide 'significant-other)
