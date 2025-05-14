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

(use-package compile
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-scroll-output 'first-error)
  :commands (define-compilation-mode)
  :preface
  (cl-defun compile-add-error-syntax
      (mode name regexp &key file line col (level 'error) hyperlink highlight)
    "Register new compilation error syntax.

Add NAME symbol to `compilation-error-regexp-alist', and then add
REGEXP FILE LINE and optional COL LEVEL info to
`compilation-error-regexp-alist-alist'."
    (or file (error "Missing value for :file keyword"))
    (or line (error "Missing value for :line keyword"))
    (let ((faces '(compilation-info-face
                   compilation-warning-face
                   compilation-error-face))
          (level (cond ((eq level 'info) 0)
                       ((eq level 'warn) 1)
                       ((eq level 'error) 2)
                       (t (error "Mnsupported level type: %S" level))))
          (mode (symbol-name (or mode 'compilation))))
      (add-to-list (intern (concat mode "-error-regexp-alist")) name)
      (add-to-list (intern (concat mode "-error-regexp-alist-alist"))
                   (list name regexp file line col level hyperlink
                         (list highlight (nth level faces))))))
  (defmacro define-project-compilation-mode (base-name &rest body)
    (declare (indent 1))
    (let* ((name (symbol-name base-name))
           (doc-name (capitalize (replace-regexp-in-string "-compilation$" "" name)))
           (current-project-root (intern (concat name "-current-project")))
           (current-project-files (intern (concat name "-current-project-files")))
           (compilation-mode-name (intern (concat name "-mode"))))
      `(progn
         (defvar ,(intern (concat name "-error-regexp-alist")) nil
           ,(concat "Alist that specifies how to match errors in " doc-name " compiler output.
See `compilation-error-regexp-alist' for more information."))
         (defvar ,(intern (concat name "-error-regexp-alist-alist")) nil
           ,(concat "Alist of values for `" (downcase doc-name) "-compilation-error-regexp-alist'.
See `compilation-error-regexp-alist-alist' for more information."))
         (defvar-local ,current-project-root nil
           ,(concat "Current root of the project being compiled.
Set automatically by the `" (symbol-name compilation-mode-name) "'."))
         (defvar-local ,current-project-files nil
           ,(concat "Current list of files belonging to the project being compiled.
Set automatically by the `" (symbol-name compilation-mode-name) "'."))
         (define-compilation-mode ,compilation-mode-name
           ,(concat doc-name " Compilation")
           ,(concat "Compilation mode for " doc-name " output.")
           (setq-local ,current-project-root (project-current t))
           (setq-local ,current-project-files (project-files ,current-project-root))
           ,@body)
         (provide ',compilation-mode-name)))))

(use-package clojure-compilation-mode
  :no-require
  :after compile
  :config
  (defun clojure-compilation-p ()
    (and (require 'clojure-mode nil 'noerror)
         (clojure-project-root-path)
         'clojure-compilation-mode))
  (add-to-list 'project-compilation-modes 'clojure-compilation-p)
  (defun clojure-compilation--split-classpath (classpath)
    "Split the CLASSPATH string."
    (split-string classpath ":" t "[[:space:]\n]+"))
  (defmemo clojure-compilation--get-project-dependencies-memo
      (command _deps-file _mod-time)
    "Call COMMAND to obtain the classpath string.
DEPS-FILE and MOD-TIME are used for memoization."
    (thread-last
      command
      shell-command-to-string
      clojure-compilation--split-classpath
      (seq-filter (lambda (s) (string-suffix-p ".jar" s)))))
  (defun clojure-compilation--get-lein-project-dependencies (root)
    "Obtain classpath from lein for ROOT."
    (let* ((project-file (expand-file-name "project.clj" root))
           (mod-time (file-attribute-modification-time (file-attributes project-file))))
      (clojure-compilation--get-project-dependencies-memo
       "lein classpath" project-file mod-time)))
  (defun clojure-compilation--get-deps-project-dependencies (root)
    "Obtain classpath from deps for ROOT."
    (let* ((project-file (expand-file-name "deps.edn" root))
           (mod-time (file-attribute-modification-time (file-attributes project-file))))
      (clojure-compilation--get-project-dependencies-memo
       "clojure -Spath" project-file mod-time)))
  (defun clojure-compilation-get-project-dependencies (project)
    "Get dependencies of the given PROJECT.
Returns a list of all jar archives."
    (when (bound-and-true-p tramp-gvfs-enabled)
      (let ((root (project-root project)))
        (cond ((file-exists-p (expand-file-name "deps.edn" root))
               (clojure-compilation--get-deps-project-dependencies root))
              ((file-exists-p (expand-file-name "project.clj" root))
               (clojure-compilation--get-lein-project-dependencies root))))))
  (defvar-local clojure-compilation-project-deps nil
    "List of project's dependencies")
  (defvar-local clojure-compilation-project-deps-mod-time nil
    "Accumulated modification time of all project's libraries")
  (define-project-compilation-mode clojure-compilation
    (require 'tramp-gvfs)
    (setq-local clojure-compilation-project-deps
                (clojure-compilation-get-project-dependencies
                 clojure-compilation-current-project))
    (setq-local clojure-compilation-project-deps-mod-time
                (seq-reduce #'+ (mapcar (lambda (f)
                                          (time-to-seconds
                                           (file-attribute-modification-time
                                            (file-attributes f))))
                                        clojure-compilation-project-deps)
                            0)))
  (defun clojure-compilation--find-file-in-project (file)
    "Check if FILE is part of the currently compiled project."
    (if (file-name-absolute-p file)
        file
      (seq-find
       (lambda (s) (string-suffix-p file s))
       clojure-compilation-current-project-files)))
  (defun clojure-compilation--file-exists-jar-p (jar file)
    "Check if FILE is present in the JAR archive."
    (with-temp-buffer
      (when (zerop (call-process "jar" nil (current-buffer) nil "-tf" jar))
        (goto-char (point-min))
        (save-match-data
          (re-search-forward (format "^%s$" (regexp-quote file)) nil t)))))
  (defmemo clojure-compilation--find-dep-memo
      (file _project _deps-mod-time)
    "Find FILE in current project dependency list.
PROJECT and DEPS-MOD-TIME are used for memoizing the call."
    (when (not (string-empty-p file))
      (seq-find (lambda (d)
                  (clojure-compilation--file-exists-jar-p d file))
                clojure-compilation-project-deps)))
  (defun clojure-compilation--find-dep (file)
    "Find FILE in current project dependency list."
    (clojure-compilation--find-dep-memo
     file
     clojure-compilation-current-project
     clojure-compilation-project-deps-mod-time))
  (defun clojure-compilation-filename ()
    "Function that gets filename from the error message.
If the filename comes from a dependency, try to guess the
dependency artifact based on the project's dependencies."
    (when-let ((filename (substring-no-properties (match-string 1))))
      (or (clojure-compilation--find-file-in-project filename)
          (when-let ((dep (clojure-compilation--find-dep filename)))
            (concat (expand-file-name dep) "/" filename)))))
  (compile-add-error-syntax
   'clojure-compilation 'some-warning
   "^\\([^:[:space:]]+\\):\\([0-9]+\\) "
   :file #'clojure-compilation-filename
   :line 2 :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clj-kondo-warning
   "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): warning"
   :file 1 :line 2 :col 3 :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clj-kondo-error
   "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): error"
   :file 1 :line 2 :col 3 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'kaocha-tap
   "^not ok.*(\\([^:]*\\):\\([0-9]*\\))"
   :file #'clojure-compilation-filename
   :line 2 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-fail
   "^.*\\(?:FAIL\\|ERROR\\) in.*(\\([^:]*\\):\\([0-9]*\\))"
   :file #'clojure-compilation-filename
   :line 2 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-reflection-warning
   "^Reflection warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
   :file #'clojure-compilation-filename
   :line 2 :col 3
   :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-performance-warning
   "^Performance warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
   :file #'clojure-compilation-filename
   :line 2 :col 3
   :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-syntax-error
   "^Syntax error .* at (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\))"
   :file #'clojure-compilation-filename
   :line 2 :col 3)
  (compile-add-error-syntax
   'clojure-compilation 'kaocha-unit-error
   "^ERROR in unit (\\([^:]+\\):\\([0-9]+\\))"
   :file #'clojure-compilation-filename
   :line 2 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'eastwood-warning
   "^\\([^:[:space:]]+\\):\\([0-9]+\\):\\([0-9]+\\):"
   :file #'clojure-compilation-filename
   :line 2 :col 3 :level 'warn :hyperlink 1 :highlight 1))


(provide 'setup-compilation)
;;; setup-compilation.el ends here
