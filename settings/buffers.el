;;; buffers.el --- manipulating buffers and the files that are in them  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; Copy the file path for the current buffer
(global-set-key (kbd "C-x M-w") 'copy-current-buffer-dwim)

;; Touch the buffer (save without changing)
(global-set-key (kbd "C-x t") 'touch-buffer-file)

;; Create new frame
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; Bury buffer
(global-set-key (kbd "s-y") 'bury-buffer)

;; Toggle two most recent buffers
(global-set-key (kbd "s-b") 'mode-line-other-buffer)

;; Eval buffer
(global-set-key (kbd "C-c C-k") 'eval-buffer)

;; Create scratch buffer
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;;;; Implementations

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))


(defun delete-current-buffer-file ()
  "Remove file connected to the current buffer and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' succesfully removed" filename)))))


(defun copy-current-file-path ()
  "Add current file path to kill ring.
Limits the filename to project root if possible."
  (interactive)
  (let ((filename (buffer-file-name)))
    (kill-new filename)
    (message "Copied file path %s" filename)))


(defun create-scratch-buffer ()
  "Create a new scratch buffer to work in (could be *scratch* - *scratchX*)."
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)))

(defun touch-buffer-file ()
  "Touch the buffer (save without changing)."
  (interactive)
  (insert " ")
  (delete-char 1)
  (save-buffer))

(defun mode-to-lang ()
  "Return a code-fence language string for the current buffer's major mode."
  (pcase major-mode
    ((or 'clojure-mode 'clojurec-mode)  "clojure")
    ('clojurescript-mode                "clojurescript")
    ((or 'emacs-lisp-mode
         'lisp-interation-mode)        "emacs-lisp")
    ((or 'python-mode 'python-ts-mode)  "python")
    ((or 'js-mode 'js-ts-mode)          "javascript")
    ((or 'typescript-mode
         'typescript-ts-mode)           "typescript")
    ('tsx-ts-mode                       "tsx")
    ((or 'rust-mode 'rust-ts-mode)      "rust")
    ((or 'go-mode 'go-ts-mode)          "go")
    ((or 'sh-mode 'bash-ts-mode)        "bash")
    ((or 'css-mode 'css-ts-mode)        "css")
    ((or 'html-mode 'mhtml-mode)        "html")
    ((or 'json-mode 'json-ts-mode)      "json")
    ((or 'yaml-mode 'yaml-ts-mode)      "yaml")
    ('sql-mode                          "sql")
    (_
     (string-remove-suffix
      "-ts-mode"
      (string-remove-suffix "-mode" (symbol-name major-mode))))))

(require 'project)

(defun region-to-string (beg end)
  "Return a fenced code block string for the region from BEG to END.
The block is prefixed with the file path (relative to project root) and
line range, e.g.:
  `src/foo.clj` L42-67:
  ```clojure
  (defn my-fn ...)
  ```"
  (let* ((file     (buffer-file-name))
         (root     (when-let* ((proj (project-current)))
                     (project-root proj)))
         (rel-file (if (and file root)
                       (file-relative-name file root)
                     (buffer-name)))
         (start-line (line-number-at-pos beg))
         (end-line   (let ((l (line-number-at-pos end)))
                       (if (and (> end beg)
                                (save-excursion (goto-char end) (bolp)))
                           (1- l) l)))
         (lang (mode-to-lang))
         (code (buffer-substring-no-properties beg end))
         (code (if (string-suffix-p "\n" code) code (concat code "\n"))))
    (format "`%s` L%d-%d:\n```%s\n%s```\n"
            rel-file start-line end-line lang code)))

(defun region-as-code-block (beg end)
  "Copy the region BEG to END as a fenced code block to the kill ring.
The block includes the relative file path and line range as a header,
ready to paste into any agent chat."
  (interactive "r")
  (let ((block (region-to-string beg end)))
    (kill-new block)
    (message "Copied code block (%s) to kill ring"
             (buffer-substring-no-properties beg (min end (+ beg 40))))))

(defun copy-current-buffer-dwim (beg end)
  "Copy file path of the current buffer or code block from BEG to END of region.
If the region is selected, use `region-as-code-block' otherwise use
`copy-current-file-path'"
  (interactive "r")
  (if (use-region-p)
      (region-as-code-block beg end)
    (copy-current-file-path)))

(provide 'buffers)
;;; buffers.el ends here
