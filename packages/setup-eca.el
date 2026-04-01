;;; setup-eca --- A vendor neutral agent interface -*- lexical-binding: t -*-

;;; Commentary:
;; Eca - Editor Code Assistant

;;; Code:
(require 'transient)

(defun my/eca-fix-autoloads ()
  "Regenerate ECA autoloads if broken by package-vc.
package-vc sometimes generates a stub autoloads file that loads itself
recursively instead of containing actual autoload definitions.  This
detects and fixes that by regenerating autoloads properly."
  (when-let* ((pkg-dir (and (package-installed-p 'eca)
                            (package-desc-dir (cadr (assq 'eca package-alist)))))
              (autoloads-file (expand-file-name "eca-autoloads.el" pkg-dir)))
    (when (and (file-exists-p autoloads-file)
               (with-temp-buffer
                 (insert-file-contents autoloads-file)
                 ;; Broken if it only has the self-referencing stub
                 (and (search-forward "Autoload indirection" nil t)
                      (not (search-forward ";;; Generated autoloads" nil t)))))
      (message "ECA: Regenerating broken autoloads...")
      (package-generate-autoloads "eca" pkg-dir)
      ;; Remove the self-referencing stub and add proper header
      (with-temp-buffer
        (insert-file-contents autoloads-file)
        (goto-char (point-min))
        (when (search-forward "Autoload indirection" nil t)
          (let ((stub-end (search-forward "\n\n" nil t)))
            (when stub-end
              (delete-region (point-min) stub-end)
              (goto-char (point-min))
              ;; Proper header with load-path setup (critical for package-vc)
              (insert ";;; eca-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-\n"
                      ";;\n;;; Code:\n\n"
                      "(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))\n\n")
              ;; Add provide/footer if missing
              (goto-char (point-max))
              (unless (search-backward "(provide 'eca-autoloads)" nil t)
                (goto-char (point-max))
                (insert "\n(provide 'eca-autoloads)\n;;; eca-autoloads.el ends here\n"))
              (write-region (point-min) (point-max) autoloads-file))))
        (load autoloads-file nil t)
        (message "ECA: Autoloads regenerated successfully.")))))

(defun my/eca-recover-completion-item (item)
  "Return ITEM with `eca-chat-completion-item' text property restored if missing.
Some completion UIs (e.g. `consult-completion-in-region') call `completing-read'
and insert the returned string without text properties.  This searches the
buffer-local completion caches for a candidate whose plain text matches ITEM
and returns the original propertized string."
  (if (get-text-property 0 'eca-chat-completion-item item)
      item
    (let ((found nil))
      (dolist (cache (list eca-chat--context-completion-cache
                           eca-chat--file-completion-cache))
        (unless found
          (maphash (lambda (_query candidates)
                     (unless found
                       (setq found (seq-find
                                    (lambda (c)
                                      (string= (substring-no-properties c) item))
                                    candidates))))
                   cache)))
      (or found item))))

(defun my/eca-chat-mode-hook ()
  "Disable various minor modes in ECA chat buffers for cleaner experience."
  (when (fboundp 'denote-rename-buffer-mode) (denote-rename-buffer-mode -1)))

(use-package eca
  :defines (eca-chat-focus-on-open)
  :functions (eca-session
              eca-chat--get-last-buffer
              eca-chat--display-buffer
              eca-chat-send-prompt
              eca-chat--expandable-content-toggle
              eca-chat--add-expandable-content)
  :preface
  (defun my/eca-send-prompt-from-minibuffer ()
    "Prompt for a message in the minibuffer and send it to the current ECA chat."
    (interactive)
    (let ((prompt (read-string "ECA Prompt: ")))
      (when (and prompt (not (string-empty-p prompt)))
        (eca-chat-send-prompt prompt))))

  (defun my/eca-ensure-chat-window-visible (&rest _)
    "Ensure the ECA chat window is visible before interacting with it."
    (when-let* ((session (eca-session))
                (buffer (eca-chat--get-last-buffer session)))
      (unless (get-buffer-window buffer t)
        (let ((eca-chat-focus-on-open nil))
          (eca-chat--display-buffer buffer)))))

  (defvar-local my/eca-auto-expand-blocks nil
    "When non-nil, auto-expand new expandable blocks as they are added.")

  (defun my/eca-auto-expand-block (id &rest _)
    "Expand ID immediately if `my/eca-auto-expand-blocks' is set in this buffer."
    (when (and (derived-mode-p 'eca-chat-mode) my/eca-auto-expand-blocks)
      (eca-chat--expandable-content-toggle id t nil)))

  (defun my/eca-chat-expand-all-blocks (&optional sticky)
    "Expand all collapsed blocks in the current ECA chat buffer.
With prefix argument STICKY, also keep all future blocks in this
buffer expanded automatically."
    (interactive "P")
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when-let* ((id (overlay-get ov 'eca-chat--expandable-content-id)))
        (eca-chat--expandable-content-toggle id t nil)))
    (when sticky
      (setq-local my/eca-auto-expand-blocks t)
      (message "ECA: future blocks in this buffer will auto-expand")))

  (defun my/eca-chat-collapse-all-blocks (&optional sticky)
    "Collapse all expanded blocks in the current ECA chat buffer.
With prefix argument STICKY, also disable future auto-expansion in
this buffer."
    (interactive "P")
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when-let* ((id (overlay-get ov 'eca-chat--expandable-content-id)))
        (eca-chat--expandable-content-toggle id t t)))
    (when sticky
      (setq-local my/eca-auto-expand-blocks nil)
      (message "ECA: auto-expand disabled for this buffer")))

  :vc (:url "https://github.com/editor-code-assistant/eca-emacs" :rev :newest)
  :hook (eca-chat-mode . my/eca-chat-mode-hook)
  :bind (("C-c ." . eca-transient-menu)
         ("C-c e" . eca-chat-toggle-window)
         ("C-c i" . eca-chat-add-context-to-user-prompt)
         (:map eca-chat-mode-map
               ("C-c C-o" . my/eca-chat-expand-all-blocks)
               ("C-c C-c" . my/eca-chat-collapse-all-blocks)))
  :ensure t
  ;;:init

  :config
  ;; Ensure chat window is visible before adding context
  (advice-add 'eca-chat-add-context-to-user-prompt :before #'my/eca-ensure-chat-window-visible)

  ;; Register the auto-expand advice once globally — it is a no-op in buffers
  ;; where my/eca-auto-expand-blocks is nil (the default).
  (advice-add 'eca-chat--add-expandable-content :after #'my/eca-auto-expand-block)

  ;; Fix @file completion with consult-completion-in-region (and any completing-read-based
  ;; UI).  Those UIs call completing-read and insert the returned string, which strips text
  ;; properties — including `eca-chat-completion-item' that exit functions need to resolve
  ;; the selected candidate.  The advice recovers the propertized original from the cache.
  (advice-add 'eca-chat--completion-context-from-prompt-exit-function
              :filter-args (lambda (args)
                             (cons (my/eca-recover-completion-item (car args)) (cdr args))))
  (advice-add 'eca-chat--completion-context-from-new-context-exit-function
              :filter-args (lambda (args)
                             (cons (my/eca-recover-completion-item (car args)) (cdr args))))
  (advice-add 'eca-chat--completion-file-from-prompt-exit-function
              :filter-args (lambda (args)
                             (cons (my/eca-recover-completion-item (car args)) (cdr args))))

  :custom
  ;;(setq eca-extra-args '("--verbose"))
  (eca-chat-auto-add-repomap t)
  (eca-worktree-mode 'isolated)
  ;; Keep tool call blocks open after they complete — no manual tabbing needed.
  (eca-chat-shrink-called-tools nil)

  )

(provide 'setup-eca)
;;; setup-eca.el ends here
