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

(defun my/eca-chat-mode-hook ()
  "Disable various minor modes in ECA chat buffers for cleaner experience."
  (when (fboundp 'denote-rename-buffer-mode) (denote-rename-buffer-mode -1)))

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


(use-package eca
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs" :rev :newest)
  :hook (eca-chat-mode . my/eca-chat-mode-hook)
  :bind (("C-c ." . eca-transient-menu)
         ("C-c e" . eca-chat-toggle-window)
         ("C-c i" . eca-chat-add-context-to-user-prompt))
  :ensure t
  ;;:init
  ;; Fix broken autoloads from package-vc before loading
  ;;(my/eca-fix-autoloads)
  :config
  ;; Ensure chat window is visible before adding context
  (advice-add 'eca-chat-add-context-to-user-prompt :before #'my/eca-ensure-chat-window-visible)

  ;; Customize transient menu keybindings
  ;; Replace "p" (repeat prompt) with minibuffer prompt (more common use case)
  (transient-suffix-put 'eca-transient-menu '(0 0 6) :key "P")
  (transient-append-suffix 'eca-transient-menu '(0 0 6)
    '("p" "Send prompt from minibuffer" my/eca-send-prompt-from-minibuffer))

  :custom
  ;;(setq eca-extra-args '("--verbose"))
  (setq eca-chat-auto-add-repomap t)

  )

(provide 'setup-eca)
;;; setup-eca.el ends here
