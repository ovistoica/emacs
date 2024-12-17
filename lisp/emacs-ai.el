;;; emacs-ai.el --- Minor mode for interacting with AI agents.

;; Version: 1.0.0
;; Author: Ovi Stoica <ovidiu.stoica1094@gmail.com>
;; Url: https://github.com/ovistoica/emacs-ai
;; Keywords: convenience, project, ai, emacs

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package allows you to easily work with AI agents.  It tries to emulate
;; many of the useful functionalities from modern editors like Cursor or Windsurf.

;; It provides an AI chat window and helpful commands around it


;;; Code:

(use-package gptel
  :straight (:host github :repo "karthink/gptel")
  :after project
  :defines
  gptel-make-anthropic
  gptel-api-key
  :preface
  (defvar-local emacs-ai-buffer nil
    "Buffer for AI interaction in the current project.")

  (defcustom emacs-ai-window-width 80
    "Width of the AI side panel window."
    :type 'integer
    :group 'emacs-ai)

  (defun emacs-ai-buffer-name ()
    "Generate the AI buffer name for the current project."
    (if-let ((project (project-current)))
        (format "*AI: %s*" (project-name project))
      "*AI*"))

  (defun emacs-ai-get-or-create-buffer ()
    "Get or create an AI buffer for the current project"
    (let ((buffer-name (emacs-ai-buffer-name)))
      (or (get-buffer buffer-name)
          (with-current-buffer (get-buffer-create buffer-name)
            (cond ;Set major mode
             ((eq major-mode gptel-default-mode))
             ((eq gptel-default-mode 'text-mode)
              (text-mode)
              (visual-line-mode 1))
             (t (funcall gptel-default-mode)))
            (gptel--sanitize-model :backend (default-value 'gptel-backend)
                                   :model (default-value 'gptel-model)
                                   :shoosh nil)
            (unless gptel-mode (gptel-mode 1))
            (goto-char (point-max))
            (skip-chars-backward "\t\r\n")
            (if (bobp) (gptel-prompt-prefix-string))
            (current-buffer)))))

  (defun emacs-ai-display-buffer (buffer)
    "Display BUFFER in a side window."
    (display-buffer
     buffer
     `(display-buffer-in-side-window
       (side . right)
       (slot . 1)
       (window-width . ,emacs-ai-window-width)
       (preserve-size . (t . nil)))))

  (defun emacs-ai-toggle-ai-panel ()
    "Toggle the AI panel for the current project."
    (interactive)
    (if-let ((window (get-buffer-window (emacs-ai-buffer-name))))
        (delete-window window)
      (let ((buffer (emacs-ai-get-or-create-buffer)))
        (emacs-ai-display-buffer buffer)
        (select-window (get-buffer-window buffer)))))
  :init
  (setq gptel-default-mode 'org-mode)  ; Use org-mode as the default
  :bind (("C-c <enter>" . gptel-send)
         ("C-c RET" . gptel-send)
         ("C-c C-<enter>" . gptel-menu)
         ("C-c C-a" . emacs-ai-toggle-ai-pannel))
  :config
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key os-secret-anthropic-key)
        gptel-model "claude-3-5-sonnet-20241022"
        gptel-max-tokens 4096
        gptel-temperature 0.7
        ;; Configure the chat UI
        gptel-window-select t           ; Select the window after creation
        gptel-window-side 'right        ; Display on the right side
        gptel-window-width 80           ; Set window width
        )

  ;; Define some useful prompt templates
  (setq gptel-prompt-templates
        '(("code-review" . "Please review the following code and suggest improvements:\n\n")
          ("explain" . "Please explain how the following code works:\n\n")
          ("refactor" . "Please suggest how to refactor this code to improve its quality:\n\n")
          ("document" . "Please help me document this code with appropriate comments:\n\n"))))

(use-package corsair
  :straight (:host github
                   :repo "rob137/Corsair"
                   :files ("corsair.el")) ; Load the specific file
  :defer t                                ; Optional: Load lazily
  :after gptel                          ; Ensure gptel is loaded before corsair
  )

(use-package copilot
  :defines
  copilot-max-char
  :commands
  copilot-mode
  :preface
  (defun os/activate-copilot ()
    (if (or (> (buffer-size) 100000)
            (string-prefix-p "*temp*-" (buffer-name)))
        ;; Or don't even warn to get rid of it.
        (message "Buffer size exceeds copilot max char limit or buffer is temporary. Copilot will not be activated.")
      (copilot-mode)))
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :config
  (setq copilot-enable-predicates nil
        warning-suppress-types '((copilot copilot--infer-indentation-offset)))
  (add-to-list 'copilot-major-mode-alist '("tsx-ts" . "typescriptreact"))
  (add-to-list 'copilot-major-mode-alist '("typescript-ts" . "typescript"))
  (add-to-list 'copilot-indentation-alist '(tsx-ts-mode 2))
  (add-to-list 'copilot-indentation-alist '(typescript-ts-mode 2))
  (add-to-list 'copilot-indentation-alist '(clojure-mode 2))
  (add-to-list 'copilot-indentation-alist '(clojurec-mode 2))
  (add-to-list 'copilot-indentation-alist '(clojurescript-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(css-ts-mode 2))
  (add-to-list 'copilot-indentation-alist '(json-ts-mode 2))
  (add-to-list 'copilot-indentation-alist '(dockerfile-mode 2))
  :bind (:map copilot-completion-map
              ("C-<tab>" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion)
              ("S-C-TAB" . 'copilot-accept-completion-by-word)
              ("S-C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  (setenv "ANTHROPIC_API_KEY" os-secret-anthropic-key)
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(require 'project)






(provide 'emacs-ai)
;;; emacs-ai.el ends here

;; This is not a literate config tangled from an Org-mode document! So I include
;; some file-specific settings to make it easier to parse. Specifically, the
;; outline that you see in this document is represented in the Lisp files as
;; Org-style collapsible outline headings. See [[*OUTLINE MODE][Outline Mode]].

;; Local Variables:
;; outline-regexp: ";; \\*+"
;; page-delimiter: ";; \\**"
;; eval:(outline-minor-mode 1)
;; eval:(outline-hide-sublevels 5)
;; End:
