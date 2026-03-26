;; setup-pi-coding-agent.el --- Pi coding agent Emacs frontend

(use-package pi-coding-agent
  :vc (:url "https://github.com/dnouri/pi-coding-agent" :rev :newest)
  :bind (("C-c C-p" . my/pi-coding-agent-toggle)
         :map pi-coding-agent-input-mode-map
         ("C-c C-m" . pi-coding-agent-menu)
         ("C-c C-p" . my/pi-coding-agent-toggle)
         :map pi-coding-agent-chat-mode-map
         ("C-c C-m" . pi-coding-agent-menu)
         ("C-c C-p" . my/pi-coding-agent-toggle))
  :init
  ;; Allow M-x pi as a shorthand
  (defalias 'pi 'pi-coding-agent)
  :config

  (defvar my/pi-coding-agent--saved-window-config nil
    "Window configuration saved before showing the pi coding agent layout.")

  (defun my/pi-coding-agent-toggle ()
    "Toggle a focused pi layout: chat on top, input on bottom.
On first press, saves the current window configuration and shows the
pi session for the current project fullscreen (split into two panes).
On second press, restores the previous window configuration.
Signals an error if no pi session exists for the current project."
    (interactive)
    (let* ((dir (pi-coding-agent--session-directory))
           (chat-buf (pi-coding-agent--find-session dir))
           (input-buf (and chat-buf
                           (buffer-local-value
                            'pi-coding-agent--input-buffer chat-buf))))
      (cond
       ;; No session yet for this project
       ((null chat-buf)
        (user-error "No pi session for this project — run M-x pi-coding-agent first"))

       ;; Pi windows are visible → restore previous layout or bury buffers
       ((get-buffer-window chat-buf)
        (if my/pi-coding-agent--saved-window-config
            (set-window-configuration my/pi-coding-agent--saved-window-config)
          (with-current-buffer chat-buf
            (pi-coding-agent--hide-session-windows)))
        (setq my/pi-coding-agent--saved-window-config nil))

       ;; Pi windows are hidden → save layout and show pi
       (t
        (setq my/pi-coding-agent--saved-window-config
              (current-window-configuration))
        (delete-other-windows)
        (switch-to-buffer chat-buf)
        (let* ((input-height (or (bound-and-true-p pi-coding-agent-input-window-height) 10))
               (input-win (split-window-below (- (window-total-height) input-height))))
          (set-window-buffer input-win input-buf)
          (select-window input-win)))))))
