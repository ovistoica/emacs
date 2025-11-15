;; Perspective
;;
;; Provides multiple named workspaces (or "perspectives") in Emacs, similar to
;; multiple desktops in window managers like Awesome and XMonad, and Spaces on
;; the Mac.

(use-package perspective
  :vc (:url "https://github.com/nex3/perspective-el")
  :bind (("C-x k" . persp-kill-buffer*)
         ("C-x b" . persp-switch-to-buffer*))
  :init
  (setq persp-mode-prefix-key (kbd "C-x x"))
  (persp-mode)
  :config
  (setq persp-modestring-short t)
  (persp-add-buffer-to-frame-global "*Messages*")
  (persp-add-buffer-to-frame-global "*Warnings*")
  (persp-add-buffer-to-frame-global "*lsp-log*"))

;; Avoid popping ediff up in separate window, it breaks perspective
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;; Macro to open perspective with `name' and evaluate `body'
(defmacro with-perspective (name &rest body)
  `(let ((initialize (not (gethash ,name (perspectives-hash))))
         (current-perspective (persp-curr)))
     (persp-switch ,name)
     (when initialize ,@body)
     (set-frame-parameter nil 'persp--last current-perspective)))

(provide 'setup-perspective)
