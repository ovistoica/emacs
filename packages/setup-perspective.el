;; Perspective
;;
;; Provides multiple named workspaces (or "perspectives") in Emacs, similar to
;; multiple desktops in window managers like Awesome and XMonad, and Spaces on
;; the Mac.

(use-package perspective
  :straight '(perspective :type git :host github :repo "nex3/perspective-el")
  :bind (("C-x C-b" . persp-ibuffer)
         ("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*))
  :init
  (setq persp-mode-prefix-key (kbd "C-x x"))
  (setq persp-modestring-short t)
  (persp-mode))

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
