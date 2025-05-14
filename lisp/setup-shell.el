;;; setup-shell.el ---  Setup for shell envs -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:
(use-package vterm
  :ensure vterm
  :config
  (setq shell-file-name "/bin/zsh"
        vterm-max-scrollback 5000))


;;;;; Stuff

(setenv "PLENV_ROOT" "/opt/plenv")
(use-package exec-path-from-shell
  :ensure exec-path-from-shell
  :demand
  :functions
  exec-path-from-shell-initialize
  :init
  ;; FIXME seeing if this does anything... (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-variables
   '(
     "CARGO_HOME"
     "GOPATH"
     "GOROOT"
     "MANPATH"
     "NVM_DIR"
     "PATH"
     "PLENV_ROOT"
     "RUSTUP_HOME"
     "SSH_AGENT_PID"
     "SSH_AUTH_SOCK"
     )))

(provide 'setup-shell)
;;; setup-shell.el ends here
