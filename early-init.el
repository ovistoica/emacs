;;; early-init.el --- Early initialization -*- lexical-binding: t -*-
;;
;; This file is loaded before the package system and GUI is initialized.
;;

;; Prevent package.el from auto-loading packages before we manually initialize it
;; We call (package-initialize) manually in settings/packages.el
(setq package-enable-at-startup nil)

;;; early-init.el ends here
