;;; appearance --- Visual defaults and display-related behavior -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Generic appearance settings — menu/tool/scrollbar off, current-line
;;; highlight, frame title, ring-bell, line numbers, ANSI colors in
;;; compilation buffers.  Omarchy-driven theme/font sync lives in the
;;; `omarchy' package; see `packages/setup-omarchy.el'.

;;; Code:

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Color all language features
(setq font-lock-maximum-decoration t)

;; Highlight current line
(global-hl-line-mode 1)

;; Include entire file path in title
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; Be less obnoxious
(blink-cursor-mode -1)
(tooltip-mode -1)

;; Custom themes directory
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

;; Don't beep. Just blink the modeline on errors.
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Enable line numbers only in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Colorize ANSI escape sequences in compilation buffers
(require 'ansi-color)
(require 'compile)
(defun my/colorize-compilation-buffer ()
  "Apply ANSI colors to compilation buffer output."
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook #'my/colorize-compilation-buffer)

;; Customize line number appearance
(require 'display-line-numbers)
(setq display-line-numbers-type 'absolute) ; Use 'relative for relative line numbers
(setq display-line-numbers-width-start t)  ; Auto-adjust width based on buffer size

(provide 'appearance)
;;; appearance.el ends here
