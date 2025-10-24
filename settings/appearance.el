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

(defun my/set-font (font &optional height)
  "Set the emacs font to FONT if it is available.
HEIGHT is optional and defaults to the current font height."
  (interactive)
  ;; Get current height if not provided
  (let ((font-height (or height (face-attribute 'default :height))))

    (when (member font (font-family-list))
      (set-face-attribute 'default nil :font font :height font-height)
      (message "Font was set to %s" font))))

(defvar my/default-font "CaskaydiaCove Nerd Font")

(defvar my/current-font
  (cond ((eq system-type 'darwin)
         my/default-font)
        ((eq system-type 'gnu/linux)
         (or (string-trim (shell-command-to-string "omarchy-font-current"))
             my/default-font))
        (t my/default-font)))

(my/set-font my/current-font)

;; Don't beep. Just blink the modeline on errors.
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Enable line numbers only in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Customize line number appearance
(setq display-line-numbers-type 'absolute) ; Use 'relative for relative line numbers
(setq display-line-numbers-width-start t)  ; Auto-adjust width based on buffer size

(provide 'appearance)
