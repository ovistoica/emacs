;;; appearance --- Settings for fonts, theme & what is shown in emacs -*- lexical-binding: t -*-
;;;
;;; Commentary:

;;; Some of the logic here is based around Omarchy as an OS where you can change
;;; themes & fonts directly from the system prompt. You'll find equivalent
;;; themes here for a homogeneus experience.

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

;; Font management

(defun my/set-font (font &optional height)
  "Set the Emacs font to FONT if it is available.
HEIGHT is optional and defaults to the current font height."
  (interactive)
  ;; Get current height if not provided
  (let ((font-height (or height (face-attribute 'default :height))))

    (when (member font (font-family-list))
      (set-face-attribute 'default nil :font font :height font-height)
      (message "Font was set to %s" font))))

(defvar my/default-font "Iosevka Nerd Font Mono")

(defun my/get-omarchy-font ()
  "Get the current font from omarchy, or return default."
  (if (eq system-type 'gnu/linux)
      (let ((font (string-trim (shell-command-to-string "omarchy-font-current"))))
        (if (string-empty-p font) my/default-font font))
    my/default-font))

;;; Theme management

;; Add custom themes directory to load path
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(defun my/raw-set-theme (theme)
  "Disable previously enabled themes, before enabling THEME to not have overlaps."
  (interactive
   (list (intern (completing-read "Choose theme: "
                                  (mapcar #'symbol-name (custom-available-themes))
                                  nil t))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme :no-confirm))

(defvar my/theme-mapping
  '(("Catppuccin" . (lambda ()
                      (setq catppuccin-flavor 'mocha)
                      (my/raw-set-theme 'catppuccin)))
    ("catppuccin" . (lambda ()
                      (setq catppuccin-flavor 'mocha)
                      (my/raw-set-theme 'catppuccin)))
    ("Catppuccin Latte" . (lambda ()
                            (setq catppuccin-flavor 'latte)
                            (my/raw-set-theme 'catppuccin)))
    ("catppuccin-latte" . (lambda ()
                            (setq catppuccin-flavor 'latte)
                            (my/raw-set-theme 'catppuccin)))
    ("everforest" . everforest)
    ("flexoki-light" . flexoki-light)
    ("Flexoki Light" . flexoki-light)
    ("gruvbox" . doom-gruvbox)
    ("Gruvbox" . doom-gruvbox)
    ("kanagawa" . kanagawa)
    ("matte-black" . matte-black)
    ("nord" . doom-nord)
    ("Nord" . doom-nord)
    ("osaka-jade" . osaka-jade)
    ("Osaka Jade" . osaka-jade)
    ("ristretto" . doom-monokai-ristretto)
    ("Ristretto" . doom-monokai-ristretto)
    ("rose-pine" . rose-pine)
    ("tokyo-night" . doom-tokyo-night)
    ("Tokyo Night" . doom-tokyo-night)
    ("Ethereal" . modus-vivendi-tinted)
    ("ethereal" . modus-vivendi-tinted))
  "Map Omarchy theme names to either symbols or configuration functions.")



(defvar my/default-theme 'default-black
  "Default theme to use if omarchy theme cannot be determined.")

(defun my/set-theme (theme-spec)
  "Load the Emacs theme corresponding to THEME-SPEC from omarchy.
THEME-SPEC can be:
- A string: looks up the theme in my/theme-mapping
- A symbol: loads that theme directly
- A function: calls the function to configure and load the theme"
  (interactive)
  (message "Got theme %s" theme-spec)
  (let ((handler (if (stringp theme-spec)
                     (or (cdr (assoc theme-spec my/theme-mapping))
                         (intern theme-spec))
                   theme-spec)))
    (condition-case err
        (progn
          (if (functionp handler)
              (progn
                (funcall handler)
                (message "Theme set to %s" theme-spec))
            (progn
              (my/raw-set-theme handler)
              (message "Theme set to %s" handler))))
      (error
       (message "Failed to load theme: %s" (error-message-string err))))))

(defun my/get-omarchy-theme ()
  "Get the current theme from omarchy, or return default."
  (if (eq system-type 'gnu/linux)
      (let ((theme (string-trim (shell-command-to-string "omarchy-theme-current"))))
        (if (string-empty-p theme) my/default-theme theme))
    my/default-theme))

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

;; Omarchy appearance initialization (deferred for daemon mode)
(defun my/init-omarchy-appearance ()
  "Initialize font and theme from omarchy settings."
  (my/set-font (my/get-omarchy-font))
  (my/set-theme (my/get-omarchy-theme))
  (when (daemonp)
    (start-process "emacs-notify" nil "notify-send"
                   "--app-name=Emacs"
                   "Emacs initialized!"
                   "Emacs daemon is ready to use.")))

;; For daemon mode, delay initialization to ensure system is ready
;; For interactive mode, run immediately after init
(if (daemonp)
    (run-with-timer 2 nil #'my/init-omarchy-appearance)
  (add-hook 'after-init-hook #'my/init-omarchy-appearance))

(provide 'appearance)
;;; appearance.el ends here
