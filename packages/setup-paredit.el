;;; setup-paredit.el --- Paredit structural editing configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures paredit for structural editing in Lisp modes.
;; Rebinds several default paredit keys that conflict with windmove or
;; standard navigation, replacing them with super-key equivalents.

;;; Code:

(declare-function paredit-wrap-round   "paredit")
(declare-function paredit-wrap-square  "paredit")
(declare-function paredit-wrap-curly   "paredit")
(declare-function paredit-forward-delete  "paredit")
(declare-function paredit-backward-kill-word "paredit")

(defun paredit-wrap-round-from-behind ()
  "Wrap the previous sexp in round parentheses."
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(defun paredit-wrap-square-from-behind ()
  "Wrap the previous sexp in square brackets."
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-square))

(defun paredit-wrap-curly-from-behind ()
  "Wrap the previous sexp in curly braces."
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-curly))

(defun paredit-kill-region-or-backward-word ()
  "Kill the active region, or kill the word before point."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-backward-kill-word)))

(defun conditionally-enable-paredit-mode ()
  "Enable paredit in the minibuffer for `eval-expression' and friends."
  (when (or (eq this-command 'eval-expression)
            (eq this-command 'cider-run-in-dev-namespace))
    (paredit-mode 1)
    (disable-inconvenient-paredit-keybindings-in-minor-mode)))

(defun disable-inconvenient-paredit-keybindings-in-minor-mode ()
  "Shadow RET in paredit when used from the minibuffer."
  (let ((oldmap (cdr (assoc 'paredit-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "RET") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(paredit-mode . ,newmap) minor-mode-overriding-map-alist)))

(use-package paredit
  :hook ((clojure-mode    . paredit-mode)
         (cider-repl-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-data-mode  . paredit-mode))
  :diminish " ()"

  :config
  ;; Rebind keys that clash with windmove / standard navigation to super equivalents
  (dolist (entry '(("M-s"         "s-s"         paredit-splice-sexp)
                   ("M-<up>"      "s-<up>"      paredit-splice-sexp-killing-backward)
                   ("M-<down>"    "s-<down>"    paredit-splice-sexp-killing-forward)
                   ("C-<right>"   "s-<right>"   paredit-forward-slurp-sexp)
                   ("C-<left>"    "s-<left>"    paredit-forward-barf-sexp)
                   ("C-M-<left>"  "s-S-<left>"  paredit-backward-slurp-sexp)
                   ("C-M-<right>" "s-S-<right>" paredit-backward-barf-sexp)))
    (let ((original    (nth 0 entry))
          (replacement (nth 1 entry))
          (command     (nth 2 entry)))
      (define-key paredit-mode-map (read-kbd-macro original) nil)
      (define-key paredit-mode-map (read-kbd-macro replacement) command)))

  ;; Make paredit work with delete-selection-mode
  (put 'paredit-forward-delete  'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-newline          'delete-selection t)

  ;; Enable `paredit-mode' in the minibuffer during `eval-expression'.
  (add-hook 'minibuffer-setup-hook #'conditionally-enable-paredit-mode)

  :bind (:map paredit-mode-map
              ("C-w"            . paredit-kill-region-or-backward-word)
              ("M-C-<backspace>" . backward-kill-sexp)
              ("C-d"            . paredit-forward-delete)
              ("M-("            . paredit-wrap-round)
              ("M-)"            . paredit-wrap-round-from-behind)
              ;; M-[ is the CSI escape prefix in terminals — Shift-Left sends
              ;; ESC [ which Emacs reads as M-[, stealing windmove-left.
              ;; Use C-c [ / C-c ] instead, which work in both GUI and terminal.
              ("M-["            . nil)
              ("M-]"            . nil)
              ("C-c ["          . paredit-wrap-square)
              ("C-c ]"          . paredit-wrap-square-from-behind)
              ("M-{"            . paredit-wrap-curly)
              ("M-}"            . paredit-wrap-curly-from-behind)))

(provide 'setup-paredit)
;;; setup-paredit.el ends here
