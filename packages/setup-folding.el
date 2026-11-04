;;; setup-folding.el --- Code folding with built-in hideshow -*- lexical-binding: t; -*-

;;; Commentary:

;; Fold code blocks in every `prog-mode' buffer with the built-in
;; `hs-minor-mode'.
;;
;; `hs-hide-block-behavior' is set to `after-point' so that folding
;; hides the form starting at/after point (e.g. a map `{...}' keeps its
;; first line and closing delimiter visible) instead of the enclosing
;; defun-level block.  Verified live on Emacs 31.1 with clojure-mode.
;;
;; `hs-minor-mode' is hooked directly: on Emacs 31 it never signals in
;; major modes without comment syntax, it just messages and turns
;; itself off again.
;;
;; Fringe indicators are left off (the default); set
;; `hs-show-indicators' to t to turn them on.

;;; Code:

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ("C-<tab>" . hs-toggle-hiding)
              ("C-S-<tab>" . hs-toggle-all))
  :custom
  (hs-hide-block-behavior 'after-point)
  (hs-display-lines-hidden t))

(provide 'setup-folding)
;;; setup-folding.el ends here
