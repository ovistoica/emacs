;;; setup-org-present.el --- Org presentation configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for org-present to do beautiful org presentation videos.
;; Based on https://systemcrafters.net/emacs-tips/presentations-with-org-present
;; and Alvaro Ramirez's (xenodium) org-present setup.
;;
;; Usage:
;;   1. Open any .org file
;;   2. M-x org-present   to start the presentation
;;   3. Navigate slides:
;;      - <right> / C-c C-n  → next slide / next item within slide
;;      - <left>  / C-c C-p  → previous slide / previous item within slide
;;   4. M-x org-present-quit   to exit (or press q)
;;
;; Each top-level heading (*, level 1) becomes a slide.
;; Sub-headings and items are revealed incrementally with C-c C-n.

;;; Code:

;; ---------------------------------------------------------------------------
;; Helper navigation functions used by org-present hooks
;; ---------------------------------------------------------------------------

(defun my/org-next-visible-heading-pos (&optional backward)
  "Return position of next visible heading, or nil if none.
Set BACKWARD to search backwards."
  (save-excursion
    (let ((pos-before (point))
          (pos-after (progn
                       (org-next-visible-heading (if backward -1 1))
                       (point))))
      (when (and pos-after (not (equal pos-before pos-after)))
        pos-after))))

(defun my/org-next-link-pos (&optional backward)
  "Return position of next org link, or nil if none.
Set BACKWARD to search backwards."
  (save-excursion
    (let* ((inhibit-message t)
           (pos-before (point))
           (pos-after (progn
                        (org-next-link backward)
                        (point))))
      (when (and pos-after (or (and backward (> pos-before pos-after))
                               (and (not backward) (> pos-after pos-before))))
        pos-after))))

(defun my/org-next-block-pos (&optional backward)
  "Return position inside next src block body, or nil if none.
Set BACKWARD to search backwards."
  (save-excursion
    (when (and backward (org-babel-where-is-src-block-head))
      (org-babel-goto-src-block-head))
    (let ((pos-before (point))
          (pos-after (ignore-errors
                       (org-next-block 1 backward)
                       (point))))
      (when (and pos-after (not (equal pos-before pos-after)))
        ;; Place point inside block body.
        (goto-char (line-beginning-position 2))
        (point)))))

(defun my/org-present-reveal-level2 ()
  "Reveal a level-2 heading and its subtree inside the current slide."
  (interactive)
  (let ((loc (point))
        (level (org-current-level))
        (heading))
    (ignore-errors (org-back-to-heading t))
    (while (or (not level) (> level 2))
      (setq level (org-up-heading-safe)))
    (setq heading (point))
    (goto-char (point-min))
    (org-overview)
    (goto-char heading)
    (while (org-up-heading-safe))
    (org-fold-show-branches)
    (org-show-children)
    (run-hook-with-args 'org-cycle-hook 'children)
    (goto-char heading)
    (org-show-subtree)
    (goto-char loc)))

;; ---------------------------------------------------------------------------
;; Present next/previous item (incremental reveal within a slide)
;; ---------------------------------------------------------------------------

(defun my/org-present-next-item (&optional backward)
  "Reveal the next item (heading, link, or block) within the current slide.
With BACKWARD non-nil, reveal the previous item.
At slide boundaries, navigate to next/previous slide."
  (interactive "P")
  ;; At beginning of slide going backward → go to previous slide.
  (if (and backward (eq (point) (point-min)))
      (org-present-prev)
    (let* ((heading-pos (my/org-next-visible-heading-pos backward))
           (link-pos    (my/org-next-link-pos backward))
           (block-pos   (my/org-next-block-pos backward))
           (closest-pos (when (or heading-pos link-pos block-pos)
                          (apply (if backward #'max #'min)
                                 (seq-filter #'identity
                                             (list heading-pos link-pos block-pos))))))
      (if closest-pos
          (progn
            (cond ((eq heading-pos closest-pos) (goto-char heading-pos))
                  ((eq link-pos closest-pos)    (goto-char link-pos))
                  ((eq block-pos closest-pos)   (goto-char block-pos)))
            ;; Reveal relevant content based on heading level.
            (cond ((> (org-current-level) 1)
                   (my/org-present-reveal-level2))
                  ((eq (org-current-level) 1)
                   ;; Level 1 — collapse children, show top level only.
                   (org-overview)
                   (org-show-entry)
                   (org-show-children)
                   (run-hook-with-args 'org-cycle-hook 'children))))
        ;; Nothing left on this slide → go to next slide.
        (org-present-next)))))

(defun my/org-present-previous-item ()
  "Reveal the previous item within the current slide, or go to previous slide."
  (interactive)
  (my/org-present-next-item t))

;; ---------------------------------------------------------------------------
;; Mode enter / exit hooks
;; ---------------------------------------------------------------------------

(defun my/org-present-mode-hook ()
  "Set up a beautiful presentation view."
  (setq-local face-remapping-alist
              '((default             (:height 1.4)           default)
                ;; Level-1 = slide title — large and bold, like xenodium's style.
                (org-level-1         (:height 2.0 :weight bold) org-level-1)
                ;; Level-2 = subtitle — normal weight, slightly smaller.
                (org-level-2         (:height 1.4 :weight normal) org-level-2)
                ;; Hide src block delimiters entirely.
                (org-block-begin-line (:height 0)            org-block-begin-line)
                (org-block-end-line   (:height 0)            org-block-end-line)))
  ;; Add top padding via a tall header line.
  ;; Explicitly inherit the buffer background so the header-line
  ;; doesn't show a differently-coloured band at the top.
  (setq-local header-line-format
              (propertize " " 'face
                          `(:height 300
                            :background ,(face-background 'default nil t)
                            :foreground ,(face-background 'default nil t))))
  ;; Hide the mode line for a clean look.
  (hide-mode-line-mode +1)
  ;; Center and wrap content.
  (visual-fill-column-mode +1)
  (visual-line-mode +1)
  ;; Show top-level children at start.
  (org-show-children)
  (run-hook-with-args 'org-cycle-hook 'children))

(defun my/org-present-mode-quit ()
  "Restore normal editing view after presentation."
  (setq-local face-remapping-alist nil)
  (setq-local header-line-format nil)
  (hide-mode-line-mode -1)
  (visual-fill-column-mode -1)
  (visual-line-mode -1))

;; ---------------------------------------------------------------------------
;; visual-fill-column — centers and wraps text nicely during presentations
;; ---------------------------------------------------------------------------

(use-package visual-fill-column
  :ensure t
  :defer t
  :custom
  ;; Wide enough to breathe, but not full-width.
  (visual-fill-column-width 80)
  ;; Left-align like the xenodium style — content hugs the left margin.
  (visual-fill-column-center-text nil))

;; ---------------------------------------------------------------------------
;; hide-mode-line — hides mode line for distraction-free presentations
;; ---------------------------------------------------------------------------

(use-package hide-mode-line
  :ensure t
  :defer t)

;; ---------------------------------------------------------------------------
;; org-present
;; ---------------------------------------------------------------------------

(use-package org-present
  :ensure t
  :defer t
  :bind (:map org-present-mode-keymap
              ("C-c C-n" . my/org-present-next-item)
              ("C-c C-p" . my/org-present-previous-item))
  :custom
  ;; Text scale applied to the whole buffer (relative to default).
  (org-present-text-scale 2)
  :hook ((org-present-mode      . my/org-present-mode-hook)
         (org-present-mode-quit . my/org-present-mode-quit))
  :config
  ;; After navigating to a new slide, show only its direct children
  ;; (so the audience sees one slide at a time).
  (add-hook 'org-present-after-navigate-functions
            (defun my/org-present-after-navigate (_buffer-name _heading)
              (org-overview)      ;; collapse everything
              (org-show-entry)    ;; unfold current entry body
              (org-show-children) ;; show direct sub-headings only
              (run-hook-with-args 'org-cycle-hook 'children))))

(provide 'setup-org-present)
;;; setup-org-present.el ends here
