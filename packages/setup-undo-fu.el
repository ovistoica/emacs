;; Undo Fu
;;
;; Simple, stable linear undo with redo for Emacs. Unlike undo-tree, does not
;; mess with Emacs internals. We still get visualisation of the tree structure
;; via vundo. In addition, undo-fu-session stores undo history accross Emacs sessions.

(use-package undo-fu
  :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq undo-limit 400000               ; 400kb (default is 160kb)
        undo-strong-limit 3000000       ; 3MB  (default is 240kb)
        undo-outer-limit 48000000       ; 48MB (default is 24mb)
        )

  :bind (([remap undo] . undo-fu-only-undo)
         ([remap redo] . undo-fu-only-redo)
         ("C-_" . undo-fu-only-undo)
         ("M-_" . undo-fu-only-redo)
         ("C-M-_" . undo-fu-only-redo-all)))

(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t)

  :bind (("C-x u" . vundo)))

(provide 'setup-undo-fu)
