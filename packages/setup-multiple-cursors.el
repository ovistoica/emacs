(use-package multiple-cursors
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-a" . mc/edit-beginnings-of-lines)
         ("C-S-c C-e" . mc/edit-ends-of-lines)
         ("s->" . mc/mark-next-like-this)
         ("s-<" . mc/mark-previous-like-this)
         ("s-a" . mc/mark-all-dwim)))

(provide 'setup-multiple-cursors)
