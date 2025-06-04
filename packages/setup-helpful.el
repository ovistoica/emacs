;; setup-helpful.el
;;
;; Better help documentation that adds info about the code itself and much more

(use-package helpful
  :ensure helpful
  :bind
  ("C-c h" . helpful-at-point)
  ("<f1> f" . helpful-callable)
  ("<f1> k" . helpful-key)
  ("<f1> v" . helpful-variable)
  ("<f1> x" . helpful-command))
