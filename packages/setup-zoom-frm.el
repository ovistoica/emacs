;; Zoom FRM
;;
;; Change text size for entire frame, not just a single buffer, lol

;; Dependencies for zoom-frm
(use-package frame-fns
  :vc (:url "https://github.com/emacsmirror/frame-fns"))

(use-package frame-cmds
  :vc (:url "https://github.com/emacsmirror/frame-cmds")
  :after frame-fns)

(use-package zoom-frm
  :vc (:url "https://github.com/emacsmirror/zoom-frm")
  :after frame-cmds
  :bind (("C-x C-+" . zoom-frm-in)
         ("C-x C--" . zoom-frm-out)
         ("C-x C-0" . zoom-frm-unzoom)))

(provide 'setup-zoom-frm)
