;; Perspective
;;
;; Provides multiple named workspaces (or "perspectives") in Emacs, similar to
;; multiple desktops in window managers like Awesome and XMonad, and Spaces on
;; the Mac.

(defun my/persp-sort-buffer-names-by-recency (names)
  "Sort NAMES (buffer names) by recency of selection.
Most recently visited buffers come first, following `buffer-list' order.
Names not present in `buffer-list' are kept at the end in their original order."
  (let ((recency (make-hash-table :test 'equal))
        (i 0))
    (dolist (buf (buffer-list))
      (puthash (buffer-name buf) i recency)
      (setq i (1+ i)))
    (sort (copy-sequence names)
          (lambda (a b)
            (< (gethash a recency most-positive-fixnum)
               (gethash b recency most-positive-fixnum))))))

(use-package perspective
  ;; C-x b is `consult-buffer' (setup-consult.el) with the perspective source
  ;; as default; `persp-switch-to-buffer*' remains on C-x x b.
  :bind (("C-x k" . persp-kill-buffer*))
  :init
  (setq persp-mode-prefix-key (kbd "C-x x"))
  (persp-mode)
  :config
  (setq persp-modestring-short t)
  (persp-add-buffer-to-frame-global "*Messages*")
  (persp-add-buffer-to-frame-global "*Warnings*")
  (persp-add-buffer-to-frame-global "*lsp-log*")

  ;; Order `persp-switch-to-buffer*' candidates by recency (most recently
  ;; visited first).  persp feeds candidates to `completing-read' without a
  ;; `display-sort-function', so Vertico would otherwise re-sort them with
  ;; `vertico-sort-history-length-alpha' and discard recency.  Register a
  ;; command-specific Vertico sort instead; it is honoured only here.
  (with-eval-after-load 'vertico-multiform
    (add-to-list 'vertico-multiform-commands
                 '(persp-switch-to-buffer*
                   (vertico-sort-function . my/persp-sort-buffer-names-by-recency)))))

;; Avoid popping ediff up in separate window, it breaks perspective
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;; Macro to open perspective with `name' and evaluate `body'
(defmacro with-perspective (name &rest body)
  `(let ((initialize (not (gethash ,name (perspectives-hash))))
         (current-perspective (persp-curr)))
     (persp-switch ,name)
     (when initialize ,@body)
     (set-frame-parameter nil 'persp--last current-perspective)))

(provide 'setup-perspective)
