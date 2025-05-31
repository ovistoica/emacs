;; Moving the cursor around

;; Like isearch, but adds region (if any) to history and deactivates mark 
(global-set-key (kbd "C-s") 'isearch-forward-use-region)
(global-set-key (kbd "C-r") 'isearch-backward-use-region)

;; Navigate between windows using shift + arrow key
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)

;: Keep them windows nice and balanced
(global-set-key (kbd "C-x 0") 'my/delete-window)
(global-set-key (kbd "C-x 3") 'my/split-window-right)

;; Move cursor back to indentation
(global-set-key (kbd "M-i") 'back-to-indentation)

;; Navigate paragraphs
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; Move more quickly
(global-set-key (kbd "C-S-n") (λ (ignore-errors (forward-line 5) (recenter))))
(global-set-key (kbd "C-S-p") (λ (ignore-errors (forward-line -5) (recenter))))
(global-set-key (kbd "C-S-f") (λ (ignore-errors (forward-char 5) (recenter))))
(global-set-key (kbd "C-S-b") (λ (ignore-errors (backward-char 5) (recenter))))


;; Readily navigate in modes with significant whitespace
(global-set-key (kbd "H-n") 'goto-next-line-with-same-indentation)
(global-set-key (kbd "H-p") 'goto-prev-line-with-same-indentation)

;; Where was I again?
(global-set-key (kbd "M-B") 'goto-last-modification)

;; Implementations

(use-package windmove)

(defun isearch-forward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (buffer-substring (region-beginning)
                                                   (region-end)))
    (deactivate-mark))
  (call-interactively 'isearch-forward))


(defun isearch-forward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (buffer-substring (region-beginning)
                                                   (region-end)))
    (deactivate-mark))
  (call-interactively 'isearch-forward))

(defun isearch-backward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (buffer-substring (region-beginning)
                                                   (region-end)))
    (deactivate-mark))
  (call-interactively 'isearch-backward))

(defun goto-next-line-with-same-indentation ()
  (interactive)
  (back-to-indentation)
  (re-search-forward (s-concat "^" (s-repeat (current-column) " ") "[^ \t\r\n\v\f]")
                     nil nil (if (= 0 (current-column)) 2 1))
  (back-to-indentation))

(defun goto-prev-line-with-same-indentation ()
  (interactive)
  (back-to-indentation)
  (re-search-backward (s-concat "^" (s-repeat (current-column) " ") "[^ \t\r\n\v\f]"))
  (back-to-indentation))

(defun goto-last-modification ()
  (interactive)
  (undo-fu-only-undo)
  (undo-fu-only-redo))

(defun my/delete-window ()
  (interactive)
  (delete-window)
  (balance-windows))

(defun my/split-window-right ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (balance-windows))

(provide 'navigation)
