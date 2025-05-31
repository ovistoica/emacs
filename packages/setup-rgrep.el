(require 'grep)

(global-set-key (kbd "M-s M-s") 'rgrep-fullscreen)

(defvar rgrep-default-search-patterns
  '("*.el" "*.clj" "*.cljs" "*.clj*" "*.edn" "*.md" "*.css"))

(defun rgrep-fullscreen (regexp &optional files dir confirm)
  "Open grep in full screen, saving windows."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
                                   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
                (my-extension (when (buffer-file-name)
                                (when-let ((my-extension (file-name-extension (buffer-file-name))))
                                  (concat "*." my-extension))))
                (files (completing-read (format "Search for %s in files matching: " regexp)
                                        (if my-extension
                                            (cons my-extension (--remove (equal it my-extension) rgrep-default-search-patterns))
                                          rgrep-default-search-patterns)
                                        nil nil nil nil my-extension))
                (dir (ido-read-directory-name "Base directory: "
                                              nil default-directory t))
                (confirm (equal current-prefix-arg '(4))))
           (list regexp files dir confirm))))))
  (window-configuration-to-register ?$)
  (rgrep regexp files dir confirm)
  (switch-to-buffer "*grep*")
  (delete-other-windows)
  (goto-char (point-min)))

(defun rgrep-quit-window ()
  (interactive)
  (kill-buffer)
  (jump-to-register ?$))

(defun rgrep-goto-file-and-close-rgrep ()
  (interactive)
  (compile-goto-error)
  (kill-buffer "*grep*")
  (delete-other-windows)
  (message "Type C-x r j $ to return to pre-rgrep windows."))

;; Don't recurse into some directories
(add-to-list 'grep-find-ignored-directories "target")
(add-to-list 'grep-find-ignored-directories "node_modules")
(add-to-list 'grep-find-ignored-directories "vendor")

;; Add custom keybindings 
(define-key grep-mode-map "q" 'rgrep-quit-window)
(define-key grep-mode-map (kbd "C-<return>") 'rgrep-goto-file-and-close-rgrep)
(define-key grep-mode-map (kbd "C-x C-s") 'wgrep-save-all-buffers)

;; Use same keybinding as occur
(setq wgep-enable-key "e")

(provide 'rgrep)
