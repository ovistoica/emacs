(use-package s)
(use-package dash)
(use-package diminish)

;; Shorthand for interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

(global-set-key (kbd "s-l") (λ (insert "\u03bb")))

;; Set up a keybinding for the very next command invocation
(defun one-shot-keybinding (key command)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map)
   t))

;; Instrument a `command' to store the current window configuration in
;; `register' and then going fullscreen.
(defmacro wrap-fullscreen (command)
  `(defadvice ,command (around ,(intern (concat "wrap-" (symbol-name command) "-fullscreen")) activate)
     (let ((my/prev (list (current-window-configuration) (point-marker))))
       ad-do-it
       (delete-other-windows)
       (setq-local my/previous-window-configuration my/prev))))

(defvar my/previous-window-configuration nil)

;; No need to remind me about eldoc-mode all the time
(diminish 'eldoc-mode)

(provide 'tooling)


