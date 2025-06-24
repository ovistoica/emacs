;; setup-project-icons.el
;; Shipclojure icons are stored in the repository

(require 'setup-babashka)

(defun project-icons--dir ()
  (concat (projectile-project-root) "resources/icons/"))

(defun project-icons--list-icons ()
  "List all of the icons in the project's icon dir without file
terminations."
  (->> (directory-files (project-icons--dir))
       (--filter
        (s-ends-with? ".svg" it))
       (--map
        (file-name-sans-extension it))))


(defun project-icons--insert-icon ()
  "Insert icon from the ones available in the project's dir at point"
  (interactive)
  (let ((icon (completing-read (format "Insert icon: ") (project-icons--list-icons))))
    (insert (concat ":" icon))))

(defun project-icons--find-lucide-icon ()
  (interactive)
  (let ((icon (read-string "Icon to search: ")))
    (browse-url (concat "https://lucide.dev/icons/?focus=&search=" icon))))

(defun project-icons--valid-svg-p (svg-string)
  "Check if string is a valid SVG."
  (and (string-match-p "^<svg" svg-string)
       (string-match-p "</svg>$" svg-string)
       (condition-case nil
           (with-temp-buffer
             (insert svg-string)
             (xml-parse-region (point-min) (point-max))
             t)
         (error (format "Not a valid svg: %s" svg-string)))))

(defun project-icons--extract-lucide-icon-name (svg-content)
  "Extract the lucide icon name from SVG HTML content.

   Looks for class names like 'lucide-chart-bar' or 'lucide-chart-bar-icon'
   and returns the icon name part (e.g., 'chart-bar').

   The function prioritizes finding the base icon name without the '-icon' suffix.

   Examples:
   - Input: class=\"lucide lucide-chart-bar-icon lucide-chart-bar\"
     Output: \"chart-bar\"
   - Input: class=\"lucide lucide-power-icon\"
     Output: \"power\"
   - Input: class=\"lucide lucide-hand\"
     Output: \"hand\"

   Returns nil if no lucide icon name is found."
  (when (stringp svg-content)
    (cond
     ;; First try to match lucide-{name}-icon pattern and extract just the name
     ((string-match "class=\"[^\"]*lucide-\\([a-z-]+\\)-icon\\b[^\"]*\"" svg-content)
      (match-string 1 svg-content))
     ;; Then try to match lucide-{name} pattern (without -icon)
     ((string-match "class=\"[^\"]*lucide-\\([a-z-]+\\)\\b[^\"]*\"" svg-content)
      (match-string 1 svg-content))
     ;; No match found
     (t nil))))



(defun project-icons--save-icon-from-kill ()
  "Takes a svg html saved in the kill ring and adds it to the project icons dir"
  (interactive)
  (let ((icon-content (x-get-selection 'CLIPBOARD)))
    (if (project-icons--valid-svg-p icon-content)
        (let* ((icon-name (or (project-icons--extract-lucide-icon-name icon-content)
                              (read-string "Icon name: ")))
               (filename (concat (project-icons--dir) icon-name ".svg")))
          (with-temp-file filename (insert icon-content))
          (message "%s written succesfully!" (concat "resources/icons/" icon-name ".svg")))
      (message "Content from clipboard is not valid svg: %s" icon-content))))

(defvar project-icons-map (make-sparse-keymap)
  "Keymap for project icons commands.")

;; Define the key bindings in the keymap
(define-key project-icons-map (kbd "i") 'project-icons--insert-icon)
(define-key project-icons-map (kbd "f") 'project-icons--find-lucide-icon)
(define-key project-icons-map (kbd "w") 'project-icons--save-icon-from-kill)

;; Bind the keymap to s-i
(global-set-key (kbd "s-i") project-icons-map)

(provide 'setup-project-icons)
