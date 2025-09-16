(use-package html-to-hiccup
  :defer t)

(defun my/string-classnames-to-vector (classnames-string)
  "Convert a string of HTML classes to a Clojure vector.
Classes become keywords if they don't contain '/' or brackets '[', ']'.
Otherwise they remain as strings."
  (let* ((classes (split-string classnames-string))
         (converted-classes
          (mapcar (lambda (class)
                    (if (or (string-match-p "/" class)
                            (string-match-p "\\[" class)
                            (string-match-p "\\]" class))
                        (format "\"%s\"" class)
                      (format ":%s" class)))
                  classes)))
    (format "[%s]" (string-join converted-classes " "))))

(defun my/convert-classes-to-vector ()
  "Convert HTML class string at point to Clojure vector of keywords/strings.
If cursor is inside a string, go to the beginning of the string first."
  (interactive)
  (save-excursion
    (let ((start nil)
          (end nil)
          (string-content nil))

      ;; First, try to find if we're inside a string using syntax-ppss
      (when (nth 3 (syntax-ppss))
        (while (nth 3 (syntax-ppss))
          (backward-char))
        (forward-char)) ; Move past the opening quote

      ;; If syntax-ppss didn't work, try to find quotes manually
      (unless (looking-at "\"")
        (let ((orig-point (point)))
          ;; Look backwards for opening quote
          (when (search-backward "\"" (line-beginning-position) t)
            (let ((quote-start (point)))
              ;; Check if there's a matching closing quote after original position
              (when (search-forward "\"" (line-end-position) t)
                (let ((quote-end (point)))
                  (when (and (>= orig-point quote-start) (<= orig-point quote-end))
                    (goto-char quote-start))))))))

      ;; Now we should be at the start of a string
      (if (looking-at "\"")
          (progn
            (setq start (point))
            (forward-char) ; Skip opening quote
            (let ((string-start (point)))
              (skip-chars-forward "^\"")
              (setq string-content (buffer-substring-no-properties string-start (point)))
              (forward-char) ; Skip closing quote
              (setq end (point))))
        (error "Not at a string"))

      ;; Replace the string with converted vector
      (when (and start end string-content)
        (delete-region start end)
        (insert (my/string-classnames-to-vector string-content))))))

(defun my/uix-to-hiccup (uix-string)
  "Convert UIX syntax to Hiccup syntax recursively.
Example: '($ :div.class1.class2 {:class \"hello1\"})'
becomes '[:div.class1.class2 {:class \"hello1\"}]'"
  (let ((trimmed (string-trim uix-string)))
    (if (and (string-prefix-p "($" trimmed)
             (string-suffix-p ")" trimmed))
        ;; Process UIX expression
        (let ((inner (string-trim (substring trimmed 2 -1))))
          ;; Recursively process nested ($ expressions within the inner content
          (let ((processed-inner (my/replace-nested-uix inner)))
            (format "[%s]" processed-inner)))
      ;; If it doesn't match UIX pattern, still check for nested expressions
      (my/replace-nested-uix uix-string))))

(defun my/replace-nested-uix (content)
  "Replace all nested ($ expressions in content with hiccup equivalents."
  (let ((result content)
        (start 0))
    (while (string-match "(\\$[[:space:]]" result start)
      (let* ((match-start (match-beginning 0))
             (paren-count 1)
             (pos (1+ match-start))
             (match-end nil))
        ;; Find the matching closing parenthesis
        (while (and (< pos (length result)) (> paren-count 0))
          (let ((char (aref result pos)))
            (cond
             ((eq char ?\() (setq paren-count (1+ paren-count)))
             ((eq char ?\)) (setq paren-count (1- paren-count))))
            (setq pos (1+ pos))))
        (when (= paren-count 0)
          (setq match-end pos)
          (let* ((uix-expr (substring result match-start match-end))
                 (hiccup-expr (my/uix-to-hiccup uix-expr)))
            (setq result (concat (substring result 0 match-start)
                                 hiccup-expr
                                 (substring result match-end)))
            (setq start (+ match-start (length hiccup-expr)))))
        (when (>= paren-count 1)
          ;; Couldn't find matching paren, move past this match
          (setq start (1+ match-start)))))
    result))

(defun my/uix-to-hiccup-yank ()
  "Convert UIX from kill ring to Hiccup and insert at point."
  (interactive)
  (let* ((kill-ring-content (current-kill 0))
         (hiccup-result (my/uix-to-hiccup kill-ring-content)))
    (insert hiccup-result)))

(defun my/uix-to-hiccup-region (start end)
  "Convert UIX in region to Hiccup, replacing in place."
  (interactive "r")
  (let* ((region-content (buffer-substring-no-properties start end))
         (hiccup-result (my/uix-to-hiccup region-content)))
    (delete-region start end)
    (insert hiccup-result)))
