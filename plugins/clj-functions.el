;;; clj-functions.el --- Helpers in clj buffers.
;;; Commentary:
;;; Helpers in clojure buffers
;;; Code:

(defgroup clj-functions nil
  "Utilities for Clojure buffers."
  :group 'languages
  :prefix "clj-functions-")

(defcustom clj-functions-html-tags
  '("a" "abbr" "address" "area" "article" "aside" "audio"
    "b" "base" "bdi" "bdo" "blockquote" "body" "br" "button"
    "canvas" "caption" "cite" "code" "col" "colgroup"
    "data" "datalist" "dd" "del" "details" "dfn" "dialog" "div" "dl" "dt"
    "em" "embed"
    "fieldset" "figcaption" "figure" "footer" "form"
    "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hr" "html"
    "i" "iframe" "img" "input" "ins"
    "kbd"
    "label" "legend" "li" "link"
    "main" "map" "mark" "meta" "meter"
    "nav" "noscript"
    "object" "ol" "optgroup" "option" "output"
    "p" "param" "picture" "pre" "progress"
    "q"
    "rp" "rt" "ruby"
    "s" "samp" "script" "section" "select" "small" "source" "span"
    "strong" "style" "sub" "summary" "sup" "svg"
    "table" "tbody" "td" "template" "textarea" "tfoot" "th"
    "thead" "time" "title" "tr" "track"
    "u" "ul"
    "var" "video"
    "wbr")
  "List of valid HTML tags."
  :type '(repeat string)
  :group 'clj-functions)

(defun clj-functions-valid-html-tag-p (tag-name)
  "Check if TAG-NAME is a valid HTML tag.
Returns t if tag is valid, nil otherwise.
TAG-NAME can be with or without angle brackets."
  (let* ((cleaned-tag (replace-regexp-in-string "[<>/]" "" tag-name))
         (lowercase-tag (downcase cleaned-tag)))
    (not (null (member lowercase-tag clj-functions-html-tags)))))

(defun clj-functions-sanitize-hiccup-kw (kw)
  "Transform a hiccup KW to a string.
:div.flex.items-center => \"div.flex.items-center\""
  (let ((str (if (stringp kw) kw (symbol-name kw))))
    (if (string-prefix-p ":" str)
        (substring str 1)
      str)))

(defun clj-functions-split-first (string separator)
  "Split STRING on first occurrence of SEPARATOR.
Returns a list of two strings: the part before and after SEPARATOR.
If SEPARATOR is not found, returns a list with STRING and nil.
SEPARATOR can be either a string or a character."
  (let* ((sep (if (characterp separator)
                  (char-to-string separator)
                separator))
         (pos (string-match (regexp-quote sep) string)))
    (if pos
        (list (substring string 0 pos)
              (substring string (+ pos (length sep))))
      (list string nil))))

(defun clj-functions-split-tag-and-css (input)
  "Split the INPUT if there si a html tag as beginning."
  (let* ((split (clj-functions-split-first input ".")))
    (if (clj-functions-valid-html-tag-p (car split))
        split
      input)))

(defun clj-functions--hiccup-classes-to-string (kw)
  "Extract CSS classes from a Hiccup-style KW and return as space-separated string.
Takes a keyword like :div.flex.items-center and returns the
classes as a string.  Returns nil if no classes are found.

Example:
  (hiccup-classes-to-string :div.flex.items-center)
  => \"flex items-center\""
  (interactive)
  (let* ((sanitized (clj-functions-sanitize-hiccup-kw kw))
         (split (clj-functions-split-tag-and-css sanitized))
         (str (if (listp split) (cadr split) split)))
    (string-trim
     (replace-regexp-in-string
      "\\." " "
      str))))

(defun clj-functions-hiccup-classes-to-string (&optional kw)
  "Extract CSS classes from a Hiccup-style KW and return as space-separated string.
Takes a keyword like :div.flex.items-center and returns the
classes as a string.  Returns nil if no classes are found.

If no kw is provided, uses region and if no region is active
prompts user for input.

Example:
  (hiccup-classes-to-string :div.flex.items-center)
  => \"flex items-center\""
  (interactive)
  (let* ((input (cond
                 ;; if explicit argument provided use it
                 (kw kw)
                 ;; if region is active, use region content
                 ((use-region-p)
                  (buffer-substring-no-properties
                   (region-beginning)
                   (region-end)))
                 (t (read-string "Enter hiccup keyword: "))))
         (result (clj-functions--hiccup-classes-to-string input)))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end))
      (insert (format "\"%s\"" result)))))

(provide 'clj-functions)
;;; clj-functions.el ends here
