;;; org-slack-export.el --- Org export backend for Slack -*- lexical-binding: t -*-

;;; Commentary:
;; Exports org-mode documents to Slack-formatted text.
;;
;; Slack now interprets pasted text as standard (CommonMark) markdown, where a
;; single asterisk means italic.  The legacy `mrkdwn' dialect it replaced used
;; a single asterisk for bold.  The flavor is chosen with
;; `org-slack-export-flavor'; the default `markdown' emits:
;;   - Bold:            **bold**
;;   - Italic:          _italic_
;;   - Strikethrough:   ~~strikethrough~~
;;   - Inline code:     `code`
;;   - Code blocks:     ``` without a language identifier
;;   - No headings:     all levels become bold text
;;   - Links:           [label](url)
;;   - Tables:          native markdown tables (| a | b |), with the first
;;                      row as the header and inline markup kept in cells
;;
;; Set `org-slack-export-flavor' to `mrkdwn' for the legacy syntax, which
;; differs in bold (*bold*) and strikethrough (~strikethrough~).  Legacy
;; Slack has no table markup, so there tables are rendered as a
;; space-aligned ASCII grid wrapped in a ``` code block.
;;
;; Entry points:
;;   M-x org-slack-export-to-buffer    — export to *Org Slack Export* buffer
;;   M-x org-slack-export-to-clipboard — export and copy to clipboard
;;   C-c C-e k b / k c                 — via the org export dispatcher
;;   C-c C-e k B / k C                 — same, but indent nested headings to
;;                                       visualize the outline hierarchy
;;
;; Heading indentation is off by default.  Enable it per-export with the
;; capitalized menu keys above, or per-file with `#+OPTIONS: slack-indent:t'.

;;; Code:

(require 'ox)
(require 'ox-md)
(require 'seq)
(require 'subr-x)

;;; Customization

(defgroup org-slack-export nil
  "Org export backend for Slack."
  :group 'org-export
  :prefix "org-slack-")

(defcustom org-slack-export-flavor 'markdown
  "Slack text syntax produced by the export backend.
`markdown' targets Slack's standard markdown parsing, where `**bold**'
is bold and a single asterisk is italic, and tables are native markdown
tables.  `mrkdwn' targets the legacy Slack dialect, where `*bold*' is
bold and tables fall back to a code-fenced ASCII grid."
  :type '(choice (const :tag "Standard markdown (**bold**, ~~strike~~)" markdown)
                 (const :tag "Legacy mrkdwn (*bold*, ~strike~)" mrkdwn))
  :group 'org-slack-export)

;;; Transcoders

(defun org-slack--bold (text)
  "Wrap TEXT in bold markup for `org-slack-export-flavor'."
  (format (if (eq org-slack-export-flavor 'mrkdwn) "*%s*" "**%s**") text))

(defun org-slack--strike (text)
  "Wrap TEXT in strikethrough markup for `org-slack-export-flavor'."
  (format (if (eq org-slack-export-flavor 'mrkdwn) "~%s~" "~~%s~~") text))

(defun org-slack-bold (_bold contents _info)
  "Transcode BOLD object to Slack bold, e.g. **bold**.
CONTENTS is the transcoded text inside the bold markup.  The delimiter
depends on `org-slack-export-flavor'."
  (org-slack--bold contents))

(defun org-slack-italic (_italic contents _info)
  "Transcode ITALIC object (/italic/) to Slack _italic_.
CONTENTS is the transcoded text inside the italic markup."
  (format "_%s_" contents))

(defun org-slack-underline (_underline contents _info)
  "Transcode UNDERLINE object — no underline in Slack, return CONTENTS as-is."
  contents)

(defun org-slack-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH object (+text+) to Slack ~~strikethrough~~.
CONTENTS is the transcoded text inside the strikethrough markup.  The
delimiter depends on `org-slack-export-flavor'."
  (org-slack--strike contents))

(defun org-slack-code (code _contents _info)
  "Transcode inline CODE object (=code=) to Slack `backtick`."
  (format "`%s`" (org-element-property :value code)))

(defun org-slack-verbatim (verbatim _contents _info)
  "Transcode VERBATIM object (~verbatim~) to Slack `backtick`."
  (format "`%s`" (org-element-property :value verbatim)))

(defun org-slack-headline (headline contents info)
  "Transcode HEADLINE to bold text — Slack has no heading syntax.
CONTENTS is the body text and subheadings under this headline.
INFO is the export plist used to transcode the headline title.
When the `:slack-indent-headlines' option is non-nil, nested headings
and their bodies are indented with spaces proportional to their level
to visualize the hierarchy; by default no indentation is added."
  (let* ((title (org-export-data (org-element-property :title headline) info))
         (level (org-element-property :level headline))
         (indent (if (plist-get info :slack-indent-headlines)
                     (make-string (* 2 (1- level)) ?\s)
                   "")))
    (concat indent (org-slack--bold title) "\n"
            (when contents
              (if (string-empty-p indent)
                  contents
                (replace-regexp-in-string "^" indent contents))))))

(defun org-slack-src-block (src-block _contents _info)
  "Transcode SRC-BLOCK to Slack code block without the language label."
  (let ((code (string-trim-right (org-element-property :value src-block))))
    (format "```\n%s\n```" code)))

(defun org-slack-paragraph (paragraph contents info)
  "Transcode PARAGRAPH to Slack, collapsing hard line breaks from auto-fill.
CONTENTS is the paragraph contents.  INFO is the export plist.
Unless `:preserve-breaks' is set, internal newlines (from `auto-fill-mode' or
manual wrapping) are replaced by spaces so Slack renders the paragraph as a
single flowing line — matching the behavior of ox-gfm's paragraph exporter."
  (unless (plist-get info :preserve-breaks)
    (setq contents (concat (mapconcat #'identity (split-string contents) " ") "\n")))
  (let ((first-object (car (org-element-contents paragraph))))
    (if (and (stringp first-object) (string-prefix-p "#" first-object))
        (concat "\\" contents)
      contents)))

(defun org-slack-link (link contents info)
  "Transcode LINK to standard Markdown [label](url) format.
CONTENTS is the link description, if any.
INFO is the export plist, passed through to the fallback transcoder.
Falls back to the default markdown transcoder for internal links."
  (let ((type (org-element-property :type link))
        (raw  (org-element-property :raw-link link)))
    (if (member type '("http" "https" "ftp"))
        (if (org-string-nw-p contents)
            (format "[%s](%s)" contents raw)
          (format "<%s>" raw))
      (org-md-link link contents info))))

;;; Tables
;;
;; With the `markdown' flavor, tables become native markdown tables.  With
;; the legacy `mrkdwn' flavor, which has no table markup, they are rendered
;; as a space-aligned ASCII grid wrapped in a triple-backtick code block,
;; with cell contents flattened to plain text because code blocks render
;; everything literally.

(defun org-slack--square-matrix (matrix)
  "Return MATRIX with every row padded with empty cells to the same length.
MATRIX is a list of rows, each a list of cell strings."
  (let ((ncols (apply #'max (mapcar #'length matrix))))
    (mapcar (lambda (row)
              (append row (make-list (- ncols (length row)) "")))
            matrix)))

(defun org-slack--escape-pipes (text)
  "Escape `|' characters in TEXT so they stay inside a markdown table cell."
  (replace-regexp-in-string "|" "\\\\|" text))

(defun org-slack--native-table (matrix)
  "Render MATRIX as a native markdown table.
MATRIX is a list of rows, each a list of cell strings.  Markdown tables
require a header, so the first row is always the header and is followed
by the delimiter row."
  (let* ((rows (org-slack--square-matrix matrix))
         (format-row (lambda (cells)
                       (concat "| " (string-join cells " | ") " |"))))
    (string-join
     (cons (funcall format-row (car rows))
           (cons (funcall format-row (make-list (length (car rows)) "---"))
                 (mapcar format-row (cdr rows))))
     "\n")))

(defun org-slack--element-to-plain-text (data)
  "Return the plain text of DATA, stripping all inline markup.
DATA is an Org element/object or a secondary string (list thereof).
Emphasis markers are dropped, inline code and verbatim collapse to
their literal value, and links collapse to their description or, when
absent, their raw target.  Used to render table cells, which Slack
shows as literal monospace text inside a code block."
  (cond
   ((null data) "")
   ((stringp data) data)
   ((and (consp data) (symbolp (car data)))
    ;; A single object.  Org stores whitespace that trailed the object
    ;; (e.g. the space after `*bold*') in `:post-blank' rather than in the
    ;; following string, so re-append it to avoid gluing words together.
    (let ((text (pcase (org-element-type data)
                  ((or 'code 'verbatim)
                   (or (org-element-property :value data) ""))
                  ('link
                   (if (org-element-contents data)
                       (org-slack--element-to-plain-text
                        (org-element-contents data))
                     (or (org-element-property :raw-link data) "")))
                  (_ (org-slack--element-to-plain-text
                      (org-element-contents data)))))
          (post-blank (or (org-element-property :post-blank data) 0)))
      (concat text (make-string post-blank ?\s))))
   ((consp data)
    (mapconcat #'org-slack--element-to-plain-text data ""))
   (t "")))

(defun org-slack--pad (string width)
  "Left-align STRING in a field of WIDTH columns, padding with spaces."
  (concat string (make-string (max 0 (- width (string-width string))) ?\s)))

(defun org-slack--format-table (matrix header-rows)
  "Render MATRIX as a space-aligned table wrapped in a Slack code block.
MATRIX is a list of rows, each a list of plain-text cell strings.
Columns are padded to their widest cell.  When HEADER-ROWS is a
positive integer, a `-+-' divider is inserted after that many leading
rows.  Returns the table between triple backticks so Slack keeps the
columns aligned in its monospace font."
  (let* ((rows (org-slack--square-matrix matrix))
         (ncols (length (car rows)))
         (widths (make-list ncols 0)))
    (dolist (row rows)
      (dotimes (i ncols)
        (setf (nth i widths)
              (max (nth i widths) (string-width (nth i row))))))
    (let* ((format-row
            (lambda (row)
              (string-trim-right
               (string-join
                (seq-map-indexed
                 (lambda (cell i) (org-slack--pad cell (nth i widths)))
                 row)
                " | "))))
           (divider (string-join
                     (mapcar (lambda (w) (make-string w ?-)) widths)
                     "-+-"))
           (lines '())
           (emitted 0))
      (dolist (row rows)
        (push (funcall format-row row) lines)
        (setq emitted (1+ emitted))
        (when (and header-rows (= emitted header-rows))
          (push divider lines)))
      (concat "```\n" (string-join (nreverse lines) "\n") "\n```"))))

(defun org-slack-table (table _contents info)
  "Transcode TABLE according to `org-slack-export-flavor'.
INFO is the export plist.  With the `markdown' flavor the table is a
native markdown table whose cells keep their inline markup.  With the
legacy `mrkdwn' flavor, which has no table markup, it is a monospace
ASCII grid inside a triple-backtick code block; cell markup is
flattened to plain text there, and a `-+-' divider is emitted after the
header rows when the Org table separates them with a rule."
  (let ((native (not (eq org-slack-export-flavor 'mrkdwn)))
        (header-rows nil)
        (count 0)
        (matrix '()))
    (dolist (row (org-element-map table 'table-row #'identity info))
      (pcase (org-element-property :type row)
        ('rule (when (and (null header-rows) (> count 0))
                 (setq header-rows count)))
        ('standard
         (setq count (1+ count))
         (push (org-element-map row 'table-cell
                 (lambda (cell)
                   (if native
                       (org-slack--escape-pipes
                        (string-trim (org-export-data cell info)))
                     (string-trim
                      (org-slack--element-to-plain-text
                       (org-element-contents cell)))))
                 info)
               matrix))))
    (cond
     ((null matrix) "")
     (native (org-slack--native-table (nreverse matrix)))
     (t (org-slack--format-table (nreverse matrix) header-rows)))))

(defun org-slack-table-row (_row contents _info)
  "Transcode table ROW by passing CONTENTS through.
The actual layout is handled by `org-slack-table', which reads the
table element directly; this only short-circuits the inherited HTML
row transcoder."
  (or contents ""))

(defun org-slack-table-cell (_cell contents _info)
  "Transcode table CELL by passing CONTENTS through.
The actual layout is handled by `org-slack-table', which reads the
table element directly; this only short-circuits the inherited HTML
cell transcoder."
  (or contents ""))

;;; Clipboard
;;
;; Emacs serves Latin-1-only text (for example `×' or `é') to Wayland
;; clients as ISO-8859-1 bytes under the STRING, TEXT, COMPOUND_TEXT and
;; `text/plain' targets, and only the explicit UTF-8 targets carry real
;; UTF-8.  A client that asks for one of the former, such as a browser
;; running Slack, decodes those bytes as UTF-8 and shows U+FFFD (`�').
;; `wl-copy' offers the same UTF-8 bytes under every text target instead.

(defun org-slack--copy-utf8 (text)
  "Put TEXT on the system clipboard so every text target is UTF-8.
Uses `wl-copy' in a Wayland session and falls back to `gui-select-text',
the default `interprogram-cut-function', when it is unavailable or fails."
  (if (and (getenv "WAYLAND_DISPLAY") (executable-find "wl-copy"))
      (let ((coding-system-for-write 'utf-8-unix))
        (unless (zerop (call-process-region text nil "wl-copy" nil nil nil))
          (gui-select-text text)))
    (gui-select-text text)))

(defun org-slack--copy-to-clipboard (text)
  "Add TEXT to the kill ring and copy it to the clipboard as UTF-8."
  (let ((interprogram-cut-function #'org-slack--copy-utf8))
    (kill-new text)))

(defun org-slack--prepare-export-buffer ()
  "Make copying from the current Slack export buffer UTF-8 safe.
Sets `interprogram-cut-function' buffer-locally so that `kill-ring-save'
and `kill-region' in this buffer go through `org-slack--copy-utf8'."
  (setq-local interprogram-cut-function #'org-slack--copy-utf8))

;;; Backend registration

(org-export-define-derived-backend 'slack 'md
  :menu-entry
  '(?k "Export to Slack"
       ((?b "To buffer"    (lambda (a s v b)
                             (org-export-to-buffer 'slack "*Org Slack Export*"
                               a s v b nil #'org-slack--prepare-export-buffer)))
        (?c "To clipboard" (lambda (a s v _b)
                             (org-slack--copy-to-clipboard
                              (org-export-as 'slack s v))
                             (message "Slack markup copied to clipboard")))
        (?B "To buffer (indent hierarchy)"
            (lambda (a s v b)
              (org-export-to-buffer 'slack "*Org Slack Export*"
                a s v b '(:slack-indent-headlines t)
                #'org-slack--prepare-export-buffer)))
        (?C "To clipboard (indent hierarchy)"
            (lambda (a s v _b)
              (org-slack--copy-to-clipboard
               (org-export-as 'slack s v nil '(:slack-indent-headlines t)))
              (message "Slack markup copied to clipboard")))))
  :options-alist
  ;; Disable the table of contents by default, and expose an option to
  ;; indent nested headings (also settable via `#+OPTIONS: slack-indent:t').
  '((:with-toc nil "toc" nil)
    (:slack-indent-headlines nil "slack-indent" nil))
  :translate-alist
  '((bold          . org-slack-bold)
    (italic        . org-slack-italic)
    (underline     . org-slack-underline)
    (strike-through . org-slack-strike-through)
    (code          . org-slack-code)
    (verbatim      . org-slack-verbatim)
    (headline      . org-slack-headline)
    (src-block     . org-slack-src-block)
    (paragraph     . org-slack-paragraph)
    (link          . org-slack-link)
    (table         . org-slack-table)
    (table-row     . org-slack-table-row)
    (table-cell    . org-slack-table-cell)))

;;; Interactive entry points

(defun org-slack-export-to-buffer ()
  "Export current org buffer to Slack markup in *Org Slack Export*."
  (interactive)
  (org-export-to-buffer 'slack "*Org Slack Export*"
    nil nil nil nil nil #'org-slack--prepare-export-buffer)
  (with-current-buffer "*Org Slack Export*"
    (goto-char (point-min))))

(defun org-slack-export-to-clipboard ()
  "Export current org buffer to Slack markup and copy to clipboard."
  (interactive)
  (org-slack--copy-to-clipboard (org-export-as 'slack))
  (message "Slack markup copied to clipboard"))

(provide 'org-slack-export)
;;; org-slack-export.el ends here
