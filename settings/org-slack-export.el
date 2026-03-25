;;; org-slack-export.el --- Org export backend for Slack -*- lexical-binding: t -*-

;;; Commentary:
;; Exports org-mode documents to Slack-formatted text.
;;
;; Slack's markup differs from standard markdown in several ways:
;;   - Bold:            *bold*
;;   - Italic:          _italic_
;;   - Strikethrough:   ~strikethrough~
;;   - Inline code:     `code`
;;   - Code blocks:     ``` without a language identifier
;;   - No headings:     all levels become bold text
;;   - Links:           [label](url) standard markdown (works in Slack)
;;
;; Entry points:
;;   M-x org-slack-export-to-buffer    — export to *Org Slack Export* buffer
;;   M-x org-slack-export-to-clipboard — export and copy to clipboard
;;   C-c C-e k b / k c                 — via the org export dispatcher

;;; Code:

(require 'ox)
(require 'ox-md)

;;; Transcoders

(defun org-slack-bold (_bold contents _info)
  "Transcode BOLD object to Slack *bold*.
CONTENTS is the transcoded text inside the bold markup."
  (format "*%s*" contents))

(defun org-slack-italic (_italic contents _info)
  "Transcode ITALIC object (/italic/) to Slack _italic_.
CONTENTS is the transcoded text inside the italic markup."
  (format "_%s_" contents))

(defun org-slack-underline (_underline contents _info)
  "Transcode UNDERLINE object — no underline in Slack, return CONTENTS as-is."
  contents)

(defun org-slack-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH object (+text+) to Slack ~strikethrough~.
CONTENTS is the transcoded text inside the strikethrough markup."
  (format "~%s~" contents))

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
Nested headings are indented with spaces proportional to their level."
  (let* ((title (org-export-data (org-element-property :title headline) info))
         (level (org-element-property :level headline))
         (indent (make-string (* 2 (1- level)) ?\s)))
    (concat indent (format "*%s*\n" title)
            (when contents
              (replace-regexp-in-string "^" indent contents)))))

(defun org-slack-src-block (src-block _contents _info)
  "Transcode SRC-BLOCK to Slack code block without the language label."
  (let ((code (string-trim-right (org-element-property :value src-block))))
    (format "```\n%s\n```" code)))

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

;;; Backend registration

(org-export-define-derived-backend 'slack 'md
  :menu-entry
  '(?k "Export to Slack"
       ((?b "To buffer"    (lambda (a s v b)
                             (org-export-to-buffer 'slack "*Org Slack Export*"
                               a s v b)))
        (?c "To clipboard" (lambda (a s v _b)
                             (kill-new (org-export-as 'slack s v))
                             (message "Slack markup copied to clipboard")))))
  :options-alist
  '((:with-toc nil "toc" nil))          ; disable table of contents by default
  :translate-alist
  '((bold          . org-slack-bold)
    (italic        . org-slack-italic)
    (underline     . org-slack-underline)
    (strike-through . org-slack-strike-through)
    (code          . org-slack-code)
    (verbatim      . org-slack-verbatim)
    (headline      . org-slack-headline)
    (src-block     . org-slack-src-block)
    (link          . org-slack-link)))

;;; Interactive entry points

(defun org-slack-export-to-buffer ()
  "Export current org buffer to Slack markup in *Org Slack Export*."
  (interactive)
  (org-export-to-buffer 'slack "*Org Slack Export*")
  (with-current-buffer "*Org Slack Export*"
    (goto-char (point-min))))

(defun org-slack-export-to-clipboard ()
  "Export current org buffer to Slack markup and copy to clipboard."
  (interactive)
  (kill-new (org-export-as 'slack))
  (message "Slack markup copied to clipboard"))

(provide 'org-slack-export)
;;; org-slack-export.el ends here
