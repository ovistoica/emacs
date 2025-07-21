;;; setup-man-mode.el --- Configuration for man-mode -*- lexical-binding: t -*-

;; Fix for BSD sed (macOS) compatibility issue with man pages
;; The default Man-filter-list uses octal character ranges that BSD sed doesn't support
(use-package man
  :config
  ;; Replace octal character ranges with decimal equivalents for BSD sed compatibility
  (setq Man-filter-list
        '(("sed" ""
           "-e '/^[\001-\037][\001-\037]*$/d'"  ;; Changed from \\o001-\\o032 to \001-\037
           "-e '/\[789]/s///g'"
           "-e '/Reformatting page.  Wait/d'"
           "-e '/Reformatting entry.  Wait/d'"
           "-e '/^[ 	]*Hewlett-Packard[ 	]Company[ 	]*-[ 	][0-9]*[ 	]-/d'"
           "-e '/^[ 	]*Hewlett-Packard[ 	]*-[ 	][0-9]*[ 	]-.*$/d'"
           "-e '/^[ 	][ 	]*-[ 	][0-9]*[ 	]-[ 	]*Formatted:.*[0-9]$/d'"
           "-e '/^[ 	]*Page[ 	][0-9]*.*(printed[ 	][0-9\\/]*)$/d'"
           "-e '/^Printed[ 	][0-9].*[0-9]$/d'"
           "-e '/^[ 	]*X[ 	]Version[ 	]1[01].*Release[ 	][0-9]/d'"
           "-e '/^[A-Za-z].*Last[ 	]change:/d'"
           "-e '/^Sun[ 	]Release[ 	][0-9].*[0-9]$/d'"
           "-e '/[ 	]*Copyright [0-9]* UNIX System Laboratories, Inc.$/d'"
           "-e '/^[ 	]*Rev\\..*Page [0-9][0-9]*$/d'")
          ("awk" "'"
           "BEGIN { blankline=0; anonblank=0; }"
           "/^$/ { if (anonblank==0) next; }"
           "{ anonblank=1; }"
           "/^$/ { blankline++; next; }"
           "{ if (blankline>0) { print \"\"; blankline=0; } print $0; }"
           "'"))))

(provide 'setup-man-mode)
;;; setup-man-mode.el ends here