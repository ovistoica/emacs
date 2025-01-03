;;; water-reminder.el --- Remind yourself to drink water  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ovi Stoica

;; Author: Ovi Stoica <ovidiu.stoica1094@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: health, reminder
;; URL: https://github.com/yourusername/water-reminder

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a simple water drinking reminder that plays a
;; sound and shows a message in the echo area every 30 minutes (by default).
;; The reminder interval and message can be customized.

;; To use this package, simply enable the global minor mode:
;;
;;     M-x water-reminder-mode
;;
;; To customize settings:
;;
;;     M-x customize-group RET water-reminder RET

;;; Code:

(require 'cl-lib)

(defgroup water-reminder nil
  "Reminder to drink water regularly."
  :group 'health
  :prefix "water-reminder-")

(defcustom water-reminder-interval 30
  "Number of minutes between water drinking reminders."
  :type 'integer
  :group 'water-reminder)

(defcustom water-reminder-message "Time to drink some water! 💧"
  "Message to display when reminding to drink water."
  :type 'string
  :group 'water-reminder)

(defcustom water-reminder-sound t
  "Whether to play a sound with the reminder."
  :type 'boolean
  :group 'water-reminder)

(defvar water-reminder-timer nil
  "Timer object for water reminder.")

(defun water-reminder-notify ()
  "Show water reminder notification and play sound."
  (when water-reminder-sound
    (condition-case err
        (progn
          (when (fboundp 'play-sound-file)
            (ding)))
      (error
       (message "Water reminder: Could not play sound: %s" (error-message-string err)))))
  (message "%s" water-reminder-message))

;;;###autoload
(define-minor-mode water-reminder-mode
  "Toggle water reminder mode.
When enabled, reminds you to drink water at regular intervals."
  :global t
  :lighter " 💧"
  (if water-reminder-mode
      (water-reminder-start)
    (water-reminder-stop)))

(defun water-reminder-start ()
  "Start water reminder timer."
  (water-reminder-stop)
  (setq water-reminder-timer
        (run-at-time nil
                     (* water-reminder-interval 60)
                     #'water-reminder-notify)))

(defun water-reminder-stop ()
  "Stop water reminder timer."
  (when water-reminder-timer
    (cancel-timer water-reminder-timer)
    (setq water-reminder-timer nil)))

;;;###autoload
(defun water-reminder-restart ()
  "Restart water reminder timer with current settings."
  (interactive)
  (when water-reminder-mode
    (water-reminder-start)))

(provide 'water-reminder)
;;; water-reminder.el ends here
