;;; third-time.el --- Third Time support for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samuel W. Flint <swflint@flintfam.org>

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://git.sr.ht/~swflint/busylight
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; TODO

;;; Code:

(require 'cl-lib)


;;; Customization

(defgroup third-time nil
  "Customize behavior of Third Time for Emacs."
  :group 'convenience
  :prefix "third-time-"
  :link '(url-link :tag "Sourcehut" "https://git.sr.ht/~swflint/third-time")
  :link '(emacs-library-link :tag "Library Source" "third-time.el"))

(defcustom third-time-fraction 3
  "What proportion of time should be used for breaks.

This is interpreted as 1 / `third-time-fraction', thus it should
be a positive integer."
  :group 'third-time
  :type 'natnum)

(defcustom third-time-working-hook nil
  "Hook to run when work begins."
  :group 'third-time
  :type 'hook)

(defcustom third-time-break-hook nil
  "Hook to run when a break begins."
  :group 'third-time
  :type 'hook)

(defcustom third-time-long-break-hook nil
  "Hook to run when a long break begins."
  :group 'third-time
  :type 'hook)

(defcustom third-time-change-hook nil
  "Functions to run on state change.

This hook will be run after the more specific state change
hooks (`third-time-working-hook', `third-time-break-hook',
`third-time-long-break-hook' and `third-time-mode-hook')"
  :group 'third-time
  :type 'hook)

(defcustom third-time-mode-hook nil
  "Functions to run on enable/disable of `third-time-mode'."
  :group 'third-time
  :type 'hook)

(defcustom third-time-log-file nil
  "File to log third time data to."
  :group 'third-time
  :type '(choice (file :tag "Log to file")
                 (const :tag "Don't log" nil)))

(defcustom third-time-log-format "%T,%s"
  "Log line format for `third-time-log-file'.

The following format codes are available:

 - %T formats time according to `third-time-log-time-format'
 - %s state entered
 - %h time worked (HH:MM)
 - %b break remaining at state change (HH:MM)"
  :group 'third-time
  :type 'string)

(defcustom third-time-log-time-format "%Y,%m,%d,%H,%M"
  "Logging time format.

Formatted using `format-time-string'."
  :group 'third-time
  :type 'string)

(defcustom third-time-alert-function #'third-time-alerter
  "How should users be alerted of break ends?

This should be a function which takes one argument, a MESSAGE."
  :group 'third-time
  :type 'function)

(defcustom third-time-nag-time 3
  "How often should the user be nagged?

When 0 no nagging will be completed.  Otherwise, this number is
interpreted as a number of minutes."
  :group 'third-time
  :type 'natnum)


;;; Variables

(defvar third-time-state nil
  "Current third time state.

This should be one of nil, :working, :break, or :long-break.

The following state transitions are assumed:

nil -> :working
:working -> {:long-break, :break, nil }
{:long-break, :break} -> { nil, :working }")

(defvar third-time-worked-total 0
  "Total time worked in the current session as a number of seconds.")

(defvar third-time-just-worked 0
  "Time worked in the most recent work session as number of seconds.")

(defvar third-time-break-timer nil
  "Timer for breaks.")

(defvar third-time-break-available 0
  "How many seconds of break are available.")

(defvar third-time-change-time 0
  "When was the last state change recorded.")

(defvar third-time-log-buffer nil
  "Buffer to log state changes to.")

(defvar third-time-nag-timer nil
  "Timer for nagging that a break is done.")


;;; Helper Functions

(defun third-time-reset-state ()
  "Reset Third Time state variables."
  (setf third-time-state nil
        third-time-worked-total 0
        third-time-just-worked 0
        third-time-break-available 0
        third-time-log-buffer nil)
  (third-time-cancel-nagger))


;;; Alerting and Nagging

(defun third-time-alert (message)
  "Show the user MESSAGE with `third-time-alert-function'."
  (funcall third-time-alert-function message))

(defun third-time-alerter (message)
  "Show Third Time alert MESSAGE."
  (message "%s\n%s"
           (propertize "Third Time Alert" 'face 'font-lock-error-face)
           message))

(defun third-time-cancel-nagger ()
  "Cancel the nag timer."
  (when (timerp third-time-nag-timer)
    (cancel-timer third-time-nag-timer)
    (setf third-time-nag-timer nil)))

(defun third-time-start-nagger ()
  "Start nagging users that their break has finished."
  (unless (= third-time-nag-time 0)
    (third-time-cancel-nagger)
    (setf third-time-nag-timer (run-with-timer (* 60 third-time-nag-time) t #'third-time-nag))))

(defun third-time-nag ()
  "Nag the user that their break is complete."
  (if (or (eq third-time-state :break)
          (eq third-time-state :long-break))
      (third-time-alert "Your break has finished.  Time to return to work.")
    (third-time-cancel-nagger)))


;;; Break timers
(defun third-time-start-break-timer (secs message)
  "Start a break timer for SECS, showing MESSAGE after break."
  (when (timerp third-time-break-timer)
    (cancel-timer third-time-break-timer)
    (setf third-time-break-timer nil))
  (setf third-time-break-timer (run-with-timer secs nil #'third-time-break-function message)))

(defun third-time-break-function (message)
  "Show MESSAGE that break is done and start nagger."
  (third-time-alert message)
  (third-time-start-nagger))


;;; Logging support

(defun third-time-log-format-line (state worked remaining)
  "Format a log time according to STATE, WORKED and REMAINING.

This uses `third-time-log-format' and `third-time-log-time-format'."
  (let ((time (format-time-string third-time-log-time-format))
        (state-string (cdr (assoc state '((:working . "WORKING")
                                          (:break . "BREAK")
                                          (:long-break . "LONGBREAK")
                                          (nil . "OFF")))))
        (hours-worked (format "%02d:%02d"
                              (floor (/ worked 60 60))
                              (floor (/ worked 60))))
        (break-remaining (format "%02d:%02d"
                                 (floor (/ remaining 60 60))
                                 (floor (/ remaining 60)))))
    (format-spec third-time-log-format `((?T . ,time)
                                         (?s . ,state-string)
                                         (?h . ,hours-worked)
                                         (?b . ,break-remaining)))))

(defun third-time-log ()
  "Log the most recent state change."
  (when third-time-log-file
    (unless third-time-log-buffer
      (setf third-time-log-buffer (find-file-literally third-time-log-file)))
    (with-current-buffer third-time-log-buffer
      (save-mark-and-excursion
        (goto-char (point-max))
        (insert (third-time-log-format-line third-time-state
                                            third-time-worked-total
                                            third-time-break-available)
                "\n")
        (save-buffer)))))


;;; Primary User Functions

;;;###autoload
(defun third-time-start-break ()
  "Start a break."
  (interactive)
  (if (not (eq third-time-state :working))
      (user-error "You cannot take a break if you are not currently working")
    (setf third-time-state :break)
    (force-mode-line-update)
    (message "TODO")))

;;;###autoload
(defun third-time-start-long-break ()
  "Start a long break."
  (interactive)
  (if (not (eq third-time-state :working))
      (user-error "You cannot take a long break if you are not currently working")
    (setf third-time-state :long-break)
    (force-mode-line-update)
    (message "TODO")))

;;;###autoload
(defun third-time-start-work ()
  "Start working."
  (interactive)
  (unless (eq third-time-state :working)
    (setf third-time-state :working)
    (force-mode-line-update)
    (message "TODO")))

;;;###autoload
(defun third-time-end-session ()
  "End the work session."
  (interactive)
  (third-time-mode -1))


;;; Global Minor Mode

(defun third-time-modeline ()
  "Format modeline entry for `third-time-mode'."
  (format " ⅓⏲[%s]"
          (cdr (assoc third-time-state
                      '((:working . "WRK")
                        (:break . "BRK")
                        (:long-break . "LNG")
                        (nil . "OFF"))))))

(defvar third-time-prefix "C-x C-S-t"
  "Prefix for `third-time-mode' bindings.

Note, must be set *before* third-time is loaded.")

(defvar third-time-mode-map
  (let ((keymap (make-keymap)))
    (mapc (lambda (binding)
            (cl-destructuring-bind (key . cmd) binding
              (define-key keymap (kbd (format "%s %s" third-time-prefix key)) cmd)))
          '(("w" . third-time-start-work)
            ("b" . third-time-start-break)
            ("l" . third-time-start-long-break)
            ("m" . third-time-start-long-break)
            ("e" . third-time-end-session)
            ("s" . third-time-stop-session)))
    keymap)
  "Keymap for `third-time-mode'.")

;;;###autoload
(define-minor-mode third-time-mode
  "Basic support for Third Time productivity."
  :global t
  :lighter (:eval (third-time-modeline))
  :keymap third-time-mode-map
  :variable third-time-state
  (if (not third-time-state)
      (third-time-reset-state)
    (third-time-start-work)))


(provide 'third-time)

;;; third-time.el ends here
