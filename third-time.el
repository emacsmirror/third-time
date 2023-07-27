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


;;; Variables

;;;; State Transitions
;; nil -> :working
;; :working -> {:long-break, :break, nil }
;; {:long-break, :break} -> { nil, :working }

(defvar third-time-state nil)

(defvar third-time-worked-total 0)

(defvar third-time-just-worked 0)

(defvar third-time-break-available 0)

(defvar third-time-change-time 0)


;;; Helper Functions



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



;;; Primary User Functions



;;; Global Minor Mode


(provide 'third-time)

;;; third-time.el ends here
