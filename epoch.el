;;; epoch.el --- Epoch: freeze time
;; Copyright (C) 2020 Nicholas Vollmer

;; Author: Nicholas Vollmer <progfolio@protonmail.com>
;; URL: https://github.com/progfolio/epoch
;; Created: December 10, 2019
;; Keywords: org, convenience
;; Package-Requires: ((emacs "26.1"))
;; Version: 0.0.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;; Freeze (current-time).

;;; Code:
(require 'org) ;;could be factored out. just using for org-read-date

(declare-function org-agenda-todo  "org-agenda.el")
(declare-function org-agenda-error "org-agenda.el")

(defvar epoch-current-time nil
  "Stores the frozen time.")

(defun epoch-current-time-advice (advised &rest _args)
  "Advise `current-time' to return `epoch-current-time'.
Argument ADVISED is `current-time'."
  (or epoch-current-time (funcall advised)))

(defun epoch-advice-enabled-p ()
  "Return t if `epoch-current-time-advice' enabled, nil otherwise."
  (and (advice-member-p #'epoch-current-time-advice 'current-time) t))

(defun epoch-advice (&optional arg)
  "If ARG is non-nil turn on freeze time. Otherwise turn it off."
  (if arg
      (advice-add #'current-time :around #'epoch-current-time-advice)
    (advice-remove #'current-time #'epoch-current-time-advice)))

(defun epoch--timestamp-time ()
  "Return scheduled time or deadline time from current org entry."
  (when-let* ((p (point))
              (default-time (or (org-get-scheduled-time p)
                                (org-get-deadline-time  p)))
              (ts (org-timestamp-from-time default-time 'with-time)))
    (org-read-date 'with-time 'to-time (org-element-interpret-data ts))))

(defmacro epoch-with-time (time &rest body)
  "Execute BODY with `epoch-current-time' at TIME.
If TIME is nil, `current-time' is used."
  (declare (indent defun))
  (let ((initial-state (make-symbol "initial-state")))
    `(let ((,initial-state (epoch-advice-enabled-p))
           (epoch-current-time ,time))
       (epoch-advice t)
       (unwind-protect
           (progn ,@body)
         (epoch-advice ,initial-state)))))

;;;###autoload
(defun epoch-set-time (&optional time)
  "Set `epoch-current-time' to TIME.
If TIME is nil prompt for time."
  (interactive)
  (setq epoch-current-time
        (or time
            (org-read-date t t nil "Epoch time: "))))

;;;###autoload
(define-minor-mode global-epoch-mode
  "Freeze time."
  :lighter " ⏱"
  :keymap (make-sparse-keymap)
  :global t
  (if global-epoch-mode
      (epoch-advice t)
    (epoch-advice nil)))

;;;###autoload
(defun epoch-todo (&optional arg)
  "Call `org-todo' with `epoch-freeze-time' set to.
Optional argument ARG is passed to `org-todo'."
  (interactive "P")
  ;;@Incomplete:
  ;;if Effort prop, add that to hours and use that string
  (let* ((timestamp (epoch--timestamp-time))
         (default-string
           (format-time-string "%Y-%m-%d %H:%M" timestamp)))
    (unwind-protect
        (progn
          (global-epoch-mode)
          (epoch-set-time (org-read-date 'with-time 'to-time nil
                                         "Epoch todo @: " timestamp default-string))
          (org-todo arg))
      (global-epoch-mode -1))))

;;;###autoload
(defun epoch-agenda-todo (&optional arg)
  "Call `org-todo' with `epoch-current-time' set.
ARG is passed to `org-todo'."
  (interactive "P")
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (with-current-buffer buffer
      (widen)
      (goto-char pos)
      (org-show-context 'agenda)
      (epoch-todo))))

(provide 'epoch)

;;; epoch.el ends here
