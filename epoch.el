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
(require 'org) ;;should be factored out.

(defgroup epoch nil
  "EPOCH: freeze Emacs current time"
  :group 'epoch
  :prefix "epoch-")

(declare-function org-agenda-todo  "org-agenda.el")
(declare-function org-agenda-error "org-agenda.el")

(defvar epoch-last-time nil
  "Internal variable to pass epoch-current-time to other functions.")
(defvar epoch-current-time nil
  "Stores the frozen time.")

(defun epoch-current-time-advice (advised &rest _args)
  "Advise `current-time' to return `epoch-current-time'.
Argument ADVISED is `current-time'."
  (or epoch-current-time (funcall advised)))

(defun epoch-float-time-advice (float-time &rest _args)
  "Advise `FLOAT-TIME' to return `epoch-current-time'.
Argument ADVISED is `current-time'."
  (funcall float-time epoch-current-time))

(defun epoch-advice-enabled-p ()
  "Return t if `epoch-current-time-advice' enabled, nil otherwise."
  (and (advice-member-p #'epoch-current-time-advice 'current-time) t))

(defun epoch-advice (&optional arg)
  "If ARG is non-nil turn on freeze time. Otherwise turn it off."
  (if arg
      (progn
        (advice-add #'current-time :around #'epoch-current-time-advice)
        (advice-add #'float-time :around #'epoch-float-time-advice))
    (advice-remove #'current-time #'epoch-current-time-advice)
    (advice-remove #'float-time   #'epoch-float-time-advice)))

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
  :group 'epoch
  :lighter " ⏱"
  :keymap (make-sparse-keymap)
  :global t
  (if global-epoch-mode
      (epoch-advice t)
    (epoch-advice nil)))

(defun epoch--last-repeat (org-entry-put &rest args)
  "Filter `ORG-ENTRY-PUT' ARGS so we're using `epoch-last-time'."
  (epoch-with-time epoch-last-time
    (let ((time (list (format-time-string (org-time-stamp-format t t)
                                          epoch-last-time)))
          (args (butlast args)))
      (apply org-entry-put (append args time))))
  (advice-remove 'org-entry-put 'epoch--last-repeat))

;;;###autoload
(defun epoch-todo (&optional arg)
  "Call `org-todo' with `epoch-freeze-time' set to.
Optional argument ARG is passed to `org-todo'."
  (interactive "P")
  ;;@Incomplete:
  ;;if Effort prop, add that to hours and use that string
  (let* ((timestamp (epoch--timestamp-time)))
    (epoch-with-time (org-read-date 'with-time 'to-time nil "Epoch todo @: "
                                    timestamp)
      (when org-log-repeat (setq epoch-last-time epoch-current-time)
            (advice-add 'org-entry-put :around 'epoch--last-repeat))
      (org-todo arg))))

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
