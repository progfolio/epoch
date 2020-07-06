;;; epoch.el --- Epoch: freeze time
;; Copyright (C) 2020 Nicholas Vollmer

;; Author: Nicholas Vollmer <progfolio@protonmail.com>
;; URL: https://github.com/progfolio/epoch
;; Created: December 10, 2019
;; Keywords: org, convenience
;; Package-Requires: ((emacs "24.4"))
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


(defvar epoch-current-time nil
  "Stores the frozen time.")

(defun epoch-current-time-advice (advised &rest _args)
  "Advise `current-time' to return `epoch-current-time'.
Argument ADVISED is `current-time'."
  (or epoch-current-time (funcall advised)))

(defun epoch-advice-enabled-p ()
  "Return t if `epoch-current-time-advie' enabled, nil otherwise."
  (and (advice-member-p #'epoch-current-time-advice 'current-time) t))

(defun epoch-advice (&optional arg)
  "If ARG is non-nil turn on freeze time. Otherwise turn it off."
  (if arg
      (advice-add #'current-time :around #'epoch-current-time-advice)
    (advice-remove #'current-time #'epoch-current-time-advice)))

;;;###autoload
(defun epoch-set-time (&optional time)
  "Set `epoch-current-time' to TIME.
If TIME is nil prompt for time."
  (interactive)
  (setq epoch-current-time
        (or time
            (org-read-date t t nil "Epoch time: "))))

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
;;KEEP ABOVE

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
  (let* ((p (point))
         (default-time (or (org-get-scheduled-time p)
                           (org-get-deadline-time  p)))
         ;; (effort (org-entry-get p "Effort"))
         ;;@Incomplete:
         ;;if Effort prop, add that to hours and use that string
         (default-string
           (format-time-string "%Y-%m-%d %H:%M" default-time)))
    (epoch-with-time
      (org-read-date 'with-time 'to-time
                     nil "Epoch todo @: "
                     default-time default-string)
      (org-todo arg))))

(provide 'epoch)

;;; epoch.el ends here
