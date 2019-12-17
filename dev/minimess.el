;;; minimess.el --- Use function like minibuffer-message in active minibuffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@linkov.net>
;; Keywords: minibuffer
;; Version: 0.1

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

;; When the minibuffer is active, use the function like `minibuffer-message'
;; to display the message temporarily at the end of the minibuffer.

;;; Code:

(defgroup minimess ()
  "Use the function like `minibuffer-message' in the active minibuffer."
  :group 'minibuffer)

(defcustom minimess-timeout 2
  "How long to display an echo-area message when the minibuffer is active.
If the value is a number, it should be specified in seconds.
If the value is not a number, such messages never time out."
  :type 'integer)

(defvar minimess-timer nil)
(defvar minimess-overlay nil)

(defvar minimess-properties nil
  "Text properties added to the text shown by `minimess-minibuffer-message'.")

(defun minimess-minibuffer-message (message &rest args)
  "Temporarily display MESSAGE at the end of the minibuffer.
The text is displayed for `minimess-timeout' seconds,
or until the next input event arrives, whichever comes first.
Enclose MESSAGE in [...] if this is not yet the case.
If ARGS are provided, then pass MESSAGE through `format-message'."
  (if (not (minibufferp (current-buffer)))
      (progn
        (if args
            (apply #'message message args)
          (message "%s" message))
        (prog1 (sit-for (or minimess-timeout 1000000))
          (message nil)))

    ;; Record message in the *Messages* buffer
    (let ((inhibit-message t))
      (if args
          (apply #'message message args)
        (message "%s" message)))

    (if (or (null args) (member (car args) '("" nil)))
        ;; The caller decided to clear the message
        (progn
          (when (timerp minimess-timer)
            (cancel-timer minimess-timer))
          (when (overlayp minimess-overlay)
            (delete-overlay minimess-overlay)))

      (setq message (if (and (null args)
                             (string-match-p "\\` *\\[.+\\]\\'" message))
                        ;; Make sure we can put-text-property.
                        (copy-sequence message)
                      (concat " [" message "]")))
      (when args (setq message (apply #'format-message message args)))
      (unless (or (null minimess-properties)
                  ;; Don't overwrite the face properties the caller has set
                  (text-properties-at 0 message))
        (setq message (apply #'propertize message minimess-properties)))

      (when (timerp minimess-timer)
        (cancel-timer minimess-timer))
      (when (overlayp minimess-overlay)
        (delete-overlay minimess-overlay))

      (setq minimess-overlay
            (make-overlay (point-max) (point-max) nil t t))
      (unless (zerop (length message))
        ;; The current C cursor code doesn't know to use the overlay's
        ;; marker's stickiness to figure out whether to place the cursor
        ;; before or after the string, so let's spoon-feed it the pos.
        (put-text-property 0 1 'cursor t message))
      (overlay-put minimess-overlay 'after-string message)

      (when (numberp minimess-timeout)
        (setq minimess-timer
              (run-with-timer minimess-timeout nil
                              (lambda () (when (overlayp minimess-overlay)
                                           (delete-overlay minimess-overlay))))))

      (when (and (stringp debug-on-message)
                 (stringp message)
                 (string-match-p debug-on-message message))
        (debug message)))))

(defun minimess-message (orig-fun format-string &rest args)
  (if (and
       ;; When `inhibit-message' is non-nil, the intention was to just
       ;; log the message to the *Messages* buffer using `message'.
       (null inhibit-message)
       (window-live-p (active-minibuffer-window))
       (window-live-p (old-selected-window))
       (bufferp (window-buffer (old-selected-window)))
       (minibufferp (window-buffer (old-selected-window))))
      (with-current-buffer (window-buffer (old-selected-window))
        (apply #'minimess-minibuffer-message format-string args))
    (apply orig-fun format-string args)))

(defun minimess-unload-function ()
  "Get free of advices."
  (advice-remove 'message #'minimess-message)
  nil)

(advice-add 'message :around #'minimess-message)

(provide 'minimess)
;;; minimess.el ends here
