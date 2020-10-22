;;; x-compose.el --- Compose input method from X11   -*- lexical-binding: t; -*-

;; https://cgit.freedesktop.org/xorg/proto/x11proto/plain/keysymdef.h

;;; Code:

(defun generate-x-compose-keymap ()
  (interactive)
  (let ((buffer (current-buffer))
        (keysymdef (make-hash-table :test 'equal)))
    ;; (dolist (i '(("KP_Space" . kp-space) ("KP_Add" . kp-add) ("KP_Divide" . kp-divide) ("KP_Equal" . kp-equal)
    ;;              ("KP_0" . kp-0) ("KP_1" . kp-1) ("KP_2" . kp-2) ("KP_3" . kp-3) ("KP_4" . kp-4)
    ;;              ("KP_5" . kp-5) ("KP_6" . kp-6) ("KP_7" . kp-7) ("KP_8" . kp-8) ("KP_9" . kp-9)))
    ;;   (puthash (car i) (cdr i) keysymdef))
    (with-temp-buffer
      (insert-file-contents "/usr/include/X11/keysymdef.h")
      (while (re-search-forward "^#define XK_\\(\\S-+\\).+/\\*[ (]U\\+\\([[:xdigit:]]+\\)" nil t)
        (puthash (match-string 1) (string-to-number (match-string 2) 16) keysymdef)))
    (with-temp-buffer
      (insert-file-contents "/usr/share/X11/locale/en_US.UTF-8/Compose")
      (while (re-search-forward "^<Multi_key> \\([^:]+\\): \"\\([^\"]+\\)\"" nil t)
        (let* ((to-chars (match-string 2))
               (from-keys (match-string 1))
               (from-chars
                (mapcar (lambda (s)
                          (if (string-match "^U\\([[:xdigit:]]+\\)" s)
                              (string-to-number (match-string 1 s) 16)
                            (gethash s keysymdef -1)))
                        (split-string from-keys "[<> \t]+" t))))
          (unless (memq -1 from-chars)
            (princ (format " (%S " (apply 'string from-chars)) buffer)
            (if (= (length to-chars) 1)
                (princ (format "?%c" (string-to-char to-chars)) buffer)
              (prin1 to-chars buffer))
            (princ ")\n" buffer)))))))

;; (define-key key-translation-map [(control ?+)] x-compose-keymap)
;; (global-set-key [(control ?+) (control ?+)] 'insert-char)

(provide 'x-compose)
;;; x-compose.el ends here
