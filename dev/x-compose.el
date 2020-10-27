;;; x-compose.el --- Compose input method from X11   -*- lexical-binding: t; -*-

;; https://cgit.freedesktop.org/xorg/proto/x11proto/plain/keysymdef.h
;; https://cgit.freedesktop.org/xorg/lib/libX11/plain/nls/en_US.UTF-8/Compose.pre
;; https://help.ubuntu.com/community/ComposeKey

;;; Code:

(defun generate-x-compose-keymap ()
  (interactive)
  (let ((buffer (current-buffer))
        (keysymdef (make-hash-table :test 'equal)))
    ;; (dolist (i '(("KP_Space" . kp-space) ("KP_Add" . kp-add) ("KP_Divide" . kp-divide) ("KP_Equal" . kp-equal)
    ;;              ("KP_0" . kp-0) ("KP_1" . kp-1) ("KP_2" . kp-2) ("KP_3" . kp-3) ("KP_4" . kp-4)
    ;;              ("KP_5" . kp-5) ("KP_6" . kp-6) ("KP_7" . kp-7) ("KP_8" . kp-8) ("KP_9" . kp-9)))
    ;;   (puthash (car i) (cdr i) keysymdef))
    ;; (dolist (i '(("KP_Space" . ?\s) ("KP_Add" . ?+) ("KP_Divide" . ?/) ("KP_Equal" . ?=)
    ;;              ("KP_0" . ?0) ("KP_1" . ?1) ("KP_2" . ?2) ("KP_3" . ?3) ("KP_4" . ?4)
    ;;              ("KP_5" . ?5) ("KP_6" . ?6) ("KP_7" . ?7) ("KP_8" . ?8) ("KP_9" . ?9)))
    ;;   (puthash (car i) (cdr i) keysymdef))
    ;; since kp-keys duplicate numeric keys: M-x flush-lines RET <KP_ RET
    (with-temp-buffer
      (insert-file-contents "/usr/include/X11/keysymdef.h")
      (while (re-search-forward "^#define XK_\\(\\S-+\\).+/\\*[ (]U\\+\\([[:xdigit:]]+\\)" nil t)
        (puthash (match-string 1) (string-to-number (match-string 2) 16) keysymdef)))
    (with-temp-buffer
      (insert-file-contents "/usr/share/X11/locale/en_US.UTF-8/Compose")
      (while (re-search-forward "^<Multi_key> \\(.*\\)" nil t)
        (let ((line (match-string 1)))
          (if (string-match "^\\([^:]+\\): \"\\([^\"]+\\)\"" line)
              (let* ((to-chars (match-string 2 line))
                     (from-keys (match-string 1 line))
                     (from-chars
                      (mapcar (lambda (s)
                                (if (string-match "^U\\([[:xdigit:]]\\{4,5\\}\\)$" s)
                                    (string-to-number (match-string 1 s) 16)
                                  (gethash s keysymdef -1)))
                              (split-string from-keys "[<> \t]+" t))))
                (if (memq -1 from-chars)
                    (princ (format ";; %s\n" line) buffer)
                  (princ (format " (%S " (apply 'string from-chars)) buffer)
                  (if (= (length to-chars) 1)
                      (let ((integer-output-format t))
                        (princ (format "%S" (string-to-char to-chars)) buffer))
                    (prin1 (vector to-chars) buffer))
                  (princ ")\n" buffer)))
            (princ (format ";; %s\n" line) buffer)))))))

;; (define-key key-translation-map [(control ?+)] x-compose-keymap)
;; (global-set-key [(control ?+) (control ?+)] 'insert-char)

(provide 'x-compose)
;;; x-compose.el ends here
