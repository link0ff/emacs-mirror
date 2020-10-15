;;; x-compose.el --- Compose                         -*- lexical-binding: t; -*-

;; https://cgit.freedesktop.org/xorg/proto/x11proto/plain/keysymdef.h

;;; Code:

(setq keysymdef (make-hash-table :test 'equal))
(with-temp-buffer
  (insert-file-contents "/usr/include/X11/keysymdef.h")
  (while (re-search-forward "^#define XK_\\(\\S-+\\).+/\\* U\\+\\(\\w+\\)" nil t)
    (puthash (match-string 1) (string-to-number (match-string 2) 16) keysymdef)))
(maphash (lambda (k v) (list k v)) keysymdef)
(gethash "apostrophe" keysymdef ?\0)
;; ?\x39
;; ?\u39

(define-key my-map "\u0027\u0027" "good")
(define-key my-map "\\u0027\\u0027" "bad")

(defvar x-compose-keymap
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap to put on the Info node name in the mode line.")

(defun test1 ()
  (let ((keysymdef (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents "/usr/include/X11/keysymdef.h")
      (while (re-search-forward "^#define XK_\\(\\S-+\\).+/\\* U\\+\\(\\w+\\)" nil t)
        (puthash (match-string 1) (match-string 2) keysymdef)))
    (with-temp-buffer
      (insert-file-contents "/usr/share/X11/locale/en_US.UTF-8/Compose")
      (while (re-search-forward "^<Multi_key> \\([^:]+\\): \"\\([^\"]+\\)\"" nil t)
        (let* ((char (match-string 2))
               (keys (match-string 1))
               (keys (replace-regexp-in-string
		      "\\s-*<\\([^>]+\\)>\\s-*"
		      (lambda (s) (concat "\\" "u" (gethash (match-string 1 keys) keysymdef "0000")))
                      keys nil t)))
          (ignore-errors
            (define-key x-compose-keymap keys char)))))))

(test1)

(define-key global-map [(control ?+)] x-compose-keymap)

(provide 'x-compose)
;;; x-compose.el ends here
