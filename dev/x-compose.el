;;; x-compose.el --- Compose input method from X11   -*- lexical-binding: t; -*-

;; https://cgit.freedesktop.org/xorg/proto/x11proto/plain/keysymdef.h

;;; Code:

(defvar x-compose-keymap
  (let ((map (make-sparse-keymap)))
    (let ((keysymdef (make-hash-table :test 'equal)))
      (with-temp-buffer
        (insert-file-contents "/usr/include/X11/keysymdef.h")
        (while (re-search-forward "^#define XK_\\(\\S-+\\).+/\\* U\\+\\(\\w+\\)" nil t)
          (puthash (match-string 1) (match-string 2) keysymdef)))
      (with-temp-buffer
        (insert-file-contents "/usr/share/X11/locale/en_US.UTF-8/Compose")
        (while (re-search-forward "^<Multi_key> \\([^:]+\\): \"\\([^\"]+\\)\"" nil t)
          (let* ((to-char (match-string 2))
                 (from-keys (match-string 1))
                 (from-chars
                  (mapcar (lambda (s)
                            (if (string-match "^U\\([[:xdigit:]]+\\)" s)
                                (string-to-number (match-string 1 s) 16)
                              (string-to-number (gethash s keysymdef "0000") 16)))
                          (split-string from-keys "[<> \t]+" t))))
            (unless (memq 0 from-chars)
              (define-key map (apply 'vector from-chars)
                (lambda ()
                  (interactive)
                  (insert to-char))))))))
    map)
  "Keymap for keys of the Compose input method.")

(define-key global-map [(control ?+)] x-compose-keymap)

(provide 'x-compose)
;;; x-compose.el ends here
