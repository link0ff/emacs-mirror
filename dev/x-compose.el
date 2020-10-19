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

(defvar x-compose-keymap
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap to put on the Info node name in the mode line.")

(define-key x-compose-keymap "\u0027\u0027" "good")
(define-key x-compose-keymap "\\u0027\\u0027" "bad")
(define-key x-compose-keymap [?\u0027 ?\u0027] "good")
(define-key x-compose-keymap [?\u0027] "good")
(define-key x-compose-keymap [?\C-x ?\C-\\] "!!!")

(defun test1 ()
  (let ((keysymdef (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents "/usr/include/X11/keysymdef.h")
      (while (re-search-forward "^#define XK_\\(\\S-+\\).+/\\* U\\+\\(\\w+\\)" nil t)
        ;; (message "%S %S" (match-string 1) (match-string 2))
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

          ;; (message "%S %S %S" to-char from-keys from-chars)
          (unless (memq 0 from-chars)
            (define-key x-compose-keymap (apply 'vector from-chars) to-char)))))))

(test1)

;; C-+ ~ o runs the command "õ" (found in global-map), which is
;; a keyboard macro.
;; It is bound to C-+ ~ o.
;; Macro: M-u
;; Keyboard macro.

(define-key x-compose-keymap [?o] "õ")

(progn (define-key global-map [(control ?+)] x-compose-keymap) nil)

(provide 'x-compose)
;;; x-compose.el ends here
