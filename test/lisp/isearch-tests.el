;;; isearch-tests.el --- Tests for isearch.el        -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2022 Free Software Foundation, Inc.

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)

(ert-deftest isearch--test-update ()
  (with-temp-buffer
    (setq isearch--current-buffer (current-buffer)))
  (with-temp-buffer
    (isearch-update)
    (should (equal isearch--current-buffer (current-buffer)))))

(ert-deftest isearch--test-done ()
  ;; Normal operation.
  (isearch-update)
  (isearch-done)
  (should-not isearch--current-buffer)
  ;; Bug #21091: let `isearch-done' work without `isearch-update'.
  (isearch-done))


;; Search functions.

(ert-deftest isearch--test-search-fun-in-text-property ()
  (let ((isearch-search-fun-function
         (lambda () (isearch-search-fun-in-text-property nil 'dired-filename)))
        (pairs '((4 . 7) (11 . 14) (21 . 24))))
    (with-temp-buffer
      (insert "foo" (propertize "foo" 'dired-filename t) "foo\n")
      (insert (propertize "foo" 'dired-filename t) "foo\n")
      (insert "foo" (propertize "foo" 'dired-filename t) "\n")

      (goto-char (point-min))
      (let ((isearch-forward t)
            (isearch-regexp nil))
        (dolist (pos (append pairs nil))
          (should (eq (cdr pos) (isearch-search-string "foo" nil t)))
          (when (car pos) (should (eq (car pos) (match-beginning 0))))))

      (goto-char (point-max))
      (let ((isearch-forward nil)
            (isearch-regexp nil))
        (dolist (pos (append (reverse pairs) nil))
          (should (eq (car pos) (isearch-search-string "foo" nil t)))
          (when (cdr pos) (should (eq (cdr pos) (match-end 0))))))

      (goto-char (point-min))
      (let ((isearch-forward t)
            (isearch-regexp t))
        (dolist (pos (append pairs nil))
          (should (eq (cdr pos) (isearch-search-string ".*" nil t)))
          (when (car pos) (should (eq (car pos) (match-beginning 0))))))

      (goto-char (point-min))
      (let ((isearch-forward t)
            (isearch-regexp t))
        (dolist (pos (append pairs nil))
          (should (eq (cdr pos) (isearch-search-string "^.*" nil t)))
          (when (car pos) (should (eq (car pos) (match-beginning 0))))))

      (goto-char (point-min))
      (let ((isearch-forward t)
            (isearch-regexp t))
        (dolist (pos (append pairs nil))
          (should (eq (cdr pos) (isearch-search-string ".*$" nil t)))
          (when (car pos) (should (eq (car pos) (match-beginning 0))))))

      (goto-char (point-max))
      (let ((isearch-forward nil)
            (isearch-regexp t))
        (dolist (pos (append (reverse pairs) nil))
          (should (eq (car pos) (isearch-search-string "^.*" nil t)))
          (when (cdr pos) (should (eq (cdr pos) (match-end 0)))))))))

;; (message "! %S %S %S" (isearch-search-string ".*$" nil t) (match-beginning 0) (match-end 0))

(provide 'isearch-tests)
;;; isearch-tests.el ends here
