;;; whitespace-tests.el --- Test suite for whitespace -*- lexical-binding: t -*-

;; Copyright (C) 2016-2022 Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'faceup)
(require 'whitespace)

(defmacro whitespace-tests--with-test-buffer (style &rest body)
  "Run BODY in a buffer with `whitespace-mode' style STYLE.
The buffer is displayed in `selected-window', and
`noninteractive' is set to nil even in batch mode."
  (declare (debug ((style form) def-body))
           (indent 1))
  `(ert-with-test-buffer-selected ()
     ;; In case global-*-mode is enabled.
     (whitespace-mode -1)
     (font-lock-mode -1)
     (let ((noninteractive nil)
           (whitespace-style ,style))
       (font-lock-mode 1)
       (whitespace-mode 1)
       ,@body)))

(defun whitespace-tests--faceup (&rest lines)
  "Convenience wrapper around `faceup-test-font-lock-buffer'.
Returns non-nil if the concatenated LINES match the current
buffer's content."
  (faceup-test-font-lock-buffer nil (apply #'concat lines)))
(let ((x (get 'faceup-test-font-lock-buffer 'ert-explainer)))
  (put 'whitespace-tests--faceup 'ert-explainer
       (lambda (&rest lines) (funcall x nil (apply #'concat lines)))))

(defun whitespace-tests--cleanup-string (string)
  (with-temp-buffer
    (insert string)
    (whitespace-cleanup)
    (buffer-string)))

(ert-deftest whitespace-cleanup-eob ()
  (let ((whitespace-style '(empty)))
    (should (equal (whitespace-tests--cleanup-string "a\n")
                   "a\n"))
    (should (equal (whitespace-tests--cleanup-string "a\n\n")
                   "a\n"))
    (should (equal (whitespace-tests--cleanup-string "a\n\t\n")
                   "a\n"))
    (should (equal (whitespace-tests--cleanup-string "a\n\t \n")
                   "a\n"))
    (should (equal (whitespace-tests--cleanup-string "a\n\t \n\n")
                   "a\n"))
    (should (equal (whitespace-tests--cleanup-string "\n\t\n")
                   ""))
    ;; Whitespace at end of non-empty line is not covered by the
    ;; `empty' style.
    (should (equal (whitespace-tests--cleanup-string "a  \n\t \n\n")
                   "a  \n"))))


;; We cannot call whitespace-mode because it will do nothing in batch
;; mode.  So we call its innards instead.
(defun whitespace-tests-whitespace-mode-on ()
  "Turn `whitespace-mode' on even in batch mode."
  (setq whitespace-mode t)
  (whitespace-turn-on)
  (whitespace-action-when-on))

(ert-deftest whitespace-tests-display-tables ()
  "Test whitespace stores and restores the buffer display table - bug26892."
  (with-temp-buffer
    (whitespace-mode -1) ; turn off in case global ws mode is active
    (let ((whitespace-style '(space-mark tab-mark newline-mark))
          (whitespace-display-mappings '((space-mark   32 [183] [46])
                                         (space-mark  160 [164] [95])
                                         (newline-mark 10 [36 10])
                                         (tab-mark      9 [187 9] [92 9])))
          (buffer-display-table nil))
      ;test the display table actually changes
      (should-not (equal nil
                         (progn (whitespace-tests-whitespace-mode-on)
                                buffer-display-table)))
      ;test the display table restores correctly
      (should (equal nil
                     (progn (whitespace-turn-off)
                            buffer-display-table)))
      ;test the stored display table is preserved
      (should (equal nil
                     (progn (whitespace-tests-whitespace-mode-on)
                            (whitespace-turn-off)
                            buffer-display-table))))))

(ert-deftest whitespace-tests--empty-bob ()
  (whitespace-tests--with-test-buffer '(face empty)
    (electric-indent-mode -1)

    ;; Insert some empty lines.  None of the lines should be
    ;; highlighted even though point is on the last line because the
    ;; entire buffer is empty lines.
    (execute-kbd-macro (kbd "SPC RET C-q TAB RET RET SPC"))
    (should (equal (buffer-string) " \n\t\n\n "))
    (should (equal (line-number-at-pos) 4))
    (should (whitespace-tests--faceup " \n"
                                      "\t\n"
                                      "\n"
                                      " "))

    ;; Adding content on the last line (and keeping point there)
    ;; should cause the previous lines to be highlighted.  Note that
    ;; the `whitespace-empty' face applies to the newline just before
    ;; the last line, which has the desired property of extending the
    ;; highlight the full width of the window.
    (execute-kbd-macro (kbd "x"))
    (should (equal (buffer-string) " \n\t\n\n x"))
    (should (equal (line-number-at-pos) 4))
    (should (whitespace-tests--faceup "«:whitespace-empty: \n"
                                      "\t\n"
                                      "\n"
                                      "» x"))

    ;; Lines should become un-highlighted as point moves up into the
    ;; empty lines.
    (execute-kbd-macro (kbd "<up>"))
    (should (equal (line-number-at-pos) 3))
    (should (whitespace-tests--faceup "«:whitespace-empty: \n"
                                      "\t\n"
                                      "»\n"
                                      " x"))
    (execute-kbd-macro (kbd "<up>"))
    (should (equal (line-number-at-pos) 2))
    (should (whitespace-tests--faceup "«:whitespace-empty: \n"
                                      "»\t\n"
                                      "\n"
                                      " x"))
    (execute-kbd-macro (kbd "<up> <home>"))
    (should (equal (point) 1))
    (should (whitespace-tests--faceup " \n"
                                      "\t\n"
                                      "\n"
                                      " x"))

    ;; Line 1 should be un-highlighted when point is in line 1 even if
    ;; point is not bobp.
    (execute-kbd-macro (kbd "<right>"))
    (should (equal (line-number-at-pos) 1))
    (should (> (point) 1))
    (should (whitespace-tests--faceup " \n"
                                      "\t\n"
                                      "\n"
                                      " x"))

    ;; Make sure lines become re-highlighted as point moves down.
    (execute-kbd-macro (kbd "<down>"))
    (should (equal (line-number-at-pos) 2))
    (should (whitespace-tests--faceup "«:whitespace-empty: \n"
                                      "»\t\n"
                                      "\n"
                                      " x"))
    (execute-kbd-macro (kbd "<down>"))
    (should (equal (line-number-at-pos) 3))
    (should (whitespace-tests--faceup "«:whitespace-empty: \n"
                                      "\t\n"
                                      "»\n"
                                      " x"))
    (execute-kbd-macro (kbd "<down>"))
    (should (equal (line-number-at-pos) 4))
    (should (whitespace-tests--faceup "«:whitespace-empty: \n"
                                      "\t\n"
                                      "\n"
                                      "» x"))

    ;; Inserting content on line 2 should un-highlight lines 2 and 3.
    (execute-kbd-macro (kbd "<up> <up> <end>"))
    (should (equal (line-number-at-pos) 2))
    (should (equal (- (point) (line-beginning-position)) 1))
    (execute-kbd-macro (kbd "y <down> <down>"))
    (should (equal (line-number-at-pos) 4))
    (should (whitespace-tests--faceup "«:whitespace-empty: \n"
                                      "»\ty\n"
                                      "\n"
                                      " x"))

    ;; Removing the content on line 2 should re-highlight lines 2 and
    ;; 3.
    (execute-kbd-macro (kbd "<up> <up> <end>"))
    (should (equal (line-number-at-pos) 2))
    (should (equal (- (point) (line-beginning-position)) 2))
    (execute-kbd-macro (kbd "DEL <down> <down>"))
    (should (equal (line-number-at-pos) 4))
    (should (whitespace-tests--faceup "«:whitespace-empty: \n"
                                      "\t\n"
                                      "\n"
                                      "» x"))))

(ert-deftest whitespace-tests--empty-eob ()
  (whitespace-tests--with-test-buffer '(face empty)
    (electric-indent-mode -1)

    ;; Insert some empty lines.  None of the lines should be
    ;; highlighted even though point is on line 1 because the entire
    ;; buffer is empty lines.
    (execute-kbd-macro (kbd "RET RET C-q TAB RET SPC C-<home>"))
    (should (equal (buffer-string) "\n\n\t\n "))
    (should (equal (line-number-at-pos) 1))
    (should (whitespace-tests--faceup "\n"
                                      "\n"
                                      "\t\n"
                                      " "))

    ;; Adding content on the first line (and keeping point there)
    ;; should cause the subsequent lines to be highlighted.
    (execute-kbd-macro (kbd "x"))
    (should (equal (buffer-string) "x\n\n\t\n "))
    (should (equal (line-number-at-pos) 1))
    (should (whitespace-tests--faceup "x\n"
                                      "«:whitespace-empty:\n"
                                      "\t\n"
                                      " »"))

    ;; Lines should become un-highlighted as point moves down into the
    ;; empty lines.
    (execute-kbd-macro (kbd "<down>"))
    (should (equal (line-number-at-pos) 2))
    (should (whitespace-tests--faceup "x\n"
                                      "\n"
                                      "«:whitespace-empty:\t\n"
                                      " »"))
    (execute-kbd-macro (kbd "<down>"))
    (should (equal (line-number-at-pos) 3))
    (should (whitespace-tests--faceup "x\n"
                                      "\n"
                                      "\t\n"
                                      "«:whitespace-empty: »"))
    (execute-kbd-macro (kbd "C-<end>"))
    (should (equal (line-number-at-pos) 4))
    (should (eobp))
    (should (equal (- (point) (line-beginning-position)) 1))
    (should (whitespace-tests--faceup "x\n"
                                      "\n"
                                      "\t\n"
                                      " "))

    ;; The last line should be un-highlighted when point is in that
    ;; line even if point is not eobp.
    (execute-kbd-macro (kbd "<left>"))
    (should (equal (line-number-at-pos) 4))
    (should (not (eobp)))
    (should (whitespace-tests--faceup "x\n"
                                      "\n"
                                      "\t\n"
                                      " "))

    ;; Make sure lines become re-highlighted as point moves up.
    (execute-kbd-macro (kbd "<up>"))
    (should (equal (line-number-at-pos) 3))
    (should (whitespace-tests--faceup "x\n"
                                      "\n"
                                      "\t\n"
                                      "«:whitespace-empty: »"))
    (execute-kbd-macro (kbd "<up>"))
    (should (equal (line-number-at-pos) 2))
    (should (whitespace-tests--faceup "x\n"
                                      "\n"
                                      "«:whitespace-empty:\t\n"
                                      " »"))
    (execute-kbd-macro (kbd "<up>"))
    (should (equal (line-number-at-pos) 1))
    (should (whitespace-tests--faceup "x\n"
                                      "«:whitespace-empty:\n"
                                      "\t\n"
                                      " »"))

    ;; Inserting content on line 3 should un-highlight lines 2 and 3.
    (execute-kbd-macro (kbd "<down> <down> <home>"))
    (should (equal (line-number-at-pos) 3))
    (should (equal (- (point) (line-beginning-position)) 0))
    (execute-kbd-macro (kbd "y <up> <up>"))
    (should (equal (line-number-at-pos) 1))
    (should (whitespace-tests--faceup "x\n"
                                      "\n"
                                      "y\t\n"
                                      "«:whitespace-empty: »"))

    ;; Removing the content on line 3 should re-highlight lines 2 and
    ;; 3.
    (execute-kbd-macro (kbd "<down> <down> <home>"))
    (should (equal (line-number-at-pos) 3))
    (should (equal (- (point) (line-beginning-position)) 0))
    (execute-kbd-macro (kbd "<deletechar> <up> <up>"))
    (should (equal (line-number-at-pos) 1))
    (should (whitespace-tests--faceup "x\n"
                                      "«:whitespace-empty:\n"
                                      "\t\n"
                                      " »"))))

(provide 'whitespace-tests)

;;; whitespace-tests.el ends here
