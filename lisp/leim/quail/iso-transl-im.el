;;; iso-transl-im.el --- Quail package based on `C-x 8' key sequences -*-coding: utf-8;-*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@linkov.net>
;; Keywords: multilingual, input method, i18n

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

;;; Commentary:

;; This input method supports the same key sequences as defined by the
;; `C-x 8' keymap in iso-transl.el.

;;; Code:

(require 'quail)

(quail-define-package
 "iso-transl" "UTF-8" "X8" t
 "Use the same key sequences as in `C-x 8' keymap defined in iso-transl.el.
Examples:
 * E -> €   1 / 2 -> ½   ^ 3 -> ³"
 '(("\t" . quail-completion)
   ("\n" . insert-char))
 t nil nil nil nil nil nil nil nil t)

(eval-when-compile
  (require 'iso-transl)
  (defmacro iso-transl-im--define-rules ()
    `(quail-define-rules
      ,@(mapcar (lambda (rule)
                  (let ((from (car rule))
                        (to (cdr rule)))
                    (list from (if (stringp to)
                                   (vector to) to))))
                iso-transl-char-map))))

(iso-transl-im--define-rules)

(provide 'iso-transl-im)
;;; iso-transl-im.el ends here
