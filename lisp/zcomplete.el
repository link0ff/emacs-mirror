;;; zcomplete.el --- zsh-like minibuffer completion based on icomplete -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@linkov.net>
;; Keywords: completion
;; Maintainer: emacs-devel@gnu.org

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

;; Like `icomplete' but provides feedback in the *Completions* window
;; instead of the minibuffer.

;;; Code:

(defgroup zcomplete nil
  "Show completions dynamically in *Completions* window from minibuffer."
  :prefix "zcomplete-"
  :link '(info-link "(emacs)Zcomplete")
  :group 'minibuffer)

(defcustom zcomplete-show-matches-on-no-input nil
  "When non-nil, show completions when first prompting for input.
This means to show completions even when the current minibuffer contents
is the same as was the initial input after minibuffer activation.
This also means that if you traverse the list of completions with
commands and just hit RET without typing any characters,
the match under point will be chosen instead of the default."
  :type 'boolean)

(defcustom zcomplete-with-completion-tables t
  "Specialized completion tables with which zcomplete should operate.
If this is t, zcomplete operates on all tables.
Otherwise this should be a list of the completion tables (e.g.,
`internal-complete-buffer') on which zcomplete should operate."
  :type '(choice (const :tag "All" t)
                 (repeat function)))

(defcustom zcomplete-compute-delay .15
  "Completions-computation stall, used only with large-number completions.
See `zcomplete-delay-completions-threshold'."
  :type 'number)

(defcustom zcomplete-delay-completions-threshold 400
  "Pending-completions number over which to apply `zcomplete-compute-delay'."
  :type 'integer)

(defcustom zcomplete-max-delay-chars 2
  "Maximum number of initial chars to apply `zcomplete-compute-delay'."
  :type 'integer)

(defcustom zcomplete-minibuffer-setup-hook nil
  "Zcomplete-specific customization of minibuffer setup.
This hook is run during minibuffer setup if Zcomplete is active."
  :type 'hook)


(defvar zcomplete--initial-input nil
  "Initial input in the minibuffer when `zcomplete-mode' was activated.
Used to implement the option `zcomplete-show-matches-on-no-input'.")

(defvar zcomplete--previous-input nil
  "Previous input in the minibuffer before editing it.
Used to optimize `zcomplete-exhibit' to not be fired while
moving point in the minibuffer.")

(defun zcomplete-post-command-hook ()
  (let ((non-essential t)) ;E.g. don't prompt for password!
    (unless (memq this-command '( previous-history-element next-history-element
                                  previous-line-or-history-element
                                  next-line-or-history-element))
      (zcomplete-exhibit))))

(defcustom zcomplete-auto-exhibit 'visible
  "Non-nil means to use pop up completions on minibuffer edit."
  :type '(choice (const :tag "Don't auto pop up completions" nil)
                 (const :tag "Pop up completions window" t)
                 (const :tag "Update completions window only when visible"
                        visible)))

(defcustom zcomplete-arrows 'visible
  "Non-nil means to use arrows to browse completions from the minibuffer."
  :type '(choice (const :tag "Don't use arrows" nil)
                 (const :tag "Use arrows" t)
                 (const :tag "Use arrows when completions window is visible"
                        visible)))

(defun zcomplete-visible ()
  (get-buffer-window "*Completions*" 0))

(defun zcomplete-bind-arrows (binding &optional horizontal)
  `(menu-item
    "" ,binding
    :filter ,(lambda (cmd)
               (when (or (eq zcomplete-arrows t)
                         (and (eq zcomplete-arrows 'visible)
                              (zcomplete-visible)
                              (or (not horizontal)
                                  (eq completions-format 'one-column))))
                 cmd))))

(defun zcomplete-bind-visible (binding)
  `(menu-item
    "" ,binding
    :filter ,(lambda (cmd)
               (when (zcomplete-visible)
                 cmd))))

(defvar-keymap zcomplete-minibuffer-mode-map ;; zcomplete-minibuffer-map
  :doc "Keymap used by `zcomplete-mode' in the minibuffer."

  "<remap> <minibuffer-complete-and-exit>" (zcomplete-bind-visible #'zcomplete-ret)
  "<remap> <minibuffer-keyboard-quit>"     (zcomplete-bind-visible #'zcomplete-quit)
  "<remap> <abort-minibuffers>"            (zcomplete-bind-visible #'zcomplete-quit)

  "<left>"    (zcomplete-bind-arrows #'zcomplete-previous-completion t)
  "<right>"   (zcomplete-bind-arrows #'zcomplete-next-completion t)
  "<up>"      (zcomplete-bind-arrows #'zcomplete-previous-line-completion)
  "<down>"    (zcomplete-bind-arrows #'zcomplete-next-line-completion)
  "<home>"    (zcomplete-bind-arrows #'zcomplete-first-completion)
  "<end>"     (zcomplete-bind-arrows #'zcomplete-last-completion)
  "<next>"    (zcomplete-bind-arrows #'scroll-other-window)
  "<prior>"   (zcomplete-bind-arrows #'scroll-other-window-down))

(defun zcomplete-ret (&optional no-exit no-quit)
  "Choose the completion from the minibuffer in its completions window."
  (interactive "P")
  (condition-case nil
      (minibuffer-choose-completion no-exit no-quit)
    (error (minibuffer-complete-and-exit))))

(defun zcomplete-quit ()
  "Exit minibuffer for zcomplete."
  (interactive)
  (minibuffer-hide-completions))

(defcustom zcomplete-auto-choose nil
  "Non-nil means to automatically insert completions to the minibuffer.
It affects the variable `minibuffer-completion-auto-choose'.
This variable is usable only when `zcomplete-auto-exhibit' is nil."
  :type 'boolean)

(defun zcomplete-next-completion (&optional n)
  "Run `minibuffer-next-completion' without auto choosing."
  (interactive "p")
  (let ((minibuffer-completion-auto-choose zcomplete-auto-choose))
    (minibuffer-next-completion n)))

(defun zcomplete-previous-completion (&optional n)
  "Run `minibuffer-previous-completion' without auto choosing."
  (interactive "p")
  (let ((minibuffer-completion-auto-choose zcomplete-auto-choose))
    (minibuffer-previous-completion n)))

(defun zcomplete-next-line-completion (&optional n)
  "Run `minibuffer-next-line-completion' without auto choosing."
  (interactive "p")
  (let ((minibuffer-completion-auto-choose zcomplete-auto-choose))
    (minibuffer-next-line-completion n)))

(defun zcomplete-previous-line-completion (&optional n)
  "Run `minibuffer-previous-line-completion' without auto choosing."
  (interactive "p")
  (let ((minibuffer-completion-auto-choose zcomplete-auto-choose))
    (minibuffer-previous-line-completion n)))

(defun zcomplete-first-completion ()
  "Run `first-completion' from the minibuffer in its completions window."
  (interactive)
  (with-minibuffer-completions-window
    (when completions-highlight-face
      (setq-local cursor-face-highlight-nonselected-window t))
    (let ((minibuffer-completion-auto-choose zcomplete-auto-choose))
      (first-completion))))

(defun zcomplete-last-completion ()
  "Run `last-completion' from the minibuffer in its completions window."
  (interactive)
  (with-minibuffer-completions-window
    (when completions-highlight-face
      (setq-local cursor-face-highlight-nonselected-window t))
    (let ((minibuffer-completion-auto-choose zcomplete-auto-choose))
      (last-completion))))


;;;###autoload
(define-minor-mode zcomplete-mode
  "Toggle incremental minibuffer completion (Zcomplete mode).

When this global minor mode is enabled, typing in the minibuffer
continuously displays a list of possible completions that match
the string you have typed.  The list of completions is displayed
in the *Completions* window.

For more information, see Info node `(emacs)Zcomplete'.
For options you can set, `\\[customize-group] zcomplete'.

You can use the following key bindings to navigate and select
completions:

\\{zcomplete-minibuffer-map}"
  :global t :group 'zcomplete
  (remove-hook 'minibuffer-setup-hook #'zcomplete-minibuffer-setup)
  (when zcomplete-mode
    (add-hook 'minibuffer-setup-hook #'zcomplete-minibuffer-setup)
    ;; (setq-default completion-show-help nil
    ;;               completions-header-format nil)
    ))

(define-minor-mode zcomplete-minibuffer-mode
  "Enable arrows in the minibuffer.
The only purpose of this mode is to activate
`zcomplete-minibuffer-mode-map' in the minibuffer."
  :global nil)

(defun zcomplete--completion-table ()
  (if (window-minibuffer-p) minibuffer-completion-table
    (or (nth 2 completion-in-region--data)
        (message "In %S (w=%S): %S"
                 (current-buffer) (selected-window) (window-minibuffer-p)))))
(defun zcomplete--field-string ()
  (if (window-minibuffer-p)
      (minibuffer-contents)
    (buffer-substring-no-properties
     (nth 0 completion-in-region--data)
     (nth 1 completion-in-region--data))))
(defun zcomplete--field-beg ()
  (if (window-minibuffer-p) (minibuffer-prompt-end)
    (nth 0 completion-in-region--data)))
(defun zcomplete--field-end ()
  (if (window-minibuffer-p) (point-max)
    (nth 1 completion-in-region--data)))

(defun zcomplete-simple-completing-p ()
  "Non-nil if current window is a minibuffer that's doing simple completion."
  (unless executing-kbd-macro
    (let ((table (zcomplete--completion-table)))
      (and table
           (or (not (functionp table))
               (eq zcomplete-with-completion-tables t)
               (member table zcomplete-with-completion-tables))))))

(defun zcomplete-minibuffer-setup ()
  "Run in minibuffer on activation to establish incremental completion.
Usually run by inclusion in `minibuffer-setup-hook'."
  (when zcomplete-mode
    (setq-local zcomplete--initial-input (zcomplete--field-string))
    (setq-local zcomplete--previous-input nil)
    (zcomplete-minibuffer-mode 1)
    (add-hook 'post-command-hook #'zcomplete-post-command-hook nil t)
    (zcomplete-exhibit)
    (run-hooks 'zcomplete-minibuffer-setup-hook)))

(defun zcomplete-exhibit ()
  "Update zcomplete completions display.
Should be run via minibuffer `post-command-hook'.
See `zcomplete-mode' and `minibuffer-setup-hook'."
  (when (and zcomplete-mode
             (zcomplete-simple-completing-p)) ;Shouldn't be necessary.
    (when (and (or zcomplete-show-matches-on-no-input
                   (not (equal (zcomplete--field-string)
                               zcomplete--initial-input)))
               (not (equal (zcomplete--field-string)
                           zcomplete--previous-input))
               (or (eq zcomplete-auto-exhibit t)
                   (and (eq zcomplete-auto-exhibit 'visible)
                        (zcomplete-visible)))
               (or
                ;; Don't bother with delay after certain number of chars:
                (> (- (point) (zcomplete--field-beg))
                   zcomplete-max-delay-chars)
                ;; Don't delay if the completions are known.
                ;; UNUSED: completion-all-sorted-completions
                ;; Don't delay if alternatives number is small enough:
                (and (sequencep (zcomplete--completion-table))
                     (< (length (zcomplete--completion-table))
                        zcomplete-delay-completions-threshold))
                ;; Delay - give some grace time for next keystroke, before
                ;; embarking on computing completions:
                (sit-for zcomplete-compute-delay)))
      (save-excursion
        (goto-char (point-max))
        (setq-local zcomplete--previous-input (zcomplete--field-string))
        (minibuffer-completion-help)
        (unless zcomplete-auto-exhibit
          (zcomplete-first-completion))))))

(let ((keymap completion-in-region-mode-map))
  (keymap-set keymap "<left>"  #'zcomplete-previous-completion)
  (keymap-set keymap "<right>" #'zcomplete-next-completion)
  (keymap-set keymap "<up>"    #'zcomplete-previous-line-completion)
  (keymap-set keymap "<down>"  #'zcomplete-next-line-completion)
  (keymap-set keymap "<home>"  #'zcomplete-first-completion)
  (keymap-set keymap "<end>"   #'zcomplete-last-completion)
  (keymap-set keymap "<next>"  #'scroll-other-window)
  (keymap-set keymap "<prior>" #'scroll-other-window-down)
  (keymap-set keymap "RET"     #'zcomplete-ret))


(provide 'zcomplete)

;;; zcomplete.el ends here
