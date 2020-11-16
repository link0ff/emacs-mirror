
;; TODO: when Isearch is active, show messages at the end of Isearch prompt

(require 'cl)
(defvar test-i 0)
(defvar test-timer (run-with-timer 3 3 (lambda () (message "☃!%s" (incf test-i)))))
(cancel-timer test-timer)

(setq set-message-function 'set-minibuffer-message)
(setq clear-message-function 'clear-minibuffer-message)


;;; BAK

(defun set-multi-message--wrapper (message)
  (list (set-multi-message (car message))))

(setq set-message-function 'set-multi-message)
(setq set-message-function 'set-minibuffer-message)
(add-function :filter-args set-message-function #'set-multi-message--wrapper)
(funcall set-message-function "mess")
(set-multi-message--wrapper "mess")

(add-function :around set-message-function #'set-multi-message)
(add-function :filter-args set-message-function #'set-multi-message)
(add-function :filter-return set-message-function #'set-multi-message)
(remove-function set-message-function #'set-multi-message)

;; (apply OLDFUN (funcall FUNCTION r))
(apply 'set-minibuffer-message '("mess"))
(funcall 'set-multi-message "mess")
(funcall 'set-minibuffer-message (funcall 'set-multi-message "mess"))
(apply 'set-minibuffer-message (funcall 'set-multi-message--wrapper "mess"))

(defun fun1 (m) (concat m "1"))
(defun fun2 (m) (concat m "2"))
(defvar var1 'fun1)
(add-function :filter-args var1 'fun2)
(funcall var1 "m")

(setq set-message-function nil)
(set-multi-message "message1")
(defun test-stack ()
  (interactive)
  ;; (setq multi-message-list nil)
  (dotimes (i 12)
    (message "Message %s" i)))


(defun set-multi-message--wrapper (orig-fun message)
  (let* ((multi-message (set-multi-message message)))
    (or (funcall orig-fun multi-message)
        multi-message)))

(setq set-message-function 'set-minibuffer-message)
(add-function :around set-message-function #'set-multi-message--wrapper)

(defun clear-multi-message--wrapper (orig-fun)
  (funcall orig-fun)
  (setq multi-message-list nil)
  (message "\n"))

(setq clear-message-function 'clear-minibuffer-message)
(add-function :around clear-message-function #'clear-multi-message--wrapper)
(remove-function clear-message-function #'clear-multi-message--wrapper)


