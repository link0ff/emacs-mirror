
(require 'cl)
(defvar test-i 0)
(defvar test-timer (run-with-timer 3 3 (lambda () (message "☃!%s" (incf test-i)))))
(cancel-timer test-timer)

(setq set-message-function 'set-minibuffer-message)
(setq clear-message-function 'clear-minibuffer-message)
