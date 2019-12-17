
(read-answer "Question: "
             `(("help0" ?h "show help")
               ("help1" ,(kbd "<f1>") "show help")
               ("help2" ,(kbd "C-M-h") "show help")
               ("help3" ,(kbd "C-h") "show help")))

(read-answer "Question: "
             '(("yes"  ?y "perform the action")
               ("no"   ?n "skip to the next")
               ("all"  ?! "accept all remaining without more questions")
               ("help" ?h "show help")
               ("quit" ?q "exit")))

(defvar test-i 0)
(defvar test-timer (run-with-timer 3 3 (lambda () (message "!!!!!!!!!!%s" (incf test-i)))))
(cancel-timer test-timer)

(setq set-message-function 'set-minibuffer-message)
(setq clear-message-function 'clear-minibuffer-message)
