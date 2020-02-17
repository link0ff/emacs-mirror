
(with-temp-buffer
  (vc-call-backend
   (vc-responsible-backend default-directory) 'dir-status-files default-directory nil
   (lambda (entries &optional more-to-come)
     (message "! %S" entries))))
