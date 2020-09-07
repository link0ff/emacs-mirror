;;; dbus-tests.el --- Tests of D-Bus integration into Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2020 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `https://www.gnu.org/licenses/'.

;;; Code:

(require 'ert)
(require 'dbus)

(defvar dbus-debug nil)
(declare-function dbus-get-unique-name "dbusbind.c" (bus))

(defconst dbus--test-enabled-session-bus
  (and (featurep 'dbusbind)
       (dbus-ignore-errors (dbus-get-unique-name :session)))
  "Check, whether we are registered at the session bus.")

(defconst dbus--test-enabled-system-bus
  (and (featurep 'dbusbind)
       (dbus-ignore-errors (dbus-get-unique-name :system)))
  "Check, whether we are registered at the system bus.")

(defconst dbus--test-service "org.gnu.Emacs.TestDBus"
  "Test service.")

(defconst dbus--test-path "/org/gnu/Emacs/TestDBus"
  "Test object path.")

(defconst dbus--test-interface "org.gnu.Emacs.TestDBus"
  "Test interface.")

(defun dbus--test-availability (bus)
  "Test availability of D-Bus BUS."
  (should (dbus-list-names bus))
  (should (dbus-list-activatable-names bus))
  (should (dbus-list-known-names bus))
  (should (dbus-get-unique-name bus)))

(ert-deftest dbus-test00-availability-session ()
  "Test availability of D-Bus `:session'."
  :expected-result (if dbus--test-enabled-session-bus :passed :failed)
  (dbus--test-availability :session))

(ert-deftest dbus-test00-availability-system ()
  "Test availability of D-Bus `:system'."
  :expected-result (if dbus--test-enabled-system-bus :passed :failed)
  (dbus--test-availability :system))

(ert-deftest dbus-test01-type-conversion ()
  "Check type conversion functions."
  (let ((ustr "0123abc_xyz\x01\xff")
	(mstr "Grüß Göttin"))
    (should
     (string-equal
      (dbus-byte-array-to-string (dbus-string-to-byte-array "")) ""))
    (should
     (string-equal
      (dbus-byte-array-to-string (dbus-string-to-byte-array ustr)) ustr))
    (should
     (string-equal
      (dbus-byte-array-to-string (dbus-string-to-byte-array mstr) 'multibyte)
      mstr))
    ;; Should not work for multibyte strings.
    (should-not
     (string-equal
      (dbus-byte-array-to-string (dbus-string-to-byte-array mstr)) mstr))

    (should
     (string-equal
      (dbus-unescape-from-identifier (dbus-escape-as-identifier "")) ""))
    (should
     (string-equal
      (dbus-unescape-from-identifier (dbus-escape-as-identifier ustr)) ustr))
    ;; Should not work for multibyte strings.
    (should-not
     (string-equal
      (dbus-unescape-from-identifier (dbus-escape-as-identifier mstr)) mstr))))

(defun dbus--test-register-service (bus)
  "Check service registration at BUS."
  ;; Cleanup.
  (dbus-ignore-errors (dbus-unregister-service bus dbus--test-service))

  ;; Register an own service.
  (should (eq (dbus-register-service bus dbus--test-service) :primary-owner))
  (should (member dbus--test-service (dbus-list-known-names bus)))
  (should (eq (dbus-register-service bus dbus--test-service) :already-owner))
  (should (member dbus--test-service (dbus-list-known-names bus)))

  ;; Unregister the service.
  (should (eq (dbus-unregister-service bus dbus--test-service) :released))
  (should-not (member dbus--test-service (dbus-list-known-names bus)))
  (should (eq (dbus-unregister-service bus dbus--test-service) :non-existent))
  (should-not (member dbus--test-service (dbus-list-known-names bus)))

  ;; `dbus-service-dbus' is reserved for the BUS itself.
  (should-error (dbus-register-service bus dbus-service-dbus))
  (should-error (dbus-unregister-service bus dbus-service-dbus)))

(ert-deftest dbus-test02-register-service-session ()
  "Check service registration at `:session' bus."
  (skip-unless (and dbus--test-enabled-session-bus
		    (dbus-register-service :session dbus--test-service)))
  (dbus--test-register-service :session)

  (let ((service "org.freedesktop.Notifications"))
    (when (member service (dbus-list-known-names :session))
      ;; Cleanup.
      (dbus-ignore-errors (dbus-unregister-service :session service))

      (should (eq (dbus-register-service :session service) :in-queue))
      (should (eq (dbus-unregister-service :session service) :released))

      (should
       (eq (dbus-register-service :session service :do-not-queue) :exists))
      (should (eq (dbus-unregister-service :session service) :not-owner)))))

(ert-deftest dbus-test02-register-service-system ()
  "Check service registration at `:system' bus."
  (skip-unless (and dbus--test-enabled-system-bus
		    (dbus-register-service :system dbus--test-service)))
  (dbus--test-register-service :system))

(ert-deftest dbus-test02-register-service-own-bus ()
  "Check service registration with an own bus.
This includes initialization and closing the bus."
  ;; Start bus.
  (let ((output
	 (ignore-errors
	   (shell-command-to-string "env DISPLAY= dbus-launch --sh-syntax")))
	bus pid)
    (skip-unless (stringp output))
    (when (string-match "DBUS_SESSION_BUS_ADDRESS='\\(.+\\)';" output)
      (setq bus (match-string 1 output)))
    (when (string-match "DBUS_SESSION_BUS_PID=\\([[:digit:]]+\\);" output)
      (setq pid (match-string 1 output)))
    (unwind-protect
	(progn
	  (skip-unless
	   (dbus-ignore-errors
	     (and bus pid
		  (featurep 'dbusbind)
		  (dbus-init-bus bus)
		  (dbus-get-unique-name bus)
		  (dbus-register-service bus dbus--test-service))))
	  ;; Run the test.
	  (dbus--test-register-service bus))

      ;; Save exit.
      (when pid (call-process "kill" nil nil nil pid)))))

(ert-deftest dbus-test03-peer-interface ()
  "Check `dbus-interface-peer' methods."
  (skip-unless
   (and dbus--test-enabled-session-bus
	(dbus-register-service :session dbus--test-service)
	;; "GetMachineId" is not implemented (yet).  When it returns a
	;; value, another D-Bus client like dbus-monitor is reacting
	;; on `dbus-interface-peer'.  We cannot test then.
	(not
	 (dbus-ignore-errors
	   (dbus-call-method
	    :session dbus--test-service dbus-path-dbus
	    dbus-interface-peer "GetMachineId" :timeout 100)))))

  (should (dbus-ping :session dbus--test-service 100))
  (dbus-unregister-service :session dbus--test-service)
  (should-not (dbus-ping :session dbus--test-service 100)))

(defun dbus--test-method-handler (&rest args)
  "Method handler for `dbus-test04-register-method'."
  (cond
   ;; No argument.
   ((null args)
    :ignore)
   ;; One argument.
   ((= 1 (length args))
    (car args))
   ;; Two arguments.
   ((= 2 (length args))
    `(:error ,dbus-error-invalid-args
             ,(format-message "Wrong arguments %s" args)))
   ;; More than two arguments.
   (t (signal 'dbus-error (cons "D-Bus signal" args)))))

(ert-deftest dbus-test04-register-method ()
  "Check method registration for an own service."
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))

  (unwind-protect
      (let ((method "Method")
            (handler #'dbus--test-method-handler))

        (should
         (equal
          (dbus-register-method
           :session dbus--test-service dbus--test-path
           dbus--test-interface method handler)
          `((:method :session ,dbus--test-interface ,method)
            (,dbus--test-service ,dbus--test-path ,handler))))

        ;; No argument, returns nil.
        (should-not
         (dbus-call-method
          :session dbus--test-service dbus--test-path
          dbus--test-interface method))
        ;; One argument, returns the argument.
        (should
         (string-equal
          (dbus-call-method
           :session dbus--test-service dbus--test-path
           dbus--test-interface method "foo")
          "foo"))
        ;; Two arguments, D-Bus error activated as `(:error ...)' list.
        (should
         (equal
          (should-error
           (dbus-call-method
            :session dbus--test-service dbus--test-path
            dbus--test-interface method "foo" "bar"))
          `(dbus-error ,dbus-error-invalid-args "Wrong arguments (foo bar)")))
        ;; Three arguments, D-Bus error activated by `dbus-error' signal.
        (should
         (equal
          (should-error
           (dbus-call-method
            :session dbus--test-service dbus--test-path
            dbus--test-interface method "foo" "bar" "baz"))
          `(dbus-error
            ,dbus-error-failed
            "D-Bus error: \"D-Bus signal\", \"foo\", \"bar\", \"baz\""))))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

(ert-deftest dbus-test05-register-property ()
  "Check property registration for an own service."
  (skip-unless dbus--test-enabled-session-bus)
  (dbus-ignore-errors (dbus-unregister-service :session dbus--test-service))

  (unwind-protect
      (let ((property1 "Property1")
            (property2 "Property2")
            (property3 "Property3"))

        ;; `:read' property.
        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property1 :read "foo")
          `((:property :session "org.gnu.Emacs.TestDBus" ,property1)
            (,dbus--test-service ,dbus--test-path))))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property1)
          "foo"))
        (should-not ;; Due to `:read' access type.
         (dbus-set-property
          :session dbus--test-service dbus--test-path
          dbus--test-interface property1 "foofoo"))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property1)
          "foo"))

        ;; `:write' property.
        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property2 :write "bar")
          `((:property :session "org.gnu.Emacs.TestDBus" ,property2)
            (,dbus--test-service ,dbus--test-path))))
        (should-not ;; Due to `:write' access type.
         (dbus-get-property
          :session dbus--test-service dbus--test-path
          dbus--test-interface property2))
        (should
         (string-equal
          (dbus-set-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property2 "barbar")
          "barbar"))
        (should-not ;; Due to `:write' access type.
         (dbus-get-property
          :session dbus--test-service dbus--test-path
          dbus--test-interface property2))

        ;; `:readwrite' property.
        (should
         (equal
          (dbus-register-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property3 :readwrite "baz")
          `((:property :session "org.gnu.Emacs.TestDBus" ,property3)
            (,dbus--test-service ,dbus--test-path))))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property3)
          "baz"))
        (should
         (string-equal
          (dbus-set-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property3 "bazbaz")
          "bazbaz"))
        (should
         (string-equal
          (dbus-get-property
           :session dbus--test-service dbus--test-path
           dbus--test-interface property3)
          "bazbaz"))

        ;; `dbus-get-all-properties'.  We cannot retrieve a value for
        ;; the property with `:write' access type.
        (let ((result
               (dbus-get-all-properties
                :session dbus--test-service dbus--test-path
                dbus--test-interface)))
          (should (string-equal (cdr (assoc property1 result)) "foo"))
          (should (string-equal (cdr (assoc property3 result)) "bazbaz"))
          (should-not (assoc property2 result))))

        ;; FIXME: This is wrong! The properties are missing.
        ;; (should
        ;;  (equal
        ;;   (dbus-get-all-managed-objects
        ;;    :session dbus--test-service dbus--test-path)
        ;;   `((,dbus--test-path
        ;;      ((,dbus-interface-peer)
        ;;       (,dbus-interface-objectmanager)
        ;;       (,dbus-interface-properties)))))))

    ;; Cleanup.
    (dbus-unregister-service :session dbus--test-service)))

(defun dbus-test-all (&optional interactive)
  "Run all tests for \\[dbus]."
  (interactive "p")
  (funcall (if interactive #'ert-run-tests-interactively #'ert-run-tests-batch)
           "^dbus"))

(provide 'dbus-tests)
;;; dbus-tests.el ends here
