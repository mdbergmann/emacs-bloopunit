;;; scalaunit.el --- `Scala test' runner -*- lexical-binding: t -*-

;; Copyright (C) 2021 Manfred Bergmann.

;; Author: Manfred Bergmann <manfred.bergmann@me.com>
;; URL: http://github.com/mdbergmann/emacs-scalaunit
;; Version: 0.1
;; Keywords: processes scala bloop test
;; Package-Requires: ((emacs "24.3"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides commands to run test cases in a Scala Bloop project.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)

(make-variable-buffer-local
 (defvar scalaunit-mode))

(defvar-local *bloop-project* nil)
(defvar-local *test-kind* 'scalatest)

(defvar-local *last-test* nil)

(defvar *scalaunit-output-buf-name* "*scalaunit output*")

(cl-defun scalaunit--get-buffer-text (&optional (beg 1) (end (point-max)))
  "Retrieve the buffer text. Specify BEG and END for specific range."
  (buffer-substring-no-properties beg end))

(defun scalaunit--find-test-class (buffer-text)
  "Generate the package for the test run.
This is usually the full designated class.
BUFFER-TEXT is a string where the matching should take place."
  (let* ((package-string (progn
                           (string-match "^package[ ]+\\(.+\\).*$"
                                         buffer-text)
                           (match-string 1 buffer-text)))
         (clazz-string (progn
                         (string-match "^class[ ]+\\(\\w+\\).*$"
                                       buffer-text)
                         (match-string 1 buffer-text))))
    (message "Package: %s" package-string)
    (message "Class: %s" clazz-string)
    (format "%s.%s" package-string clazz-string)
    ))

(defun scalaunit--project-root-dir ()
  "Return the project root directory."
  (locate-dominating-file default-directory ".bloop"))

(defun scalaunit--execute-test-in-context (test)
  "Call specific test. TEST specifies a test to run."
  (let* ((test-cmd-args (list "bloop" "test" *bloop-project*
                              "--only" test))
         (call-args
          (append (list (car test-cmd-args) nil *scalaunit-output-buf-name* t)
                  (cdr test-cmd-args))))
    (message "calling: %s" call-args)
    (let* ((default-directory (scalaunit--project-root-dir))
           (call-result (apply 'call-process call-args)))
      (message "cwd: %s" default-directory)
      (message "test call result: %s" call-result)
      call-result)))

(defun scalaunit--handle-successful-test-result ()
  "Do some stuff when the test ran OK."
  (message "%s" (propertize "Tests OK" 'face '(:foreground "green"))))

(defun scalaunit--handle-unsuccessful-test-result ()
  "Do some stuff when the test ran NOK."
  (message "%s" (propertize "Tests failed!" 'face '(:foreground "red"))))

(defun scalaunit--retrieve-projects ()
  "Retrieve the available bloop projects."
  (let ((default-directory (scalaunit--project-root-dir)))
    (message "%s" (shell-command-to-string "bloop projects"))))

(cl-defun scalaunit--run-test (&optional (test-spec nil))
  "Execute the test. Specify optional TEST-SPEC if a specific test should be run."
  (message "scalaunit: run-test")

  (unless *bloop-project*
    (error "Please set a bloop project first!"))
  
  (unless (string-equal "scala-mode" major-mode)
    (error "Need 'scala-mode' to run!"))
  
  (get-buffer-create *scalaunit-output-buf-name*)

  (with-current-buffer *scalaunit-output-buf-name*
    (erase-buffer))
  
  (let* ((test-to-run (if test-spec
                          test-spec
                        (scalaunit--find-test-class
                         (scalaunit--get-buffer-text))))
         (test-result (scalaunit--execute-test-in-context test-to-run)))
    (setq-local *last-test* test-to-run)
    (when test-result
      (if (= test-result 0)
          (scalaunit--handle-successful-test-result)
        (scalaunit--handle-unsuccessful-test-result))
      (with-current-buffer *scalaunit-output-buf-name*
        (ansi-color-apply-on-region (point-min) (point-max))))))

(defun scalaunit-run ()
  "Save buffers and execute command to run the test."
  (interactive)
  (scalaunit--run-preps)
  (scalaunit--run-test))

(defun scalaunit-run-last ()
  "Save buffers and execute command to run the test."
  (interactive)
  (scalaunit--run-preps)
  (scalaunit--run-test *last-test*))

(defun scalaunit--run-preps ()
  "Save buffers."
  (save-buffer)
  (save-some-buffers))

(defun scalaunit-select-project ()
  "Prompts for the Bloop project."
  (interactive)

  (setq-local *bloop-project* (completing-read "[scalaunit] Bloop project: "
                                               (split-string (scalaunit--retrieve-projects))))
  (message "Selected project: %s" *bloop-project*))

(define-minor-mode scalaunit-mode
  "Scala unit - test runner. Runs a command that runs tests."
  :lighter " ScalaUnit"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-t") 'scalaunit-run)
            (define-key map (kbd "C-c C-r") 'scalaunit-run-last)
            (define-key map (kbd "C-c C-p") 'scalaunit-select-project)
            map))

(provide 'scalaunit)

;; ---------------------------------------------
;; tests ;; more tests
;; ---------------------------------------------

(defvar scalaunit--run-tests nil)

(eval-when-compile
  (setq scalaunit--run-tests t))

(defun scalaunit--test--find-test-class ()
  "Test finding the class context."
  (let ((buffer-text "package foo.bar
class FooBar {
}"))
    (assert (string= "foo.bar.FooBar"
                     (scalaunit--find-test-class buffer-text)))))

(when scalaunit--run-tests
  (scalaunit--test--find-test-class)
  )

;;; scalaunit.el ends here
