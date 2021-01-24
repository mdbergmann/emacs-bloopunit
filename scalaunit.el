;;; scalaunit.el --- `Scala test' runner -*- lexical-binding: t -*-

;; Copyright (C) 2021 Manfred Bergmann.

;; Author: Manfred Bergmann <manfred.bergmann@me.com>
;; URL: http://github.com/mdbergmann/scalaunit
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

(make-variable-buffer-local
 (defvar scalaunit-mode))

(make-variable-buffer-local
 (defvar bloop-project nil))

(make-variable-buffer-local
 (defvar test-kind 'scalatest))

(defvar *scalaunit-output-buf-name* "scalaunit output")

(defun scalaunit--find-test-class ()
  "Generate the package for the test run. This is usually the full designated class."
  (let* ((buffer-text (buffer-substring-no-properties 1 (point-max)))
         (package-string (progn
                           (string-match "^package[ ]+\\(.+\\).*$"
                                         buffer-text)
                           (match-string 1 buffer-text)))
         (clazz-string (progn
                         (string-match "^class[ ]+\\(\\w+\\)[ ]+extends"
                                       buffer-text)
                         (match-string 1 buffer-text))))
    (message "Package: %s" package-string)
    (message "Class: %s" clazz-string)
    (format "%s.%s" package-string clazz-string)
    ))

(defun scalaunit--project-root-dir ()
  "Return the project root directory."
  (locate-dominating-file default-directory ".bloop"))

(defun scalaunit--execute-test-in-context ()
  "Call specific test."
  (let* ((test-cmd-args (list "bloop" "test" bloop-project "--only" (scalaunit--find-test-class)))
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

(defun scalaunit--run-test ()
  "Execute the test."
  (message "scalaunit: run-test")

  (unless bloop-project
    (message "Please set a bloop project first!"))
  
  (unless (string-equal "scala-mode" major-mode)
    (message "Need 'scala-mode' to run!")
    (return-from 'scalaunit--run-test))
  
  (get-buffer-create *scalaunit-output-buf-name*)

  (with-current-buffer *scalaunit-output-buf-name*
    (erase-buffer))

  (let ((test-result (scalaunit--execute-test-in-context)))
    (when test-result
      (if (= test-result 0)
          (scalaunit--handle-successful-test-result)
        (scalaunit--handle-unsuccessful-test-result))
      (with-current-buffer *scalaunit-output-buf-name*
        (ansi-color-apply-on-region (point-min) (point-max))))))
      
(defun scalaunit-run ()
  "Save buffers and execute command to run the test."
  (interactive)
  (save-buffer)
  (save-some-buffers)
  (scalaunit--run-test))

(defun scalaunit-set-project ()
  "Prompts for the Bloop project."
  (interactive)

  (setq bloop-project (completing-read "[scalaunit] Bloop project: "
                                       (split-string (scalaunit--retrieve-projects)))))

(define-minor-mode scalaunit-mode
  "Scala unit - test runner. Runs a command that runs tests."
  :lighter " ScalaUnit"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c t") 'scalaunit-run)
            map))

(provide 'scalaunit)
;;; scalaunit.el ends here
