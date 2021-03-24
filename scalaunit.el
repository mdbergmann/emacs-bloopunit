;;; scalaunit.el --- `Scala test' runner -*- lexical-binding: t -*-

;; Copyright (C) 2021 Manfred Bergmann.

;; Author: Manfred Bergmann <manfred.bergmann@me.com>
;; URL: http://github.com/mdbergmann/emacs-scalaunit
;; Version: 0.2
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

(defun scalaunit--find-test-method (buffer-text curr-position)
  "Find a single test case for the test run.
BUFFER-TEXT is a string where the matching should take place.
CURR-POSITION is the current position of the curser in the buffer."
  (with-temp-buffer
    (insert buffer-text)
    (goto-char curr-position)
    (let ((found-point (search-backward "test(" nil t)))
      (message "point: %s" found-point)
      (if found-point
          (let ((matches (string-match "test(\"\\(.*\\)\").*"
                                       buffer-text
                                       (- found-point 1))))
            (message "matches: %s" matches)
            (match-string 1 buffer-text))))))

(defun scalaunit--project-root-dir ()
  "Return the project root directory."
  (locate-dominating-file default-directory ".bloop"))

(defun scalaunit--execute-test-in-context (test-args)
  "Call specific test. TEST-ARGS specifies a test to run."
  (message "Run with test args: %s" test-args)
  (let* ((test-cmd-args (append
                         (list "bloop" "test" *bloop-project* "--only")
                         test-args))
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

(defun scalaunit--get-test-args (test-spec single)
  "Calculates test-args as used in exeute-in-context.
TEST-SPEC is a given, previously executed test.
When this is not null, it'll be used.
Otherwise we calculate a new test-spec, from class
and (maybe) single test case if SINGLE is T."
  (if (not (null test-spec))
      test-spec
    (let* ((test-class (scalaunit--find-test-class
                        (scalaunit--get-buffer-text)))
           (single-test (if single
                            (scalaunit--find-test-method
                             (scalaunit--get-buffer-text)
                             (point))
                          nil))
           (test-args (list test-class)))
      (if single-test
          (append test-args
                  (list "--" "-z" single-test))
        test-args))))

(cl-defun scalaunit--run-test (&optional (test-spec nil) (single nil))
  "Execute the test.
Specify optional TEST-SPEC if a specific test should be run.
Specify optional SINGLE (T)) to try to run only a single test case."
  (message "scalaunit: run-test")

  (unless *bloop-project*
    (error "Please set a bloop project first!"))
  
  (unless (string-equal "scala-mode" major-mode)
    (error "Need 'scala-mode' to run!"))
  
  (get-buffer-create *scalaunit-output-buf-name*)

  (with-current-buffer *scalaunit-output-buf-name*
    (erase-buffer))
  
  (let* ((test-args (scalaunit--get-test-args test-spec single))
         (test-result (scalaunit--execute-test-in-context test-args)))
    (setq-local *last-test* test-args)
    (when test-result
      (if (= test-result 0)
          (scalaunit--handle-successful-test-result)
        (scalaunit--handle-unsuccessful-test-result))
      (with-current-buffer *scalaunit-output-buf-name*
        (ansi-color-apply-on-region (point-min) (point-max))))))

(defun scalaunit-run-all ()
  "Save buffers and execute all test cases in the context."
  (interactive)
  (scalaunit--run-preps)
  (scalaunit--run-test))

(defun scalaunit-run-single ()
  "Save buffers and execute a single test in the context."
  (interactive)
  (scalaunit--run-preps)
  (scalaunit--run-test nil t))

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
            (define-key map (kbd "C-c C-t") 'scalaunit-run-all)
            (define-key map (kbd "C-c C-s") 'scalaunit-run-single)
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

(defun scalaunit--test--find-test-method ()
  "Test finding the test-case context."
  (let ((buffer-text "some stuff
  test(\"foo bar test\") {
in test
}"))
    (assert (string= "foo bar test"
                     (scalaunit--find-test-method buffer-text 41)))))

(when scalaunit--run-tests
  (scalaunit--test--find-test-class)
  (scalaunit--test--find-test-method)
  )

;;; scalaunit.el ends here
