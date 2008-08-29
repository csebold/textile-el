; -*- mode: emacs-lisp; mode: auto-fill; -*-
; unit-test.el (part of textile.el)
; $Id$

; by Charles Sebold <csebold+textile@gmail.com>

; see copyright notice at the top of textile2.el

(require 'textile2)

(defun Tut-concat-lines (my-list &optional separator)
  "Concatenate lines together, separating them with SEPARATOR (defaults
to \\n)."
  (unless separator
    (setq separator "\n"))
  (if (cdr my-list)
      (concat (car my-list) separator
              (Tut-concat-lines (cdr my-list) separator))
    (car my-list)))

(defun Tut-trim-string (my-string)
  "Remove leading and trailing newlines and comments from a string."
  (let (my-list)
    (dolist (this-line (split-string my-string "\n"))
      (unless (string-match "^//" this-line)
        (push this-line my-list)))
    (while (string= (car my-list) "")
      (pop my-list))
    (setq my-list (reverse my-list))
    (while (string= (car my-list) "")
      (pop my-list))
    (Tut-concat-lines my-list)))

(defun Tut-get-title (my-string)
  "Get title from comment string."
  (if (string-match "^//\\([^\n]+\\)" my-string)
      (match-string 1 my-string)
    "Untitled"))

(defun textile-unit-test ()
  "Test Textile functionality against a standard testcases.txt-format
file."
  (interactive)
  (let ((all-tests (delete "" (split-string (buffer-string)
                                            "-----\n"))))
    (dolist (this-test all-tests)
      (let* ((test-q-and-a (delete ""
                                   (split-string this-test
                                                 "==>\n")))
             (test-title (Tut-get-title (car test-q-and-a)))
             (test-q (Tut-trim-string (car test-q-and-a)))
             (test-a (Tut-trim-string (cadr test-q-and-a)))
             (test-x (textile-string test-q)))
        (if (not (string= test-a test-x))
            (message "Test %s failed." test-title))))))

(provide 'unit-test)