;;-*- Mode: Emacs-Lisp -*-
;;; Cask --- project definition

;; Copyright (C) 2015 Sam Halliday

;; Author: Sam Halliday <Sam.Halliday@gmail.com>

;;; Commentary:
;;
;;  Cask is a package manager for emacs lisp projects, this generates
;;  the *-pkg.el file and could be our test runner in the future.
;;
;;  See http://cask.readthedocs.org/en/latest/guide/dsl.html for more
;;  information about Cask.
;;
;;    cask pkg-file
;;
;;    cask update
;;    cask install
;;
;;  are particularly useful commands.
;;
;; To run the tests:
;;    cask exec ert-runner
;;
;;; Code:

(source melpa-stable)

(package-file "scala-mode.el")

(development
 (depends-on "ert-runner")
 (depends-on "ecukes")
 (depends-on "espuds")
 (depends-on "undercover"))

;;; Cask ends here
