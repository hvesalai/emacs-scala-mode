;;; scala-mode-fontlock.el - Major mode for editing scala, font-lock
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

(provide 'scala-mode-fontlock)

(require 'scala-mode-syntax)
(require 'scala-mode-constants)

(defun scala-font-lock:mark-symbol (limit)
  (if (re-search-forward scala-syntax:reserved-symbols-re limit t)
      (goto-char (match-end 2)) ;; step back to the match (re matches futher)
    nil))

(defun scala-font-lock:mark-underscore (limit)
  (if (re-search-forward scala-syntax:reserved-symbol-underscore-re limit t)
      (goto-char (match-end 2)) ;; step back to the match (re matches futher)
    nil))
  
(defvar scala-font-lock:keywords
  `(;; keywords
    (,scala-syntax:keywords-re 0 font-lock-keyword-face)

    ;; symbols
    (scala-font-lock:mark-symbol 2 font-lock-keyword-face)
    (scala-font-lock:mark-underscore 2 font-lock-keyword-face)

))
