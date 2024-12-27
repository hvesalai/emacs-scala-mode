;;; scala-organise.el --- organise scala imports -*- lexical-binding: t -*- -*- lexical-binding: t -*-

;; Copyright (C) 2022 Sam Halliday
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  A simplistic command that organises Java-style import sections (i.e. no
;;  relative paths). Only the first import section, up to any non-import line
;;  (including comments) is organised. For anything more complex than this,
;;  consider using https://github.com/liancheng/scalafix-organize-imports
;;
;;; Code:

(require 'subr-x)

(defcustom scala-organise-first '(("java." "javax." "sun.") "scala.")
  "Prefixes (strings or lists of strings), that are organised first."
  :type 'listp
  :group 'scala
  :safe 'listp
  :local t)

(defun scala-organise ()
  "Organise the import section"
  (interactive)
  (save-excursion
    (goto-char 0)
    (let (;; alist of the form ("prefix." ("Symbol", "_", "etc"))
          (imports))
      (when (re-search-forward (rx line-start "import ") nil t)
        (forward-line 0)
        (let ((start (point)))
          (while (looking-at (rx (or "\n" (: "import " (group (+ (not (or "{" "\n"))))))))
            (when-let ((match (match-string-no-properties 1)))
              (goto-char (match-end 1))
              (setq imports
                    (if (looking-at "{")
                        ;; multi-part import
                        (let ((block-start (point)))
                          (forward-sexp)
                          (let* ((block (buffer-substring-no-properties block-start (point)))
                                 (parts (split-string block "," nil (rx (+ (or space "{" "}"))))))
                            (scala-organise--alist-append match parts imports)))
                      ;; standalone import
                      (let* ((part (car (reverse (split-string match (rx ".")))))
                             (prefix (string-remove-suffix part match)))
                        (scala-organise--alist-append prefix part imports)))))
            (forward-line 1))

          (delete-region start (point))
          (let* ((keys (sort (delete-dups (mapcar #'car imports)) #'string<)))
            (dolist (setting scala-organise-first)
              (let (done)
                (dolist (key keys)
                  (when (scala-organise--special-p key setting)
                    (insert (scala-organise--render (assoc key imports)))
                    (push key done)))
                (when done
                  (insert "\n"))
                (setq keys (seq-difference keys done))))
            (dolist (key keys)
              (insert (scala-organise--render (assoc key imports))))
            (when keys
              (insert "\n")))))
      (when (re-search-forward (rx line-start (* space) "import ") nil t)
        (message "Inline imports, starting at line %i, have not been organised." (line-number-at-pos))))))

(defun scala-organise--special-p (entry setting)
  "Return non-nil if the ENTRY string matches the SETTING (a string
or a list of strings)."
  (if (listp setting)
      (seq-find (lambda (s) (string-prefix-p s entry)) setting)
    (string-prefix-p setting entry)))

(defun scala-organise--render (entry)
  "Return a string for the ENTRY (prefix . entries).
Entries will be alphabetically sorted and deduped. If the special
character `_' appears, it will replace all other (non-renamed)
entries."
  (let* ((parts (sort (delete-dups (cdr entry)) #'string<))
         (parts_ (if (member "_" parts)
                      (cons "_" (seq-filter (lambda (e) (string-match-p (rx "=>") e)) parts))
                    parts))
         (clean (lambda (s) (replace-regexp-in-string (rx (* space) "=>" (* space)) " => " s)))
         (rendered (if (and (= (length parts_) 1)
                            (not (string-match-p (rx "=>") (car parts_))) )
                       (car parts_)
                     (concat "{ " (mapconcat clean parts_ ", ") " }"))))
    (concat "import " (car entry) rendered "\n")))

(defun scala-organise--alist-append (key value alist)
  "Return an ALIST with KEY mapped to VALUE `append'ed to the existing value.
If VALUE (or the existing value) is not a list, it will be
converted into a single element list before being appended."
  (let* ((existing (cdr (assoc key alist)))
         (existing_ (if (listp existing) existing (list existing)))
         (value_ (if (listp value) value (list value)))
         (update (append value_ existing_)))
    (cons (cons key update) alist)))

(provide 'scala-organise)
;;; scala-organise.el ends here -*- lexical-binding: t -*-
