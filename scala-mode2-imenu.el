;;; scala-mode-lib.el - Major mode for editing scala, common functions
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

(require 'scala-mode2-syntax)

(defcustom scala-imenu:should-flatten-index nil
  "Controls whether or not the imenu index is flattened or hierarchical.")

(defun scala-imenu:create-index ()
  (interactive)
  (let ((class nil) (index nil))
    (goto-char (point-max))
    (while (setq class (scala-imenu:previous-class))
      (setq index (cons class index)))
    (if scala-imenu:should-flatten-index (scala-imenu:flatten-index index) index)))

(defun scala-imenu:previous-class ()
  (interactive)
  (let ((last-point (point)) (class-name nil))
    (scala-syntax:beginning-of-definition)
    (if (eq (point) last-point) nil
      (progn (save-excursion (re-search-forward scala-syntax:all-definition-re)
		      
			     (setq class-name (match-string-no-properties 2)))
	     `(,class-name . ,(cons `("<class>" . ,(point-marker))
				    (scala-imenu:class-members)))))))

(defun scala-imenu:class-members ()
  (interactive)
  (let ((start-point (point)))
    (save-excursion (scala-syntax:end-of-definition)
		    (backward-char)
		    (scala-imenu:get-class-members start-point))))

(defun scala-imenu:get-class-members (stop-at-point)
  (scala-syntax:beginning-of-definition)
  (let ((marker (point-marker)))
    (if (< stop-at-point (point))
	(let ((member-name (save-excursion
			     (re-search-forward scala-syntax:all-definition-re)
			     (match-string-no-properties 2))))
	  (cons `(,member-name . ,marker)
		(scala-imenu:get-class-members stop-at-point)))
      nil)))

(defun scala-imenu:flatten-index (index)
  (reduce #'append
	  (mapcar (lambda (class-info)
		    (let ((class-name (car class-info)))
		      (mapcar (lambda (member-info) `(,(concat class-name "." (car member-info)) .
						      ,(cdr member-info)))
			      (cdr class-info)))) index)))


(provide 'scala-mode2-imenu)
