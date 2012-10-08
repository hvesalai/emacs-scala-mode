;;; scala-mode.el - Major mode for editing scala, indenting
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

(provide 'scala-mode-indent)

(defun scala-indent:run-on-p (&optional point) 
  (interactive)
  "Returns t if the current point (or point at 'point) is on a
line that is a run-on from a previous line."
  (save-excursion
    (when point (goto-char point))
    (scala-syntax:beginning-of-code-line)
    (not (or (looking-at scala-syntax:mustNotContinue-re)
             (scala-syntax:looking-back-empty-line-p)
             (scala-syntax:looking-back-token 
              scala-syntax:mustTerminate-re)))))

;    (defconst scala-syntax:mustNotTerminate-re
;      scala-syntax:reserved-symbols-unsafe-re

(defun scala-indent:goto-run-on-beginning ()
  "Moves back to the point whose column will be used as the
anchor relative to which indenting is calculated. If this row is
not a run on, does nothing."
  (when (scala-indent:run-on-p)
    (scala-syntax:beginning-of-code-line)
    (let ((block-beg (or (nth 1 (syntax-ppss)) (point-min))))
      (while (and (scala-indent:run-on-p)
                  (> (point) block-beg))
        (if (scala-syntax:looking-back-token "\\s)" 1)
            (backward-list)
          (skip-syntax-backward "^)" (max block-beg (line-beginning-position 0))))))
    (unless (= (char-syntax (char-before)) ?\()         
      (scala-syntax:beginning-of-code-line))))

(defun scala-indent:list-element-line-p (&optional point)
  "Returns t if the current point is in a list. A list is
something that begins with '(' or '[', or 'for {'. A list element
is preceded by ,"
  (save-excursion
    ;; first check that the previous line ended with ','
    (beginning-of-line)
    (let ((list-beg (nth 1 (syntax-ppss))))
      (if (not (and (scala-syntax:looking-back-token "," 1)
                    list-beg))
          ;; not at list element 2..n (list element 1 is not considered)
          nil
        (goto-char list-beg)
        (or (= (char-after list-beg) ?\()
            (= (char-after list-beg) ?\[)
            (and (= (char-after list-beg) ?\{)
                 (scala-syntax:looking-back-token "for")))))))

(defun scala-indent:goto-list-beginning ()
  "Moves back to the point whose column will be used to indent
lists rows. If this row is not a list element, does nothing"
  (interactive)
  (when (scala-indent:list-element-line-p)
    (goto-char (1+ (nth 1 (syntax-ppss))))
    (let ((block-beg (point)))
      (forward-comment (buffer-size))
      (if (= (line-number-at-pos (point))
             (line-number-at-pos block-beg))
          ;; on same line as block start
          (progn (goto-char block-beg)
                 (skip-syntax-forward " "))
        ;; on different line
        (back-to-indentation)))))
    
