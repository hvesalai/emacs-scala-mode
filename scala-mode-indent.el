;;; scala-mode.el - Major mode for editing scala, indenting
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

(provide 'scala-mode-indent)

(require 'scala-mode-syntax)
(require 'scala-mode-lib)

(defcustom scala-indent:step 2
  "The number of spaces an indentation step should be. The actual
indentation will be one or two steps depending on context."
  :type 'integer
  :group 'scala)

(defun scala-indent:run-on-p (&optional point) 
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

(defun scala-indent:goto-run-on-anchor (&optional point)
  "Moves back to the point whose column will be used as the
anchor relative to which indenting for currnet point (or point
'point') is calculated. Returns the new point or nil if point is
not on a run-on line."
  (if (not (scala-indent:run-on-p point))
      nil
    (when point (goto-char point))
    (scala-syntax:beginning-of-code-line)
    (let ((block-beg (1+ (or (nth 1 (syntax-ppss)) (1- (point-min))))))
      (while (and (scala-indent:run-on-p)
                  (> (point) block-beg))
        ;; move back all parameter groups, if any
        (scala-syntax:beginning-of-code-line)
        (scala-syntax:skip-backward-ignorable)
        (scala-syntax:backward-parameter-groups)))
    (back-to-indentation)
;;;      (when (< (point) block-beg)
;;;          (goto-char block-beg)))
    (point)))

(defun scala-indent:list-element-line-p (&optional point)
  "Returns t if the current point (or point 'point') is in a
list. A list is something that begins with '(' or '[', or 'for
{'. A list element is preceded by ,"
  (save-excursion
    ;; first check that the previous line ended with ','
    (when point (goto-char point))
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

(defun scala-indent:goto-list-anchor (&optional point)
  "Moves back to the point whose column will be used to indent
list rows at current point (or point 'point'). Returns the new
point or nil if the point is not in a list element > 1."
  (if (not (scala-indent:list-element-line-p point))
      nil
    (goto-char (1+ (nth 1 (syntax-ppss point))))
    (let ((block-beg (point)))
      (forward-comment (buffer-size))
      (if (= (line-number-at-pos (point))
             (line-number-at-pos block-beg))
          ;; on same line as block start
          (progn (goto-char block-beg)
                 (skip-syntax-forward " "))
        ;; on different line
        (back-to-indentation))
      (point))))

(defun scala-indent:body-p (&optional point)
  "Return t if current point (or point 'point) is on a line
that follows = or => (or it's unicode equivalent)"
  (save-excursion
    (when point (goto-char point))
    (beginning-of-line)
    (scala-syntax:looking-back-token scala-syntax:body-start-re 2)))

(defun scala-indent:goto-body-anchor (&optional point)
  (if (not (scala-indent:body-p point))
      nil
    (when point (goto-char point))
    (beginning-of-line)
    (goto-char (or (nth 1 (syntax-ppss point)) (point-min)))
    (beginning-of-line)
    (point)))

(defun scala-indent:goto-block-anchor (&optional point)
  "Moves back to the point whose column will be used as the
anchor for calculating block indent for current point (or point
'point'). Returns point or nil, if not inside a block."
  (let ((block-beg (nth 1 (syntax-ppss point))))
    (if (not block-beg)
        nil
      (scala-indent:goto-run-on-anchor block-beg))))

(defun scala-indent:parentheses-line-p (&optional point)
  ""
  (save-excursion
    (when point (goto-char point))    
    (scala-syntax:beginning-of-code-line)
    (= (char-syntax (char-after)) ?\()))

(defun scala-indent:parentheses-anchor (&optional point)
  "Moves back to the point whose column will be used as the
anchor for calculating opening parenthesis indent for the current
point (or point 'point'). Returns point or nil, if line does not
start with opening parenthesis."
  (if (not (scala-indent:parentheses-line-p point))
      nil
    (scala-indent:goto-run-on-anchor point)))

(defun scala-indent:apply-indent-rules (rule-indents &optional point)
  "Evaluates each rule, until one returns non-nil value. Returns
the sum of the value and the respective indent step, or nil if
nothing was applied."
  (if (not rule-indents)
      nil
    (save-excursion
      (let* ((rule-indent (car rule-indents))
             (rule (car rule-indent))
             (indent (cadr rule-indent))
             (anchor (funcall rule point)))
        (if anchor
            (+ (current-column) (eval indent))
          (scala-indent:apply-indent-rules (cdr rule-indents)))))))

(defun scala-indent:calculate-indent-for-line (&optional point)
  "Calculate the appropriate indent for the current point or the
point 'point'"
  (or (scala-indent:apply-indent-rules
       `((scala-indent:parentheses-anchor 0)
         (scala-indent:goto-run-on-anchor (* 2 scala-indent:step))
         (scala-indent:goto-list-anchor 0)
         (scala-indent:goto-body-anchor scala-indent:step)
         (scala-indent:goto-block-anchor scala-indent:step))
       point)
      0))

(defun scala-indent:indent-line-to (column)
  "Indent the line to column and move cursor to the indent
column, if it was at the left margin."
  (if (<= (current-column) (current-indentation))
      (indent-line-to column)
    (save-excursion (indent-line-to column))))
    
(defun scala-indent:indent-line ()
  "Indents the current line."
  (interactive)
  ;; TODO: do nothing if inside string or comment
  (let ((indent (scala-indent:calculate-indent-for-line)))
    (when indent
      (scala-indent:indent-line-to indent))))
  
