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

(defun scala-indent:backward-sexp-to-beginning-of-line ()
  "Skip sexps backwards until reaches beginning of line (i.e. the
point is at the first non whitespace or comment character). It
does not move outside enclosin list. Returns the current point or
nil if the beginnig of line could not be reached because of
enclosing list."
  (let ((code-beg (scala-lib:point-after 
                   (scala-syntax:beginning-of-code-line))))
    (ignore-errors 
      (while (> (point) code-beg)
        (scala-syntax:backward-sexp)
        (when (< (point) code-beg) 
          ;; moved to previous line, set new target
          (setq code-beg (scala-lib:point-after 
                          (scala-syntax:beginning-of-code-line))))))
    (if (> (point) code-beg)
        nil
      (point))))

(defun scala-indent:run-on-p (&optional point) 
  (interactive)
  "Returns t if the current point (or point at 'point) is on a
line that is a run-on from a previous line."
  (save-excursion
    (when point (goto-char point))
    (scala-syntax:beginning-of-code-line)
    (if (looking-at scala-syntax:mustNotContinue-re)
        nil
      (scala-syntax:skip-backward-ignorable)
      (not (or (bobp)
               (scala-syntax:looking-back-empty-line-p)
               (scala-syntax:looking-back-token 
                scala-syntax:mustTerminate-re))))))

;    (defconst scala-syntax:mustNotTerminate-re
;      scala-syntax:reserved-symbols-unsafe-re

(defun scala-indent:goto-run-on-anchor (&optional point)
  "Moves back to the point whose column will be used as the
anchor relative to which indenting for current point (or point
'point') is calculated. Returns the new point or nil if the point
is not on a run-on line."
  (if (not (scala-indent:run-on-p point))
      nil
    (when point (goto-char point))
    (scala-syntax:beginning-of-code-line)
    (while (and (scala-indent:run-on-p)
                (scala-syntax:skip-backward-ignorable)
                (scala-indent:backward-sexp-to-beginning-of-line)))
    (point)))

(defun scala-indent:list-p (&optional point)
  "Returns the start of the list, if the current point (or point
'point') is in a list, or nil. A list must be either enclosed in
parentheses or start with 'val', 'var' or 'import'."
  (save-excursion
    ;; first check that the previous line ended with ','
    (when point (goto-char point))
    (scala-syntax:beginning-of-code-line)
    (if (not (scala-syntax:looking-back-token "," 1))
        nil
      (goto-char (match-beginning 0))
      (ignore-errors ; catches when we get at parentheses
        (while (not (or (bobp)
                        (looking-at scala-syntax:list-keywords-re)
                        (scala-syntax:looking-back-empty-line-p)
                        (scala-syntax:looking-back-token ";")))
          (scala-syntax:backward-sexp)))
      (cond ((= (char-syntax (char-before)) ?\()
             (point))
            ((looking-at scala-syntax:list-keywords-re)
             (goto-char (match-end 0)))))))

(defun scala-indent:goto-list-anchor (&optional point)
  "Moves back to the point whose column will be used to indent
list rows at current point (or point 'point'). Returns the new
point or nil if the point is not in a list element > 1."
  (let ((list-beg (scala-indent:list-p (point))))
    (if (not list-beg)
        nil
      (goto-char list-beg)
      ;; find the first element of the list
      (forward-comment (buffer-size))
      (if (= (line-number-at-pos list-beg) 
             (line-number-at-pos))
          (goto-char list-beg)
        (beginning-of-line))

      ;; align list with first non-whitespace character
      (skip-syntax-forward " "))))

(defun scala-indent:body-p (&optional point)
  "Returns the position of equals or double arrow symbol if
current point (or point 'point) is on a line that follows = or
=> (or it's unicode equivalent), or nil if not."
  (save-excursion
    (when point (goto-char point))
    (scala-syntax:beginning-of-code-line)
    (scala-syntax:looking-back-token scala-syntax:body-start-re 3)))

(defun scala-indent:goto-body-anchor (&optional point)
  (let ((declaration-end (scala-indent:body-p point)))
    (if (not declaration-end)
        nil
      (goto-char declaration-end)
      (when (scala-indent:backward-sexp-to-beginning-of-line)
        (scala-indent:goto-run-on-anchor))
      (point))))

(defun scala-indent:goto-block-anchor (&optional point)
  "Moves back to the point whose column will be used as the
anchor for calculating block indent for current point (or point
'point'). Returns point or (point-min) if not inside a block." 
  (let ((block-beg (nth 1 (syntax-ppss 
                           (scala-lib:point-after (beginning-of-line))))))
    (if (not block-beg)
        nil
      ;; check if the opening paren is the first on the line,
      ;; if so, it is the anchor. If not, then go back to the
      ;; start of the line
      (goto-char block-beg)
      (scala-syntax:backward-parameter-groups)
      (if (= (point) (scala-lib:point-after
                      (scala-syntax:beginning-of-code-line)))
          (point)
        (when (scala-indent:backward-sexp-to-beginning-of-line)
          (scala-indent:goto-run-on-anchor))
        (point)))))           

(defun scala-indent:open-parentheses-line-p (&optional point)
  "Returns the position of the first character of the line,
if the current point (or point 'point') is on a line that starts
with a parentheses, or nil if not."
  (save-excursion
    (when point (goto-char point))    
    (scala-syntax:beginning-of-code-line)
    (if (looking-at "\\s(") (point) nil)))

(defun scala-indent:goto-open-parentheses-anchor (&optional point)
  "Moves back to the point whose column will be used as the
anchor for calculating opening parenthesis indent for the current
point (or point 'point'). Returns point or nil, if line does not
start with opening parenthesis."
  ;; There are four cases we need to consider:
  ;; 1. parameters on separate line (who would be so mad?).
  ;; 2. curry parentheses, i.e. 2..n parentheses groups.
  ;; 3. value body parentheses (follows '=').
  ;; 4. non-value body parentheses (follows class, trait, new, def, etc).
  ;; Of these 1. will be handled by run-on indent rule and 3. should
  ;; be handled by body indent rule.
  (let ((parentheses-beg (scala-indent:open-parentheses-line-p point)))
    (if (not parentheses-beg)
        nil
      (goto-char parentheses-beg)
      (cond 
       ;; case 2
       ((scala-syntax:looking-back-token "[])]" 1)
        (scala-syntax:backward-parameter-groups)
        (point))
       ;; case 4
       ((and (= (char-after) ?\{)
             (not (scala-syntax:looking-back-token "=" 1)))
        (scala-indent:goto-run-on-anchor)
        (point))))))

(defun scala-indent:apply-indent-rules (rule-indents &optional point)
  "Evaluates each rule, until one returns non-nil value. Returns
the sum of the value and the respective indent step, or nil if
nothing was applied."
  (if (not rule-indents)
      nil
    (save-excursion
      (let* ((pos (scala-syntax:beginning-of-code-line))
             (rule-indent (car rule-indents))
             (rule-statement (car rule-indent))
             (indent-statement (cadr rule-indent))
             (anchor (funcall rule-statement point))
             (indent (if (functionp indent-statement)
                         (funcall indent-statement pos anchor) 
                       (eval indent-statement))))
        (if anchor
            (+ (current-column) indent)
          (scala-indent:apply-indent-rules (cdr rule-indents)))))))

(defun scala-indent:resolve-block-step (start anchor)
  "Resolves the appropriate indent step for block line at position
'start' relative to the block anchor 'anchor'."
  (if (= (char-syntax (char-after start)) ?\))
      0 ;; block close parentheses line up with anchor
    ;; TODO: case blocks
    scala-indent:step))
    
(defun scala-indent:calculate-indent-for-line (&optional point)
  "Calculate the appropriate indent for the current point or the
point 'point'. Returns the new column, or nil if the indent
cannot be determined."
  (or (scala-indent:apply-indent-rules
       `((scala-indent:goto-open-parentheses-anchor 0)
         (scala-indent:goto-run-on-anchor (* 2 scala-indent:step))
         (scala-indent:goto-list-anchor 0)
         (scala-indent:goto-body-anchor scala-indent:step)
         (scala-indent:goto-block-anchor scala-indent:resolve-block-step)
     )
       point)
      0))

(defun scala-indent:indent-line-to (column)
  "Indent the line to column and move cursor to the indent
column, if it was at the left margin."
  (if (<= (current-column) (current-indentation))
      (indent-line-to column)
    (save-excursion (indent-line-to column))))

(defun scala-indent:indent-code-line ()
  "Indent a line of code. Expect to be outside of any comments or
strings"
  (let ((indent (scala-indent:calculate-indent-for-line)))
    (if indent
        (scala-indent:indent-line-to indent)
      (message "No indent rule for current line"))))
    
(defun scala-indent:indent-line ()
  "Indents the current line."
  (interactive)
  (let ((state (save-excursion (syntax-ppss (line-beginning-position)))))
    (if (not (nth 8 state)) ;; 8 = start pos of comment or string, nil if none
        (scala-indent:indent-code-line)
      (scala-indent:indent-line-to (current-indentation))
      nil
      )))
