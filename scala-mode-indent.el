;;; scala-mode.el - Major mode for editing scala, indenting
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

(require 'scala-mode-syntax)
(require 'scala-mode-lib)

(eval-when-compile
  (defvar scala-indent:effective-run-on-strategy)
  (defvar scala-indent:previous-indent-pos))

(defcustom scala-indent:step 2
  "The number of spaces an indentation step should be. The actual
indentation will be one or two steps depending on context."
  :type 'integer
  :safe #'integerp
  :group 'scala)

(defcustom scala-indent:indent-value-expression nil
  "Whether or not to indent multi-line value expressions, with
one extra step. When true, indenting will be

val x = try {
    some()
  } catch {
    case e => other
  } finally {
    clean-up()
  }

When nil, the same will indent as

val x = try {
  some()
} catch {
  case e => other
} finally {
  clean-up()
}
"
  :type 'boolean
  :group 'scala)

(defcustom scala-indent:align-parameters nil
  "Whether or not to indent parameter lists so that next
  parameter lines always align under the first parameter. When
  non-nil, indentation will be

def foo(x: Int, y: List[Int]
        z: Int)

val x = foo(1, List(1, 2, 3) map (i =>
              i + 1
            ), 2)

When nil, the same will indent as

def foo(x: Int, y: List[Int]
        z: Int)

val x = foo(1, List(1, 2, 3) map (i =>
    i + 1
  ), 2)
"
  :type 'boolean
  :safe #'booleanp
  :group 'scala)

(defcustom scala-indent:align-forms nil
  "Whether or not to align 'else', 'yield', 'catch', 'finally'
below their respective expression start. When non-nil, identing
will be

val x = if (foo)
          bar
        else
          zot

when nil, the same will indent as

val x = if (foo)
    bar
  else
    zot
"
  :type 'boolean
  :group 'scala)

(defconst scala-indent:eager-strategy 0
  "See `scala-indent:run-on-strategy'")
(defconst scala-indent:operator-strategy 1
  "See `scala-indent:run-on-strategy'")
(defconst scala-indent:reluctant-strategy 2
  "See `scala-indent:run-on-strategy'")
(defconst scala-indent:keywords-only-strategy 3
  "A strategy used internally by indent engine")

(defcustom scala-indent:default-run-on-strategy 2
  "What strategy to use for detecting run-on lines, i.e. lines
that continue a statement from the previous line. Possible values
are:

'reluctant', which marks only lines that begin with -- or
that follow a line that ends with -- a reserved word that cannot start
or end a line, such as 'with'.

'operators', which extends the previous strategy by marking also
lines that begin with -- or that follow a line that ends with --
an operator character. For example, '+', '-', etc.

'eager', which marks all rows which could be run-ons, i.e. which
are not ruled out by the language specification.
"
  :type `(choice (const :tag "eager" ,scala-indent:eager-strategy)
                 (const :tag "operators" ,scala-indent:operator-strategy)
                 (const :tag "reluctant" ,scala-indent:reluctant-strategy))
  :group 'scala)

(make-variable-buffer-local 'scala-indent:effective-run-on-strategy)

(defcustom scala-indent:add-space-for-scaladoc-asterisk t
  "When non-nil, a space will be added after a scaladoc asterisk,
when it is added to an empty line."
  :type 'boolean
  :safe #'booleanp
  :group 'scala)

(defcustom scala-indent:use-javadoc-style nil
  "When non-nil, multi-line comments are indented according to Javadoc
style (i.e. indented to the first asterisk). This overrides the
Scaladoc behavior of indenting comment lines to the second asterisk."
  :type 'boolean
  :safe #'booleanp
  :group 'scala)

(defun scala-indent:run-on-strategy ()
  "Returns the currently effecti run-on strategy"
  (or scala-indent:effective-run-on-strategy
      scala-indent:default-run-on-strategy
      scala-indent:eager-strategy))

(defun scala-indent:toggle-effective-run-on-strategy ()
  "If effective run-on strategy is not set, it is set as follows:
- if default is eager or operators, then it is set to reluctant
- if default is reluctant, then it is set to eager. If it is set,
it is nilled."
  (if scala-indent:effective-run-on-strategy
      (setq scala-indent:effective-run-on-strategy nil)
    (let ((new-strategy
           (cond ((= (scala-indent:run-on-strategy)
                     scala-indent:reluctant-strategy)
                  scala-indent:eager-strategy)
                 ((or (= (scala-indent:run-on-strategy)
                         scala-indent:operator-strategy)
                      (= (scala-indent:run-on-strategy)
                         scala-indent:eager-strategy))
                  scala-indent:reluctant-strategy))))
      (setq scala-indent:effective-run-on-strategy new-strategy))))

(defun scala-indent:reset-effective-run-on-strategy ()
  (setq scala-indent:effective-run-on-strategy nil))

(defun scala-indent:rotate-run-on-strategy ()
  (interactive)
  (let ((new-strategy
         (cond ((= scala-indent:default-run-on-strategy
                         scala-indent:reluctant-strategy)
                scala-indent:operator-strategy)
               ((= scala-indent:default-run-on-strategy
                         scala-indent:operator-strategy)
                scala-indent:eager-strategy)
               ((= scala-indent:default-run-on-strategy
                         scala-indent:eager-strategy)
                scala-indent:reluctant-strategy))))
    (setq scala-indent:default-run-on-strategy new-strategy)
;    (message "scala-indent:default-run-on-strategy set to %s" scala-indent:default-run-on-strategy)
    ))

(defun scala-indent:backward-sexp-to-beginning-of-line ()
  "Skip sexps backwards until reaches beginning of line (i.e. the
point is at the first non whitespace or comment character). It
does not move outside enclosing list. Returns the current point or
nil if the beginning of line could not be reached because of
enclosing list."
  (let ((code-beg (scala-lib:point-after
                   (scala-syntax:beginning-of-code-line))))
    (ignore-errors
      (while (> (point) code-beg)
        (scala-syntax:backward-sexp)
	(skip-syntax-backward ".")
        (when (< (point) code-beg)
          ;; moved to previous line, set new target
          (setq code-beg (scala-lib:point-after
                          (scala-syntax:beginning-of-code-line))))))
    (unless (> (point) code-beg)
      (point))))

(defun scala-indent:align-anchor ()
  "Go to beginning of line, if a) `scala-indent:align-parameters' is nil or
`scala-indent:backward-sexp-to-beginning-of-line' is non-nil. This has the
effect of staying within lists if `scala-indent:align-parameters' is non-nil."
  (when (or (scala-indent:backward-sexp-to-beginning-of-line)
            (not scala-indent:align-parameters))
    (back-to-indentation)))

(defun scala-indent:value-expression-lead (start anchor &optional not-block-p)
  ;; calculate an indent lead. The lead is one indent step if there is a '='
  ;; between anchor and start, otherwise 0.
  (if (and scala-indent:indent-value-expression
           (ignore-errors
             (save-excursion
               (let ((block-beg (if not-block-p
                                    start
                                  (nth 1 (syntax-ppss start)))))
                 (goto-char anchor)
                 (scala-syntax:has-char-before ?= block-beg)))))
      scala-indent:step 0))

;;;
;;; Run-on
;;;

(defconst scala-indent:mustNotTerminate-keywords-re
  (regexp-opt '("extends" "match" "with") 'words)
  "Some keywords which occur only in the middle of an expression")

(defconst scala-indent:mustNotTerminate-line-beginning-re
  (concat "\\(" scala-indent:mustNotTerminate-keywords-re
          "\\|:\\("  scala-syntax:after-reserved-symbol-re "\\)\\)")
  "All keywords and symbols that cannot terminate an expression
and must be handled by run-on. Reserved-symbols not included.")

(defconst scala-indent:mustTerminate-re
  (concat "\\([,;]\\|=>?" scala-syntax:end-of-code-line-re
          "\\|\\s(\\|" scala-syntax:empty-line-re "\\)")
  "Symbols that must terminate an expression or start a
sub-expression, i.e the following expression cannot be a
run-on. This includes only parenthesis, '=', '=>', ',' and ';'
and the empty line")

(defconst scala-indent:mustNotContinue-re
  (regexp-opt '("abstract" "catch" "case" "class" "def" "do" "else" "final"
                "finally" "for" "if" "implicit" "import" "lazy" "new" "object"
                "override" "package" "private" "protected" "return" "sealed"
                "throw" "trait" "try" "type" "val" "var" "while" "yield" "inline")
              'words)
  "Words that we don't want to continue the previous line")

(defconst scala-indent:mustBeContinued-line-end-re
  (concat "\\(" scala-syntax:other-keywords-unsafe-re
          "\\|:" scala-syntax:end-of-code-line-re "\\)")
  "All keywords and symbols that cannot terminate an expression
and are in fact a sign of run-on. Reserved-symbols not included.")

(defun scala-indent:run-on-p (&optional point strategy)
  "Returns t if the current point is in the middle of an expression"
  ;; use default strategy if none given
  (when (not strategy) (setq strategy (scala-indent:run-on-strategy)))
  (save-excursion
    (when point (goto-char point))
    (unless (eobp)
      ;; NOTE: of course this 'cond' could be written as one big boolean
      ;; expression, but I doubt that would be so readable and maintainable
      (cond
       ;; NO: this line starts with close parenthesis
       ((= (char-syntax (char-after)) ?\))
        nil)
       ;; NO: the previous line must terminate
       ((save-excursion
          (scala-syntax:skip-backward-ignorable)
          (or (bobp)
              (scala-syntax:looking-back-empty-line-p)
              (scala-syntax:looking-back-token scala-indent:mustTerminate-re)))
        nil)
       ;; YES: in a region where newlines are disabled
       ((and (scala-syntax:newlines-disabled-p)
             (not (= strategy scala-indent:keywords-only-strategy)))
        t)
       ;; NO: this line starts with a keyword that starts a new
       ;; expression (e.g. 'def' or 'class')
       ((looking-at scala-indent:mustNotContinue-re)
        nil)
       ;; NO: this line is the start of value body
       ((scala-indent:body-p)
        nil)
       ;; YES: eager strategy can stop here, everything is a run-on if no
       ;; counter evidence
       ((= strategy scala-indent:eager-strategy)
        t)
       ;; YES: this line must not terminate because it starts with a
       ;; middle of expression keyword
       ((looking-at scala-indent:mustNotTerminate-line-beginning-re)
        t)
       ;; YES: end of prev line must not terminate
       ((let ((case-fold-search nil))
          (scala-syntax:looking-back-token
         scala-indent:mustBeContinued-line-end-re))
        t)
       ;; YES: this line starts with type param
       ((= (char-after) ?\[)
        t)
       ;; YES: this line starts with open paren and the expression
       ;; after all parens is a run-on
       ((and (= (char-after) ?\()
             (save-excursion (scala-syntax:forward-parameter-groups)
                             (scala-syntax:skip-forward-ignorable)
                             (or (= (char-after) ?=)
                                 (= (char-after) ?{)
                                 (scala-indent:run-on-p nil strategy))))
        t)
       ;; NO: that's all for keywords-only strategy
       ((= strategy scala-indent:keywords-only-strategy)
        nil)
       ;; YES: this line starts with punctuation
       ((= (char-after) ?\.)
        t)
       ;; YES: prev line ended with punctuation
       ((scala-syntax:looking-back-token ".*[.]")
        t)
       ;; NO: that's all for reluctant-strategy
       ((= strategy scala-indent:reluctant-strategy)
        nil)
       ;; YES: this line starts with opchars
       ((save-excursion
          (< 0 (skip-chars-forward scala-syntax:opchar-group)))
        t)
       ;; YES: prev line ends with opchars
       ((save-excursion
          (scala-syntax:skip-backward-ignorable)
          (> 0 (skip-chars-backward scala-syntax:opchar-group)))
        t)
       ;; NO: else nil (only operator strategy should reach here)
       (t nil)))))

(defun scala-indent:run-on-line-p (&optional point strategy)
  "Returns t if the current point (or point at 'point) is on a
line that is a run-on from a previous line."
  (save-excursion
    (when point (goto-char point))
    (scala-syntax:beginning-of-code-line)
    (scala-indent:run-on-p nil strategy)))

(defun scala-indent:goto-run-on-anchor (&optional point strategy)
  "Moves back to the point whose column will be used as the
anchor relative to which indenting for current point (or point
'point') is calculated. Returns the new point or nil if the point
is not on a run-on line."
  (when (scala-indent:run-on-line-p point strategy)
    (when point (goto-char point))
    (scala-syntax:beginning-of-code-line)
    (while (and (scala-indent:run-on-line-p nil strategy)
                (scala-syntax:skip-backward-ignorable)
                (scala-indent:backward-sexp-to-beginning-of-line)))
    (scala-indent:align-anchor)
    (point)))

(defconst scala-indent:double-indent-re
  ;; used to include with but given...with is a counterexample
  (concat (regexp-opt '("extends" "forSome") 'words)
          "\\|:\\("  scala-syntax:after-reserved-symbol-re "\\)"))

(defconst scala-indent:forms-align-re
  (regexp-opt '("yield" "then" "else" "catch" "finally") 'words))

(defun scala-indent:forms-align-p (&optional point)
  "Returns `scala-syntax:beginning-of-code-line' for the line on
which current point (or point 'point') is, if the line starts
with one of 'yield', 'then', 'else', 'catch' and 'finally', otherwise
nil. Also, the previous line must not be with '}'"
  (save-excursion
    (when point (goto-char point))
    (scala-syntax:beginning-of-code-line)
    (when (looking-at scala-indent:forms-align-re)
      (goto-char (match-beginning 0))
      (point))))

(defun scala-indent:for-enumerators-p (&optional point)
  "Returns the point after opening parentheses if the current
point (or point 'point') is in a block of enumerators. Return nil
if not in a list of enumerators or at the first enumerator."
  (unless point (setq point (point)))
  (save-excursion
    (goto-char point)
    (scala-syntax:beginning-of-code-line)
    (let ((state (syntax-ppss point)))
      (unless (or (eobp) (= (char-syntax (char-after)) ?\)))
        (when (and state (nth 1 state))
          (goto-char (nth 1 state))
          (when (scala-syntax:looking-back-token scala-syntax:for-re)
            (forward-char)
            (forward-comment (buffer-size))
            (when (< (point) point)
              (1+ (nth 1 state)))))))))

;;;
;;; Block
;;;

(defun scala-indent:goto-block-anchor (&optional point)
  "Moves back to the point whose column will be used as the
anchor for calculating block indent for current point (or POINT).
Returns point or (point-min) if not inside a block."
  (when-let ((block-beg (nth 1 (syntax-ppss
                                (scala-lib:point-after (beginning-of-line))))))
    ;; Check if the opening paren is the first on the line, if so, it is the
    ;; anchor. If not, then go back to the start of the line
    (goto-char block-beg)
    (if (= (point) (scala-lib:point-after
                    (scala-syntax:beginning-of-code-line)))
        (point)
      (goto-char (or (scala-syntax:looking-back-token
                      scala-syntax:body-start-re 3)
                     (point)))
      (scala-syntax:backward-parameter-groups)
      (when (scala-indent:backward-sexp-to-beginning-of-line)
        (scala-indent:goto-run-on-anchor nil
                                         scala-indent:keywords-only-strategy))
      (scala-indent:align-anchor)
      (point))))

(defun scala-indent:analyze-syntax-stack (stack)
  "TODO document"
  (pcase stack
    ('(?\n ?.) 'dot-chain)
    ('(final) 'decl)
    ('(override) 'decl)
    ('(class) 'decl)
    ('(object) 'decl)
    ('(implicit) 'decl)
    ('(while) 'decl)
    ('(enum) 'decl)
    ('(trait) 'decl)
    (`(if . ,_) 'if)
    (`(then . ,_) 'then)
    (`(else . ,_) 'else)
    (`(for) 'for-comp)
    (`(for . ,_) 'for-body)
    (`(yield . ,_) 'yield-from-comp)
    (`(<- . ,_) 'generator)
    (`(def . ,_) 'decl)
    (`(val . ,_) 'decl)
    (`(var . ,_) 'decl)
    (`(given . ,_) 'decl)
    (`(,_ match) 'match)
    ('(with) 'block)
    (`(do . ,_) 'block)
    ((and `(class . ,tail) (guard (memq ': tail))) 'block)
    (`(case . ,_) 'case)
    (`(= ,_ . ,_) 'decl-lhs)
    (`(=> . ,_) 'arrow-lhs)
    ((and `(enum . ,tail) (guard (memq ': tail))) 'block)
    ((and `(trait . ,tail) (guard (memq ': tail))) 'block)
    ((and `(object . ,tail) (guard (memq ': tail))) 'block)))

(defun scala-indent:relative-indent-by-elem (syntax-elem)
  "TODO document"
  (pcase syntax-elem
    (`(dedented decl . ,_) :maintain)
    ('(dedented) 2)
    ('(dedented then) 0)
    ('(dedented else) 0)
    ('(dedented dot-chain) 4)
    ('(decl-lhs dot-chain) 4)
    (`(decl-lhs decl . ,_) 0)
    (`(decl-lhs for-comp) 0)
    (`(decl-lhs generator) 0) ;; TODO for comprehensions are problematic
    ('(for-comp yield-from-comp) 0)
    ('(dedented yield-from-comp) :maintain)
    ('(decl-lhs yield-from-comp) -2)
    (`(for-body . ,_) 2)
    ('(if then) 0)
    ('(then else) 0)
    (`(decl-lhs) 2)
    (`(decl-lhs . ,_) 0)
    (`(match case . ,_) 2)
    ('(arrow-lhs) 2)
    ('(block) 2)
    (`(decl . ,_) 2)
    ('(generator yield-from-comp) -2)
    (`(generator . ,_) 0)
    (`(block . ,_) 2)
    ('(case case) 0) ;; e.g. in enums
    (`(arrow-lhs case . ,_) 0) ;; within match
    ))

(defun scala-indent:find-analysis-start (&optional point)
  "TODO document"
  (save-excursion
    (when point (goto-char point))
    (let (stack)
      ;; Always look at a token on the current for starters
      (when (> (current-indentation) (current-column))
        (scala-syntax:forward-token)
        (setq stack (cons ?\n stack)))
      (if (= (line-beginning-position) (line-end-position))
          ;; Handle blank lines
          (progn
            (scala-syntax:backward-sexp-forcing)
            (setq stack (cons ?\n stack)))
        ;; (beginning-of-thing 'sexp) gets confused by `.'
        (unless (looking-at-p "\\.")
          ;; Avoid double-reading curent symbol
          (beginning-of-thing 'sexp)))
      (list (point)
            (current-indentation)
            stack))))

(defun scala-indent:analyze-context (orig-indent point &optional init-stack)
  "TODO document"
  (save-excursion
    (goto-char point)
    (let (result
          (stack init-stack))
      ;; TODO probably want to bound this much more tightly than the beginning
      ;; of the buffer. This means worse case performance could be bad.
      (while (and (not result) (> (point) 1))
        (if (< orig-indent (current-indentation))
            (setq result 'dedented)
          (setq stack
                (cons (if (looking-at-p "\\.")
                          ?.
                        (sexp-at-point))
                      stack))
          ;(message (format "stack: %s" stack))
          (setq result
                (scala-indent:analyze-syntax-stack stack))
          ;(message (format "result: %s" result))
          (when (and (not result)
                     (save-excursion (= (point)
                                        (scala-syntax:beginning-of-code-line))))
            (setq stack (cons ?\n stack))
            ;(message (format "stack: %s" stack))
            (setq result
                  (scala-indent:analyze-syntax-stack stack))
            ;(message (format "result: %s" result))
            (when result
              (scala-syntax:backward-sexp-forcing)))
          (unless result
            (while (looking-at-p "\\.") (backward-char))
            (scala-syntax:backward-sexp-forcing))))
      (list result
            (line-number-at-pos)
            (current-indentation)
            (point)))))

(defun scala-indent:new-calculate-indent-for-line (&optional point)
  "TODO document"
  (let* ((initResult (scala-indent:find-analysis-start point))
         (point (car initResult))
         (orig-indent (cadr initResult))
         (stack (caddr initResult))
         (line-no (line-number-at-pos point))
         (analysis (scala-indent:analyze-context orig-indent point stack))
         (syntax-elem (list (car analysis)))
         (ctxt-line (cadr analysis))
         (ctxt-indent (caddr analysis))
         (stopped-point (cadddr analysis)))
    ;(message "analysis: %s" analysis)
    (while (and (= ctxt-line line-no) (> line-no 1))
      (setq analysis
            (scala-indent:analyze-context
             orig-indent
             (save-excursion
               (goto-char stopped-point)
               (scala-syntax:backward-sexp-forcing)
               (point))))
      ;(message "analysis: %s" analysis)
      (setq syntax-elem (cons (car analysis) syntax-elem))
      (setq ctxt-line (cadr analysis))
      (setq ctxt-indent (caddr analysis))
      (setq stopped-point (cadddr analysis)))
    ;(message "syntax-elem: %s" syntax-elem)
    (when-let ((_ (< ctxt-line line-no))
               (relative (scala-indent:relative-indent-by-elem syntax-elem)))
      (if (eq :maintain relative)
          (current-indentation)
        (+ ctxt-indent relative)))))

(defun scala-indent:resolve-block-step (start anchor)
  "Resolves the appropriate indent step for block line at position
'start' relative to the block anchor 'anchor'."
  (let
      ((lead (scala-indent:value-expression-lead start anchor)))
    (cond
     ;; at end of buffer
     ((= start (point-max)) (+ scala-indent:step lead))
     ;; block close parentheses line up with anchor in normal case
     ((= (char-syntax (char-after start)) ?\))
      (+ 0 lead))
     ;; case-lines indent normally, regardless of where they are
     ((scala-syntax:looking-at-case-p start)
      (+ scala-indent:step lead))
     ;; other than case-line in case-block get double indent
     ((save-excursion
        (goto-char (1+ (or (nth 1 (syntax-ppss start)) 0)))
        (forward-comment (buffer-size))
        (and (scala-syntax:looking-at-case-p)
             (> (line-number-at-pos) (line-number-at-pos anchor))
             (> start (match-beginning 0))))
      (+ (* 2 scala-indent:step) lead))
     ;; normal block line
     (t  (+ scala-indent:step lead)))))

;;;
;;; Indentation engine
;;;

(defun scala-indent:apply-indent-rules (rule-indents &optional point)
  "Evaluates each rule, until one returns non-nil value. Returns
the sum of the value and the respective indent step, or nil if
nothing was applied."
  (when rule-indents
    (save-excursion
      (when point (goto-char point))
      (let* ((pos (scala-syntax:beginning-of-code-line))
             (rule-indent (car rule-indents))
             (rule-statement (car rule-indent))
             (indent-statement (cadr rule-indent))
             (anchor (funcall rule-statement point)))
        (if anchor
            (progn
              (if scala-mode:debug-messages
                  (message "indenting acording to %s at %d for pos %d for point %s" rule-statement anchor pos point))
              (when (/= anchor (point))
                (error (format "Assertion error: anchor=%d, point=%d" anchor (point))))
              (+ (current-column)
                 (save-excursion
                   (if (functionp indent-statement)
                       (funcall indent-statement pos anchor)
                     (eval indent-statement)))))
          (scala-indent:apply-indent-rules (cdr rule-indents)))))))

(defun scala-indent:calculate-indent-for-line (&optional point)
  "Calculate the appropriate indent for the current point or POINT.

Returns the new column, or nil if the indent cannot be determined."
  (or
   (scala-indent:apply-indent-rules
    `((scala-indent:goto-block-anchor scala-indent:resolve-block-step)
      )
    point)
   (scala-indent:new-calculate-indent-for-line point)
   0))

(defun scala-indent:indent-line-to (column)
  "Indent the line to column and move cursor to the indent
column, if it was at the left margin."
  (when column
    (if (<= (current-column) (current-indentation))
        (indent-line-to column)
      (save-excursion (indent-line-to column)))))

(make-variable-buffer-local 'scala-indent:previous-indent-pos)

(defun scala-indent:remove-indent-from-previous-empty-line ()
  "Handles removing of whitespace from a previosly indented code
line that was left empty (i.e. whitespaces only). Also clears the
scala-indent:previous-indent-pos variable that controls the process."
  (when (and scala-indent:previous-indent-pos
             (/= scala-indent:previous-indent-pos (point)))
    (save-excursion
      (beginning-of-line)
      (if (= scala-indent:previous-indent-pos
             (point))
          (setq scala-indent:previous-indent-pos
                (when (looking-at "^\\s +$") (point)))
        (goto-char scala-indent:previous-indent-pos)
        (when (looking-at "^\\s +$")
          (delete-region (match-beginning 0) (match-end 0)))
        (setq scala-indent:previous-indent-pos nil)))))

(defun scala-indent:indent-code-line (&optional strategy)
  "Indent a line of code. Expect to be outside of any comments or
strings"
  (if strategy
      (setq scala-indent:effective-run-on-strategy strategy)
    (if (eq last-command this-command)
        (scala-indent:toggle-effective-run-on-strategy)
      (scala-indent:reset-effective-run-on-strategy)))
;  (message "run-on-strategy is %s" (scala-indent:run-on-strategy))
  (scala-indent:indent-line-to (scala-indent:calculate-indent-for-line))
  (scala-lib:delete-trailing-whitespace)
  (setq scala-indent:previous-indent-pos
        (save-excursion
          (beginning-of-line)
          (when (looking-at "^\\s +$") (point)))))

(defun scala-indent:indent-line (&optional strategy)
  "Indents the current line."
  (interactive "*")
  (let ((state (save-excursion (syntax-ppss (line-beginning-position)))))
    (if (nth 8 state) ;; 8 = start pos of comment or string
        (scala-indent:indent-line-to
         (cond ((integerp (nth 4 state))    ;; 4 = nesting level of multi-line comment
                (scala-indent:scaladoc-indent (nth 8 state)))
               ((eq t (nth 3 state))   ;; 3 = t for multi-line string
                (or (save-excursion
                      (beginning-of-line)
                      (when (and (looking-at "\\s *|")
                                 (progn (goto-char (nth 8 state))
                                        (looking-at "\\(\"\"\"\\)|")))
                        (goto-char (match-end 1))
                        (current-column)))
                    (current-indentation)))
               (t (current-indentation))))
      (scala-indent:indent-code-line strategy)))
  )

(defun scala-indent:indent-with-reluctant-strategy ()
  (interactive "*")
  (scala-indent:indent-line scala-indent:reluctant-strategy))

(defun scala-indent:scaladoc-indent (comment-start-pos)
  "Calculate indent for a multi-line comment. Scaladoc
lines (starting with /**) are indented under the second
aseterix. Other multi-line comment rows are indented undet the
first asterisk.

Note: start line is indented as code since the start of the
comment is outside the comment region. "
  (save-excursion
    (goto-char comment-start-pos)
    (when (looking-at "/\\*+")
      (goto-char
       (if (and (not scala-indent:use-javadoc-style)
                (= (- (match-end 0) (match-beginning 0)) 3))
           (- (match-end 0) 1)
         (+ (match-beginning 0) 1)))
      (current-column))))

(defun scala-indent:indent-on-parentheses ()
  (when (and (= (char-syntax (char-before)) ?\))
             (= (save-excursion (back-to-indentation) (point)) (1- (point))))
    (scala-indent:indent-line)))

(defconst scala-indent:indent-on-words-re
  (concat "^\\s *"
          (regexp-opt '("catch" "case" "then" "else" "finally" "yield") 'words)))

(defun scala-indent:indent-on-special-words ()
  "This function is meant to be used with post-self-insert-hook.

Indents the line if position is right after a space that is after
a word that needs to be indented specially."
  ;; magic numbers used 4 = length of "case", 7 = length of "finally"
  (when (and (> (current-column) 4)
             (= (char-before) ?\s)
             (= (char-syntax (char-before (- (point) 1))) ?w)
             (save-excursion (backward-char)
                             (looking-back scala-indent:indent-on-words-re 7))
             (not (nth 8 (syntax-ppss))))
    (scala-indent:indent-line-to (scala-indent:calculate-indent-for-line))))

(defun scala-indent:indent-on-scaladoc-asterisk ()
  "This function is meant to be used with post-self-insert-hook.

Indents the line if position is right after an asterisk in a
multi-line comment block and there is only whitespace before the asterisk.

If scala-indent:add-space-for-scaladoc-asterisk is t, also adds a
space after the asterisk if the asterisk is the last character on
the line."
  (let ((state (syntax-ppss)))
    (when (and (integerp (nth 4 state))
               (looking-back "^\\s *\\*" (line-beginning-position)))
      (when scala-indent:add-space-for-scaladoc-asterisk
        (insert " "))
      (scala-indent:indent-line-to (scala-indent:scaladoc-indent (nth 8 state))))))

(defun scala-indent:fix-scaladoc-close ()
  "This function is meant to be used with post-self-insert-hook.

Changes 'asterisk space slash' to 'asterisk slash' in a
multi-line comment if position is right after that slash and
scala-indent:add-space-for-scaladoc-asterisk is t."
  (let ((state (syntax-ppss)))
    (when (and scala-indent:add-space-for-scaladoc-asterisk
               (integerp (nth 4 state))
               (looking-back "^\\s *\\*\\s /" (line-beginning-position)))
      (delete-region (- (point) 2) (- (point) 1)))))

(defun scala-indent:insert-asterisk-on-multiline-comment ()
  "Insert an asterisk at the end of the current line when at the beginning
of a line inside a multi-line comment "
  (let* ((state (syntax-ppss))
         (comment-start-pos (nth 8 state)))
    (when (and (integerp (nth 4 state))
               ; Ensure that we're inside a scaladoc comment
               (string-match-p "^/\\*\\*[^\\*]"
                               (buffer-substring-no-properties
                                comment-start-pos
                                (+ comment-start-pos 4)))
               ; Ensure that the previous line had a leading asterisk or was the comment start.
               (let ((prev-line (buffer-substring-no-properties
                                 (line-beginning-position 0)
                                 (line-end-position 0))))
                 (or
                  (string-match-p "^\\s-*\\*" prev-line)
                  (string-match-p "\\s-*/\\*\\*" prev-line))))
      (skip-syntax-forward " ")
      (insert "*")
      (scala-indent:indent-on-scaladoc-asterisk))))

(defun scala-indent:fixup-whitespace ()
  "`scala-mode' version of `fixup-whitespace'"
  (interactive "*")
  (save-excursion
    (delete-horizontal-space)
    (if (or (looking-at "^\\|[]):.]")
	    (save-excursion (forward-char -1)
                            (if (nth 4 (syntax-ppss))
                                (looking-at "$\\|\\s(")
                              (looking-at "$\\|[[(.]")))
            (and (= (char-before) ?{) (= (char-after) ?})))
	nil
      (insert ?\s))))

(defun scala-indent:join-line (&optional arg)
  "scala-mode version of `join-line', i.e. `delete-indentation'"
  (interactive "*P")
  (beginning-of-line)
  (if arg (forward-line 1))
  (when (= (preceding-char) ?\n)
    (delete-region (point) (1- (point)))
    (delete-horizontal-space)
    (let ((state (syntax-ppss)))
      (cond
       ((and (integerp (nth 4 state)) ; nestable comment (i.e. with *)
             (looking-at " *\\*\\($\\|[^/]\\)")
             (save-excursion (goto-char (max (nth 8 state) (line-beginning-position)))
                             (looking-at "\\s */?\\*")))
        (delete-char 2))
       ((and (nth 4 state) ; row comment (i.e. with //)
             (looking-at " //"))
        (delete-char 3))))
    (scala-indent:fixup-whitespace)))

(provide 'scala-mode-indent)
