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

(defcustom scala-indent:use-cycle-indent nil
  "When non-nil, indentation will cycle from the new indent
  strategy indent, the last known indent, and the left margin on
  subsequent indent-line calls."
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
       ;; ((scala-indent:body-p) ;; TODO did I delete this function when I shouldn't have?
       ;; TODO or even if I did, maybe it just doesn't matter because the
       ;; heuristics that union this algorithm with the other will compensate?
       ;;  nil)
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
  "A kind of tokenize step of the hand-wavy parse"
  (pcase stack
    ;; <hitting the beginning of a block when starting in the middle> { (
    (`(?\{) 'left-boundary) ;; too aggressive?
    (`(?\{ ,_ . ,_) 'left-boundary)
    (`(?\( ,_ . ,_) 'left-boundary)
    ;; <dot chaining>
    (`(?\n ?.) 'dot-chain)
    (`(?\n ?. . ,_) 'dot-chain)
    ;; =
    (`(= ?\n . ,_) 'decl-lhs)
    ((and `(= ,_ . ,tail) (guard (memq ?\n tail))) 'after-decl)
    (`(= ,_ . ,_) 'decl-inline-lhs)
    ;; =>
    (`(=> ?\n . ,_) 'arrow-lhs)
    ((and `(=> ,_ . ,tail) (guard (memq ?\n tail))) 'after-arrow)
    (`(=> ,_ . ,_) 'arrow-lhs)
    ;; <-
    (`(<- . ,_) 'generator)
    ;; case
    (`(case . ,_) 'case)
    ;; class
    ((and `(class . ,tail) (guard (memq ': tail))) 'block)
    (`(class . ,_) 'decl)
    ;; def
    ((and `(def . ,tail) (guard (memq '= tail)))
     (if (memq ?\n tail) 'after-decl 'block))
    (`(def . ,_) 'decl)
    ;; do
    (`(do ,_ . ,_) 'block)
    ;; else
    (`(else ?\n . ,_) 'else-conseq)
    (`(else) 'else)
    (`(else . ,_) 'else-inline)
    ;; enum
    ((and `(enum . ,tail) (guard (memq ': tail))) 'block)
    (`(enum . ,_) 'decl)
    ;; final
    (`(final) 'decl)
    ;; for
    (`(for) 'for-comp)
    (`(for . ,_) 'for-body)
    ;; given
    (`(given . ,_) 'decl)
    ;; if
    (`(if ?\n . ,_) 'if-cond)
    (`(if . ,_) 'if)
    ;; implicit
    (`(implicit) 'decl)
    ;; import
    ((and `(import . ,tail) (guard (memq ?\n tail))) 'after-decl)
    (`(import . ,_) 'decl)
    ;; match
    (`(,_ match) 'match)
    ;; object
    ((and `(object . ,tail) (guard (memq ': tail))) 'block)
    (`(object . ,_) 'decl)
    ;; override
    (`(override) 'decl)
    ;; package
    (`(package . ,_) 'decl)
    ;; sealed
    (`(sealed) 'decl)
    ;; then
    (`(then ?\n . ,_) 'then-conseq)
    (`(then) 'then)
    (`(then . ,_) 'then-inline)
    ;; trait
    ((and `(trait . ,tail) (guard (memq ': tail))) 'block)
    (`(trait . ,_) 'decl)
    ;; val
    ((and `(val . ,tail) (guard (memq '= tail)))
     (if (memq ?\n tail) 'after-decl 'block))
    (`(val . ,_) 'decl)
    ;; var
    (`(var . ,_) 'decl)
    ;; while
    (`(while) 'decl)
    ;; with
    (`(with) 'block)
    ;; yield
    (`(yield . ,_) 'yield-from-comp)
    ))

(defun scala-indent:relative-indent-by-elem (syntax-elem)
  "TODO document"
  (pcase syntax-elem
    ;; after-decl
    (`(after-decl else) -2)
    (`(after-decl) 0)
    ;; arrow-lhs
    (`(arrow-lhs) 2)
    (`(arrow-lhs case . ,_) 0) ;; within match
    (`(arrow-lhs dot-chain) 4)
    (`(arrow-lhs . ,_) :maintain)
    ;; block
    (`(block) 2)
    (`(block . ,_) 2)
    ;; case
    (`(case) :maintain)
    (`(case case) 0) ;; e.g. in enums
    (`(case ,_) 2)
    ;; decl
    (`(decl decl) 0)
    (`(decl decl decl-inline-lhs) 0)
    (`(decl else) -2)
    (`(decl . ,_) 2)
    ;; decl-lhs
    (`(decl-lhs decl . ,_) 2)
    (`(decl-lhs dot-chain) 4)
    (`(dot-chain dot-chain) 0)
    (`(decl-lhs for-comp) 0)
    (`(decl-lhs generator) 0)
    (`(decl-lhs yield-from-comp) -2)
    (`(decl-lhs) 2)
    (`(decl-lhs . ,_) 0)
    ;; else
    (`(else ,_) 2)
    ;; else-conseq
    (`(else-conseq) 2)
    (`(else-conseq . ,_) :maintain)
    ;; else-inline
    (`(else-inline . ,_) 0)
    ;; for-body
    (`(for-body . ,_) 2)
    ;; for-comp
    (`(for-comp yield-from-comp) 0)
    ;; generator
    (`(generator yield-from-comp) -2)
    (`(generator . ,_) 0)
    ;; if
    (`(if then) 0)
    (`(if then-inline) 0)
    (`(if . ,_) 2)
    ;; if-cond
    (`(if-cond then) 0)
    (`(if-cond) 2)
    ;; left-boundary
    (`(left-boundary dot-chain) 4)
    ;; match
    (`(match case . ,_) 2)
    ;; then
    (`(then else) 0)
    (`(then else-inline) 0)
    (`(then ,_) 2)
    ;; then-conseq
    (`(then-conseq else) 0)
    (`(then-conseq) 2)
    (`(then-conseq ,_) 2)
    ;; then-inline
    (`(then-inline else) 0)
    (`(then-inline else-inline) 0)
    ;; yield-from-comp
    (`(yield-from-comp) 0)
    ;; <fallbacks>
    (`(,_ then) -2)
    (`(,_ else) -2)
    ))

(defun scala-indent:find-analysis-start (&optional point)
  "Find a place to start tokenizing in a consistent manner"
  (save-excursion
    (when point (goto-char point))
    (let (stack)
      ;; Always look at a token on the current for starters
      (when (> (current-indentation) (current-column))
        (scala-syntax:forward-token))
      (if (= (line-beginning-position) (line-end-position))
          ;; Handle blank lines
          (progn
            (scala-syntax:backward-sexp-forcing)
            (setq stack (cons ?\n stack)))
        ;; (beginning-of-thing 'sexp) gets confused by `.'
        (unless (looking-at-p "\\.")
          ;; Avoid double-reading current symbol
          (beginning-of-thing 'sexp)))
      ;; handle the occurence of case in various contexts
      (or (save-excursion
            (when-let ((_ (looking-at-p (concat "case *"
                                              scala-syntax:class-or-object-re)))
                     (point (progn (forward-to-word 1) (point)))
                     (class-or-object (sexp-at-point)))
              ;; This throws away the stack we've built up above. The assumption
              ;; here is that this case is mutually exclusive with those above.
              (scala-indent:skip-back-over-modifiers point
                                                     (list class-or-object))))
       (list (point) stack)))))

(defun scala-indent:analyze-context (point &optional init-stack)
  "TODO document"
  (save-excursion
    (goto-char point)
    (let (result
          last-indentation
          (stack init-stack))
      (while (and (not result) (> (point) 1))
        (setq stack
              (if (looking-at-p "\\.")
                  (cons ?. stack)
                (let ((s (or (sexp-at-point) (char-after))))
                  (backward-char)
                  (if (looking-at-p "\\.")
                      ;; Try hard to notice dot-chaining
                      (cons ?. (cons s stack))
                    (if (looking-at-p "\"")
                        ;; A little hack in case we are inside of a string
                        stack
                      (forward-char)
                      (cons s stack))))))
        (setq result
              (scala-indent:analyze-syntax-stack stack))
        (when (and (not result)
                   (save-excursion (= (point)
                                      (scala-syntax:beginning-of-code-line))))
          (setq stack (cons ?\n stack))
          (setq result
                (scala-indent:analyze-syntax-stack stack))
          (when result
            (setq last-indentation (current-indentation))
            (scala-syntax:backward-sexp-forcing)))
        (unless result
          (setq last-indentation (current-indentation))
          (while (looking-at-p "\\.") (backward-char))
          ;; ")." is a funny case where we actually do want to be on the dot
          (if (looking-at-p ")") (forward-char))
          (scala-syntax:backward-sexp-forcing)))
      (let* ((x (or (scala-indent:skip-back-over-modifiers (point) stack)
                    (list (point) stack)))
             (point (nth 0 x))
             (stack (nth 1 x)))
        (list result
              (line-number-at-pos)
              (current-indentation)
              last-indentation
              point
              stack)))))

(defun scala-indent:full-stmt-less-than-line (syntax-elem stopped-point)
  (and
   (consp syntax-elem)
   ;; read a full statement
   (pcase (car syntax-elem)
     ('after-decl t)
     ('after-arrow t))
   (save-excursion
     (goto-char stopped-point)
     ;; but that statement took up less than a line
     (> (current-column) (current-indentation)))))

(defun scala-indent:continue-lookback? (syntax-elem
                                       ctxt-line
                                       line-no
                                       stopped-point
                                       end-stack)
  (or (and (= ctxt-line line-no) (> line-no 1)
           ;; If we keep reading for this reason, we've accepted the
           ;; existing tokens and so need to clear the stack
           (list syntax-elem ;; syntax-elem
                 nil ;; stack
                 (save-excursion ;; point
                   (goto-char stopped-point)
                   (scala-syntax:backward-sexp-forcing)
                   (point))))
      (when (scala-indent:full-stmt-less-than-line syntax-elem stopped-point)
        ;; If we read a full statement that was only part of a line,
        ;; drop it and try again for more context
        (list (cdr syntax-elem) ;; syntax-elem
              end-stack ;; restart with the existing stack
              (save-excursion ;; point
                (goto-char stopped-point)
                (scala-syntax:backward-sexp-forcing)
                (point))))
      ;; We know we have a dot-chain, but we need to get more context to know
      ;; how to position it
      (when (equal syntax-elem '(dot-chain))
        (list syntax-elem ;; syntax-elem
              nil ;; stack
              stopped-point ;; point
              ))))

(defun scala-indent:skip-back-over-modifiers (point stack)
  (if-let* ((head (car stack))
            (_ (memq head '(trait class object)))
            (new-point point)
            (new-sexp t)
            (new-stack stack))
      (save-excursion
        (goto-char new-point)
        (scala-syntax:backward-sexp-forcing)
        (setq new-sexp (sexp-at-point))
        (while (memq new-sexp
                     '(final sealed case open abstract implicit private))
          (setq new-point (point))
          (setq new-stack (cons new-sexp new-stack))
          (scala-syntax:backward-sexp-forcing)
          (setq new-sexp (sexp-at-point)))
        (list new-point new-stack))))

(defun scala-indent:whitespace-biased-indent (&optional point)
  "Whitespace-syntax-friendly heuristic indentation engine.

The basic idea is to look back a relatively short distance (one semantic line
back with some hand-waving) to parse the context based on a two-level
tokenization. The parser is not anything like well-formalized, but it can start
at an arbitrary point in the buffer, and except in pathological cases, look at
relatively few lines in order to make a good guess; and it is tolerant to a
certain amount of incorrect or in-progress syntactic forms."
  (let* ((initResult (scala-indent:find-analysis-start point))
         (point (car initResult))
         (stack (cadr initResult))
         (line-no (line-number-at-pos point))
         (analysis (scala-indent:analyze-context point stack))
         (syntax-elem (list (nth 0 analysis)))
         (ctxt-line (nth 1 analysis))
         (ctxt-indent (nth 2 analysis))
         (prev-indent (nth 3 analysis))
         (stopped-point (nth 4 analysis))
         (end-stack (nth 5 analysis))
         )
    (message "analysis: %s" analysis)
    (while (when-let ((x (scala-indent:continue-lookback?
                        syntax-elem ctxt-line line-no stopped-point end-stack)))
             (setq syntax-elem (nth 0 x))
             (setq stack (nth 1 x))
             (setq point (nth 2 x))
             t)
      (setq analysis (scala-indent:analyze-context point stack))
      (setq syntax-elem
	    (if (nth 0 analysis)
		(cons (nth 0 analysis) syntax-elem)
	      syntax-elem))
      (setq ctxt-line (nth 1 analysis))
      (setq ctxt-indent (nth 2 analysis))
      (setq prev-indent (nth 3 analysis))
      (let ((old-stopped-point stopped-point))
        (setq stopped-point (nth 4 analysis))
        (when (eq old-stopped-point stopped-point)
          (message
           "Whitespace-friendly indentation algorithm not making progress :(")
          (error "Got stuck at %s" stopped-point)))
      (setq end-stack (nth 5 analysis)))
    (when-let ((_ (< ctxt-line line-no))
               (relative (scala-indent:relative-indent-by-elem syntax-elem)))
      (list (if (eq :maintain relative)
                (current-indentation)
              (+ (if (eq ?\n (car end-stack))
                     ;; Oops, moved a bit too far back while determining
                     ;; context. Don't really want to determine our indentation
                     ;; based on the line whose newline we are looking at, but
                     ;; rather the next one.
                     prev-indent
                   ctxt-indent)
                 relative))
            stopped-point))))

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

(defun scala-indent:block-biased-indent (point)
  "TODO."
  (save-excursion
    (when point (goto-char point))
    (let* ((pos (scala-syntax:beginning-of-code-line))
           (anchor (scala-indent:goto-block-anchor point)))
      (when anchor
        (when (/= anchor (point))
          (error (format "Assertion error: anchor=%d, point=%d" anchor (point))))
        (list
         (+ (current-column)
            (save-excursion
              (scala-indent:resolve-block-step pos anchor)))
         anchor
        )
        ))))

(defun scala-indent:reconcile (whitespace block)
  (let ((ws-indent (nth 0 whitespace))
        (ws-lookback-point (nth 1 whitespace))
        (blk-indent-point (nth 0 block))
        (blk-lookback (nth 1 block)))
    (cond
     ;; Nothing to reconcile
     ((eq ws-indent blk-indent-point) ws-indent)
     ;; Counterintuitive as it may be, the algorithm that had to look the
     ;; farthest back (and so has the smallest lookback point) is least likely
     ;; to have gotten the answer right. This is because both algorithms have
     ;; bias toward not giving up; but the more remote they get from their
     ;; starting point, the more likely it is that they did not understand the
     ;; local syntax, and are going to suggest a large and unpleasant change in
     ;; indentation. Or from another perspective: we want to bias toward local
     ;; correctness. If they stopped on the same character, then we know from
     ;; the behavior of the block algorithm that it is a parenthetical
     ;; character; in which case the block algorithm most likely got the right
     ;; answer.
     ((> ws-lookback-point blk-lookback) ws-indent)
     (t blk-indent-point))))

(defun scala-indent:calculate-indent-for-line (&optional point)
  "Calculate the appropriate indent for the current point or POINT.

Returns the new column, or nil if the indent cannot be determined."
  (let ((whitespace (ignore-errors
                      (scala-indent:whitespace-biased-indent point)))
        (block (scala-indent:block-biased-indent point)))
    (pcase (cons whitespace block)
      (`(nil . ,x) (nth 0 x))
      (`(,x . nil) (nth 0 x))
      (`(,x . ,y) (scala-indent:reconcile x y)))
   ))

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

(defvar-local scala-indent:cycle-indent-stack (list)
  "The automatically buffer local scala indent cycle stack.

The stack is initialized as (left-margin, (current-indentation))
when the custom var \"scala-indent:use-cycle-indent\" is non-nil
and \"scala-indent:indent-line\" is called. Subsequent
\"scala-indent:indent-line\" calls pop the indentation value from
the stack, until it is empty, resetting the indentation cycle.")



(defun scala-indent:cycle-indent-stack-push (indentation)
  "Pushes an integer value onto the \"scala-indent:cycle-indent-stack\".

Will fail if INDENTATION is not an integer"

  (if (integerp indentation)
      (add-to-list 'scala-indent:cycle-indent-stack indentation)
    (error "\"scala-indent:cycle-indent-stack-push\": Invalid INDENTATION argument %s"
	   indentation)))

(defun scala-indent:cycle-indent-stack-pop ()
  "Gets the top value of the \"scala-indent:cycle-indent-stack\" stack.

 Modifies the stack in-place."

  (pop (buffer-local-value 'scala-indent:cycle-indent-stack (current-buffer))))

(defun scala-indent:cycle-indent-stack-depth ()
  "The current depth of the \"scala-indent:cycle-indent-stack\" stack"
  
  (length (buffer-local-value 'scala-indent:cycle-indent-stack (current-buffer))))


(defun scala-indent:cycle-indent-stack-emptyp (x)
  "Check if the \"scala-indent:cycle-indent-stack\" is empty.

Returns t if the \"scala-indent:cycle-indent-stack\" is empty,
nil otherwise."

  (= (length (buffer-local-value 'scala-indent:cycle-indent-stack (current-buffer))) 0))

(defun scala-indent:cycle-indent-line (&optional strategy)
  "Cycle scala indentation using optionally passed STRATEGY.

When the \"scala-indent:cycle-indent-stack\" is empty, push 0 and
the current indentation onto the stack, then indent according to
the optionally passed STRATEGY.  Indent to the top of
\"scala-indent:cycle-indent-stack\" when non-empty."
  
  (interactive "*")
  (cond ((scala-indent:cycle-indent-stack-emptyp nil)
	 (scala-indent:cycle-indent-stack-push (current-indentation))
	 (scala-indent:cycle-indent-stack-push 0)
	 (call-interactively 'scala-indent:strategy-indent-line t))
	(t (scala-indent:indent-line-to (scala-indent:cycle-indent-stack-pop)))))

;; the previously-named scala-indent:indent-line
(defun scala-indent:strategy-indent-line (&optional strategy)
  "Indent lines according to the OPTIONAL scala indentation STRATEGY."
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

(defun scala-indent:indent-line (&optional strategy)
  "Indent the current line with cycling.

If the custom var \"scala-indent:use-cycle-indent\" is non-nil,
cycle-indent using the optionally passed STRATEGY.  Indent using
the optionally passed STRATEGY without cycling otherwise."
  
  (interactive "*")
  (if scala-indent:use-cycle-indent
      (call-interactively 'scala-indent:cycle-indent-line t)
    (call-interactively 'scala-indent:strategy-indent-line t)))

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
