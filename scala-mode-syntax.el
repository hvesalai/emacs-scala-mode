;;;; scala-mode-syntax.el - Major mode for editing scala, syntax
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

;;; Based on Scala Language Specification (SLS) Version 2.9

(provide 'scala-mode-syntax)

(require 'scala-mode-constants)

;;;;
;;;; Scala syntax regular expressions
;;;;

;;; Based on the Scala language specification 2.9.  Note: order is not
;;; the same as in the document, as here things are declared before
;;; used.

;;; A note on naming. Things that end with '-re' are regular
;;; expressions.  Things that end with '-group' are regular expression
;;; character groups without the enclosing [], i.e. they are not
;;; regular expressions, but can be used in declaring one.

;; single letter matching groups (Chapter 1)
(defconst scala-syntax:hexDigit-group "0-9A-Fa-f")
(defconst scala-syntax:UnicodeEscape-re (concat "\\\\u[" scala-syntax:hexDigit-group "]\\{4\\}"))

(defconst scala-syntax:upper-group "_[:upper:]\\$") ;; missing _ to make ids work
(defconst scala-syntax:upperAndUnderscore-group (concat "_" scala-syntax:upper-group ))
(defconst scala-syntax:lower-group "[:lower:]")
(defconst scala-syntax:letter-group (concat scala-syntax:lower-group scala-syntax:upper-group)) ;; TODO: add Lt, Lo, Nl
(defconst scala-syntax:digit-group "0-9")
(defconst scala-syntax:letterOrDigit-group (concat
                                            scala-syntax:upperAndUnderscore-group 
                                            scala-syntax:lower-group 
                                            scala-syntax:digit-group))
(defconst scala-syntax:opchar-group "!#%&*+/:<=>?@\\\\\\^|~\\-") ;; TODO: Sm, So

;; Scala delimiters (Chapter 1), but no quotes
(defconst scala-syntax:delimiter-group ".,;")

;; Integer Literal (Chapter 1.3.1)
(defconst scala-syntax:nonZeroDigit-group "1-9")
(defconst scala-syntax:octalDigit-group "0-7")
(defconst scala-syntax:decimalNumeral-re 
  (concat "0" 
          "\\|[" scala-syntax:nonZeroDigit-group "][" scala-syntax:digit-group "]*"))
(defconst scala-syntax:hexNumeral-re (concat "0x[" scala-syntax:hexDigit-group "]+"))
(defconst scala-syntax:octalNumeral-re (concat "0[" scala-syntax:octalDigit-group "]+"))
(defconst scala-syntax:integerLiteral-re (concat "-?" ;; added from definition of literal
                                          "\\(" scala-syntax:decimalNumeral-re
                                          "\\|" scala-syntax:hexNumeral-re
                                          "\\|" scala-syntax:octalNumeral-re
                                          "\\)[Ll]?"))

;; Floating Point Literal (Chapter 1.3.2)
(defconst scala-syntax:exponentPart-re (concat "\\([eE][+-]?[" scala-syntax:digit-group "]+\\)"))
(defconst scala-syntax:floatType-re "[fFdD]")
(defconst scala-syntax:floatingPointLiteral-re 
  (concat "-?" ;; added from definition of literal
          "\\([" scala-syntax:digit-group "]+\\.[" scala-syntax:digit-group "]*" 
          scala-syntax:exponentPart-re "?" scala-syntax:floatType-re "?"
          "\\|" "\\.[" scala-syntax:digit-group "]+" 
          scala-syntax:exponentPart-re "?" scala-syntax:floatType-re "?"
          "\\|" "[" scala-syntax:digit-group "]+" scala-syntax:exponentPart-re
          "\\|" "[" scala-syntax:digit-group "]+" scala-syntax:floatType-re "\\)"))

;; Boolean Literals (Chapter 1.3.3)
(defconst scala-syntax:booleanLiteral-re "true|false")

;; Escape Sequences (Chapter 1.3.6)
(defconst scala-syntax:escapeSequence-re "\\\\['btnfr\"\\\\]")

;; Character Literals (Chapter 1.3.4)
(defconst scala-syntax:characterLiteral-re 
  (concat "\\('\\)\\(" "[^\\\\]" ;; should be just printable char, but this is faster
          "\\|" scala-syntax:escapeSequence-re
          "\\|" scala-syntax:UnicodeEscape-re "\\)\\('\\)"))

;; String Literals (Chapter 1.3.5)
(defconst scala-syntax:stringElement-re
  (concat "\\(" "[^\n\"\\\\\]" 
          "\\|" scala-syntax:escapeSequence-re 
          "\\|" scala-syntax:UnicodeEscape-re "\\)"))
(defconst scala-syntax:oneLineStringLiteral-re (concat "\\(\"\\)" scala-syntax:stringElement-re "*\\(\"\\)"))
(defconst scala-syntax:multiLineStringLiteral-re
  "\\(\"\\)\\(\"\"\\(\"?\"?[^\"]\\)*\"\"+\\)\\(\"\\)")
(defconst scala-syntax:stringLiteral-re
  (concat "\\(" scala-syntax:multiLineStringLiteral-re 
          "\\|" scala-syntax:oneLineStringLiteral-re "\\)" ))

;; Identifiers (Chapter 1.1)
(defconst scala-syntax:op-re (concat "[" scala-syntax:opchar-group "]+"))
(defconst scala-syntax:idrest-re 
  ;; Eagerness of regexp causes problems with _. The following is a workaround,
  ;; but the resulting regexp matches only what SLS demands.
  (concat "\\(" "[_]??" "[" scala-syntax:letter-group scala-syntax:digit-group "]+" "\\)*"
          "\\(" "_+" scala-syntax:op-re "\\|" "_" "\\)?"))
(defconst scala-syntax:varid-re (concat "[" scala-syntax:lower-group "]" scala-syntax:idrest-re))
(defconst scala-syntax:capitalid-re (concat "[" scala-syntax:upperAndUnderscore-group "]" scala-syntax:idrest-re))
(defconst scala-syntax:plainid-re (concat "\\(" scala-syntax:capitalid-re
                                   "\\|" scala-syntax:varid-re 
                                   "\\|" scala-syntax:op-re "\\)"))
;; stringlit is referred to, but not defined Scala Language Specification 2.9
(defconst scala-syntax:stringlit-re (concat scala-syntax:stringElement-re "*?")) 
(defconst scala-syntax:quotedid-re (concat "`" scala-syntax:stringlit-re "`"))
(defconst scala-syntax:id-re (concat "\\(" scala-syntax:plainid-re 
                              "\\|" scala-syntax:quotedid-re "\\)"))

;; Symbol literals (Chapter 1.3.7)
(defconst scala-syntax:symbolLiteral-re
  ;; must end with non-' to not conflict with scala-syntax:characterLiteral-re
  (concat "'" scala-syntax:plainid-re "\\([^']\\|$\\)"))

;; Literals (Chapter 1.3)
(defconst scala-syntax:literal-re
  (concat "\\(" scala-syntax:integerLiteral-re
          "\\|" scala-syntax:floatingPointLiteral-re
          "\\|" scala-syntax:booleanLiteral-re
          "\\|" scala-syntax:characterLiteral-re
          "\\|" scala-syntax:stringLiteral-re 
          "\\|" scala-syntax:symbolLiteral-re 
          "\\|" "null" "\\)"))

;; Paths (Chapter 3.1)
(defconst scala-syntax:classQualifier-re (concat "\\[" scala-syntax:id-re "\\]"))
(defconst scala-syntax:stableId-re 
  (concat "\\(\\(" scala-syntax:id-re 
          "\\|" "this"
          "\\|" "super" scala-syntax:classQualifier-re "\\)\\.\\)*"
          scala-syntax:id-re))
(defconst scala-syntax:path-re
  (concat "\\(" scala-syntax:stableId-re
          "\\|" "\\(" scala-syntax:id-re "\\." "\\)?" "this" "\\)"))


;;;
;;; Other regular expressions
;;;

(defconst scala-syntax:empty-line-re  
  "^\\s *$")

(defconst scala-syntax:keywords-re
  (regexp-opt '("abstract" "case" "catch" "class" "def"
                "do" "else" "extends" "false" "final"
                "finally" "for" "forSome" "if" "implicit"
                "import" "lazy" "match" "new" "null"
                "object" "override" "package" "private" "protected"
                "return" "sealed" "super" "this" "throw"
                "trait" "try" "true" "type" "val"
                "var" "while" "with" "yield") 'words))


;; false, true, null, super, this are neither

(defconst scala-syntax:after-reserved-symbol-underscore-re
  ;; what can be after reserved symbol _ (if there is something else, it
  ;; will be upper case letter per SLS)
  (concat "$\\|[^" scala-syntax:letterOrDigit-group "]"))

(defconst scala-syntax:reserved-symbol-underscore-re
  ;; reserved symbol _
  (concat "\\(^\\|[^" scala-syntax:letterOrDigit-group "]\\)"
          "\\(_\\)"
          "\\($\\|[^" scala-syntax:letterOrDigit-group "]\\)"))

(defconst scala-syntax:reserved-symbols-unsafe-re
  ;; reserved symbols. The regexp is unsafe as it does not
  ;; check the context.
  "\\([:=#@\u21D2\u2190]\\|=>\\|<[:%!?\\-]\\|>:\\)" )

(defconst scala-syntax:reserved-symbols-re
  ;; reserved symbols and XML starts ('<!' and '<?')
  (concat "\\(^\\|[^" scala-syntax:opchar-group "]\\)" 
          scala-syntax:reserved-symbols-unsafe-re
          "\\($\\|[^" scala-syntax:opchar-group "]\\)"))

(defconst scala-syntax:reserved-re
  (concat scala-syntax:keywords-re 
          "\\|" scala-syntax:reserved-symbols-re 
          "\\|" scala-syntax:reserved-symbol-underscore-re))

(defconst scala-syntax:mustNotTerminate-keywords-re
  (regexp-opt '("catch" "else" "extends" "finally"
                "forSome" "match" "with" "yield") 'words)
  "Keywords whiOAch cannot end a expression and are infact a sign of run-on.")

(defconst scala-syntax:mustNotTerminate-re
  (concat "\\(" scala-syntax:mustNotTerminate-keywords-re 
          "\\|[.]\\)")
  "All keywords and symbols that cannot terminate a expression
and are infact a sign of run-on. Reserved-symbols not included.")

(defconst scala-syntax:mustTerminate-re
  (concat "\\([({\\[,;]\\|=>?\\|" scala-syntax:empty-line-re "\\)")
  "Symbols that must terminate an expression, i.e the following expression
cannot be a run-on. This includes only parenthesis, '=', '=>', ',' and ';'
and the empty line")

(defconst scala-syntax:mustNotContinue-re
  ;; Although SLS says case continues an expression, we say
  ;; for the sake of indenting, it does not. Hence case is
  ;; never a run-on of the previous line.
  ;; While we cannot handle here since it could be 'do..while'
  (concat "\\("
          (regexp-opt '("abstract" "class" "def" "do" "final"
                        "for" "if" "implicit" "import" "lazy"
                        "new" "object" "override" "package" "private"
                        "protected" "return" "sealed" "throw"
                        "trait" "try" "type" "val" "var" "case") 
                      'words)
          "\\|[)}]\\|]\\)")
  "Keywords that begin an expression and parenthesis that end an
expression, i.e they cannot be run-on to the previous line even
if there is no semi in between.")

(defconst scala-syntax:double-arrow-re 
  "=>\\|\u21D2")

(defconst scala-syntax:multiLineStringLiteral-start-re
  "\\(\"\\)\"\"")

(defconst scala-syntax:multiLineStringLiteral-end-re 
  "\"\"+\\(\"\\)")

(defun scala-syntax:find-reserved-symbols () 
  (interactive)
  (re-search-forward scala-syntax:reserved-symbols-re nil t))


;;;;
;;;; Character syntax table and related syntax-propertize functions
;;;;

;;; The syntax table relies havily on the syntax-propertize-functions being
;;; run. Hence this syntax requires at least emacs 24, which introduced
;;; this new facility.

(defvar scala-syntax:syntax-table nil
  "Syntax table used in `scala-mode' buffers.")
(when (not scala-syntax:syntax-table)
  (let ((syntab (make-syntax-table)))
    ;; 1. start by reseting the syntax table: only (){}[] are
    ;; parentheses, so all others marked as parentheses in the parent
    ;; table must be marked as symbols, nothing is a punctuation
    ;; unless otherwise stated
    (map-char-table 
     #'(lambda (key value)
         (when (or (= (syntax-class value) 4) ; open
                   (= (syntax-class value) 5) ; close
                   (= (syntax-class value) 1)) ; punctuation
           (modify-syntax-entry key "_" syntab)))
     (char-table-parent syntab))

    ;; Below 'space', everything is either illegal or whitespace.
    ;; Consider as whitespace, unless otherwise stated below.
    (modify-syntax-entry '(0 . 32) " " syntab)

    ;; The scala parentheses
    (modify-syntax-entry ?\( "()" syntab)
    (modify-syntax-entry ?\[ "(]" syntab)
    (modify-syntax-entry ?\{ "(}" syntab)
    (modify-syntax-entry ?\) ")(" syntab)
    (modify-syntax-entry ?\] ")[" syntab)
    (modify-syntax-entry ?\} "){" syntab)

    ;; _ is upper-case letter, but will be modified to be symbol
    ;; constituent when in reserved symbol position by
    ;; syntax-propertize-function
    (modify-syntax-entry ?\_ "w" syntab)

    ;; by default all opchars are punctuation, but they will be
    ;; modified by syntax-propertize-function to be symbol
    ;; constituents when a part of varid or capitalid
    (dolist (char (mapcar 'identity "#%:<=>@!&*+-/?\\^|~\u21D2\u2190")) ;; TODO: Sm, So
      (modify-syntax-entry char "." syntab))

    ;; what can I say? It's the escape char.
    (modify-syntax-entry ?\\ "." syntab)
  
    ;; scala strings cannot span lines, so we mark
    ;; " as punctuation, but do the real stuff
    ;; in syntax-propertize-function for properly
    ;; formatted strings.
    (modify-syntax-entry ?\" "." syntab)

    ;; backquote is given paired delimiter syntax so that
    ;; quoted ids are parsed as one sexp. Fontification
    ;; is done separately.
    (modify-syntax-entry ?\` "$" syntab)

    ;; ' is considered an expression prefix, since it can
    ;; both start a Symbol and is a char quote. It
    ;; will be given string syntax by syntax-propertize-function
    ;; for properly formatted char literals.
    (modify-syntax-entry ?\' "'" syntab)

    ;; punctuation as specified by SLS
    (modify-syntax-entry ?\. "." syntab)
    (modify-syntax-entry ?\; "." syntab)
    (modify-syntax-entry ?\, "." syntab)

    ;; comments
    ;; the `n' means that comments can be nested
    (modify-syntax-entry ?\/  ". 124b" syntab)
    (modify-syntax-entry ?\*  ". 23n"   syntab)
    (modify-syntax-entry ?\n  "> b" syntab)
    (modify-syntax-entry ?\r  "> b" syntab)
    
    (setq scala-syntax:syntax-table syntab)))

(defun scala-syntax:propertize-extend-region (start end)
  "See syntax-propertize-extend-region-functions"
  ;; nothing yet
  nil)

(defmacro scala-syntax:put-syntax-table-property (match-group value)
  "Add 'syntax-table entry 'value' to the region marked by the
match-group 'match-group'"
  `(put-text-property (match-beginning ,match-group) 
                      (match-end ,match-group) 
                      'syntax-table 
                      ,value))

(defun scala-syntax:propertize-characterLiterals (start e§nd)
  "Mark start and end of character literals as syntax class
7 (string quotes). Only valid character literals will be marked."
  (save-excursion
    (goto-char start)
    (while (re-search-forward scala-syntax:characterLiteral-re end t)
      (scala-syntax:put-syntax-table-property 1 '(7 . nil))
      (scala-syntax:put-syntax-table-property 3 '(7 . nil)))))

(defun scala-syntax:propertize-stringLiterals (start end)
  "Mark start and end of both one-line and multi-line string
literals. One-line strings use syntax class 7 (string quotes),
while multi-line strings are marked with 15 (generic string
delimiter). Multi-line string literals are marked even if they
are unbalanced. One-line string literals have to be balanced to
get marked. This means invalid one-line strings will not be fontified."
  (let* ((string-state (nth 3 (syntax-ppss start)))
         (unbalanced-p (eq string-state t)))

    (if (and string-state (not unbalanced-p)) 
        ;; a normal string is open, let's de-propertize
        (remove-text-properties start end '(syntax-table nil))
      (save-excursion
        (goto-char start)
        ;; close the closing for the unbalanced multi-line literal
        (when (and unbalanced-p
                   (re-search-forward scala-syntax:multiLineStringLiteral-end-re end t))
          (scala-syntax:put-syntax-table-property 1 '(15 . nil)))
        ;; match any balanced one-line or multi-line literals
        (catch 'break
          (while (re-search-forward scala-syntax:stringLiteral-re end t)
            (cond
             ((match-beginning 2)
              (scala-syntax:put-syntax-table-property 2 '(15 . nil))
              (scala-syntax:put-syntax-table-property 5 '(15 . nil)))
             ((or (match-end 7) ; group 7 is non-nil, ie. online string is not empty
                  (= (match-end 8) (line-end-position)) ; empty string at line end
                  (not (= (char-after (match-end 8)) ?\"))) ; no " after empty string 
              (scala-syntax:put-syntax-table-property 6 '(7 . nil))
              (scala-syntax:put-syntax-table-property 8 '(7 . nil)))
             (t ;; backtrack and continue to next while loop
              (goto-char (match-beginning 0))
              (throw 'break nil)))))
        ;; match any start of multi-line literals that are not yet balanced
        (when (re-search-forward scala-syntax:multiLineStringLiteral-start-re end t)
          (scala-syntax:put-syntax-table-property 1 '(15 . nil)))))))

(defun scala-syntax:propertize-underscore-and-idrest (start end)
  "Mark all underscores (_) as symbol constituents (syntax 3) or
upper case letter (syntax 2). Also mark opchars in idrest as
symbol constituents (syntax 3)"
  (save-excursion
    (goto-char start)
    (while (re-search-forward "_" end t)
      (let ((match-beg (match-beginning 0))
            (match-end (match-end 0)))
        (put-text-property 
         match-beg match-end 'syntax-table
         (if (= match-beg (line-beginning-position))
             (if (looking-at scala-syntax:after-reserved-symbol-underscore-re)
                 '(3 . nil) ; symbol constituent
               '(2 . nil)) ; word syntax
           (save-excursion
             (goto-char (1- match-beg))
             (if (looking-at scala-syntax:reserved-symbol-underscore-re)
                 '(3 . nil) ; symbol constituent
               ;; check for opchars that should be marked as symbol constituents (3)
               (goto-char match-end)
               (when (looking-at scala-syntax:op-re)
                 (scala-syntax:put-syntax-table-property 0 '(3 . nil)))
               '(3 . nil))))))))) ;; symbol constituent syntax (3) also for the '_'

(defun scala-syntax:propertize (start end)
  "See syntax-propertize-function" 
  (scala-syntax:propertize-characterLiterals start end)
  (scala-syntax:propertize-stringLiterals start end)
  (scala-syntax:propertize-underscore-and-idrest start end))


;;;;
;;;; Syntax navigation functions
;;;;

(defun scala-syntax:beginning-of-code-line ()
  (interactive)
  "Move to the beginning of code on the line, or to the end of
the line, if the line is empty"
  (let ((eol (line-end-position)))
    (beginning-of-line)
    (forward-comment (buffer-size))
    (if (> (point) eol)
        eol
      (skip-syntax-forward " " eol))))

(defun scala-syntax:looking-back-empty-line-p ()
  "Return t if the previous line is empty"
  (save-excursion
    (skip-syntax-backward " " (line-beginning-position))
    (and (bolp)
         (forward-line -1)
         (looking-at scala-syntax:empty-line-re))))

(defun scala-syntax:skip-backward-ignorable ()
  "Move backwards over ignorable whitespace and comments. A
completely empty line is not ignorable and will not be moved
over. Returns the number of points moved (will be negative)."
  (while (and (not (scala-syntax:looking-back-empty-line-p))
              (forward-comment -1)))
  (skip-syntax-backward " " (line-beginning-position)))

(defun scala-syntax:looking-back-token (re)
  "Return the start position of the token matched by re, if the
current position is preceeded by it, or nil if not. All ignorable
comments and whitespace are ignored, i.e. does not search past an
empty line. Expects to be outside of comment."
  (save-excursion
    ;; skip back all comments
    (scala-syntax:skip-backward-ignorable)
    (let ((end (point)))
      ;; skip back punctuation or ids (words and related symbols and delimiters)
      (or (/= 0 (skip-chars-backward scala-syntax:delimiter-group))
          (/= 0 (skip-syntax-backward "."))
          (/= 0 (skip-syntax-backward "("))
          (/= 0 (skip-syntax-backward "w_'$")))
      ;; if we didn't move, then we didn't find anything
      (if (= (point) end)
          nil
        (if (looking-at re) (point) nil)))))

(defun scala-syntax:runonp (&optional point) 
  (interactive)
  "Returns t if the current point (or point at 'point) is on a
line that is a run-on to previous line."
  (save-excursion
    (when point (goto-char point))
    (scala-syntax:beginning-of-code-line)
    (let ((p (not (or (looking-at scala-syntax:mustNotContinue-re)
                      (scala-syntax:looking-back-empty-line-p)
                      (scala-syntax:looking-back-token 
                       scala-syntax:mustTerminate-re)))))
      (when p (message "runon @ %d" (point)))
      p )))
    
;    (defconst scala-syntax:mustNotTerminate-re
;      scala-syntax:reserved-symbols-unsafe-re
;(defconst scala-syntax:mustTerminate-re
;(defconst scala-syntax:mustNotContinue-re
