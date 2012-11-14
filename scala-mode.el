;;; scala-mode.el - Major mode for editing scala
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

;;; Based on Scala Language Specification (SLS) Version 2.9

(provide 'scala-mode)

(require 'scala-mode-constants)
(require 'scala-mode-syntax)
(require 'scala-mode-fontlock)
(require 'scala-mode-indent)
(require 'scala-mode-map)
(require 'cc-cmds)

;; Tested only for emacs 24
(unless (<= 24 emacs-major-version)
  (error
   (format "The Scala mode has been tested only on Emacs version 24.2 (and not your Emacs version %s.%s)"  
           emacs-major-version  emacs-minor-version)))

(defgroup scala nil
  "A programming mode for the Scala language 2.9"
  :group 'languages)

(defmacro scala-mode:make-local-variables (&rest quoted-names)
  (cons 'progn (mapcar #'(lambda (quoted-name) `(make-local-variable ,quoted-name)) quoted-names)))

(defconst scala-mode:comment-line-start
  (concat "[ \t]*"            ; whitespace
          "\\(//+\\|\\**\\)"  ; comment start
          "[ \t]*"))          ; whitespace

(defconst scala-mode:paragraph-start
  (concat scala-mode:comment-line-start
          "\\($"               ; empty line
          "\\|=[^=\n]+=[ ]*$"   ; heading 1
          "\\|==[^=\n]+==[ ]*$"   ; heading 2
          "\\|===[^=\n]+===[ ]*$"   ; heading 3
          "\\|====+[^=\n]+====+[ ]*$"   ; heading 4-n
          "\\|-"               ; unordered liststs
          "\\|[1IiAa]\\."      ; ordered lists
;          "\\|{{{"            ; code block start
;          "\\|}}}"            ; code block end
          "\\|@[a-zA-Z]+\\>"   ; annotations
          "\\)"
          ))

(defconst scala-mode:paragraph-separate
  (concat scala-mode:comment-line-start "$"))

(defun scala-mode:forward-sexp-function (&optional count)
  (unless count (setq count 1))
  (if (< count 0)
      (dotimes (n (abs count))
        (scala-syntax:backward-sexp))
    (dotimes (n count)
      (scala-syntax:forward-sexp))))

;; (defun scala-mode ()
;;   "Major mode for editing scala code.

;; When started, runs `scala-mode-hook'. 

;; \\{scala-mode-map}"
;;   (interactive)
;;   (kill-all-local-variables)
;;   (set-syntax-table scala-mode-syntax-table)

;;   (scala-mode:make-local-variables
;;    'require-final-newline
;;    'comment-start
;;    'comment-end
;;    'comment-start-line
;;    'comment-column
;;    'comment-multi-line)

;;;###autoload
(define-derived-mode scala-mode prog-mode "Scala"
  "Major mode for editing scala code.

When started, runs `scala-mode-hook'. 

\\{scala-mode-map}" 
  :syntax-table                         scala-syntax:syntax-table
;  :group                               
;  :abbrev

  (scala-mode:make-local-variables
   'scala-mode:debug-messages
   'syntax-propertize-function
   'font-lock-defaults
   'paragraph-start
   'paragraph-separate
   'fill-paragraph-function
   'comment-start
   'comment-end
   'comment-start-skip
   'comment-column
   'comment-multi-line
   'forward-sexp-function
   'indent-line-function
   'indent-tabs-mode)

  (add-hook 'syntax-propertize-extend-region-functions
            'scala-syntax:propertize-extend-region)
  (setq scala-mode:debug-messages       nil

        syntax-propertize-function      'scala-syntax:propertize
        parse-sexp-lookup-properties    t

        ;; TODO: font-lock
        font-lock-defaults              '((scala-font-lock:keywords)
                                          nil)

        ;; TODO: paragraph-start, paragraphs-separate, paragraph-ignore-fill-prefix
        ;; TODO: beginning-of-defun-function, end-of-defun-function

        ;; comments
        paragraph-start                 scala-mode:paragraph-start
        paragraph-separate              scala-mode:paragraph-separate
        fill-paragraph-function         'c-fill-paragraph
        comment-start                   "// "
        comment-end                     ""
        comment-start-skip              "\\(//+\\|/\\*+\\)[ \t]*"
        comment-column                  0
        comment-multi-line              t
        ;; TODO: comment-indent-function

        ;; TODO: forward-sexp-function
        forward-sexp-function           'scala-mode:forward-sexp-function
        indent-line-function            'scala-indent:indent-line
        indent-tabs-mode                nil
        )
  (use-local-map scala-mode-map)
  (turn-on-font-lock)
  ;; add indent functionality to some characters
  (scala-mode-map:add-self-insert-hooks)
)

;; Attach .scala files to the scala-mode
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))
(modify-coding-system-alist 'file "\\.sbt\\'" 'utf-8)

(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(modify-coding-system-alist 'file "\\.scala\\'" 'utf-8)
