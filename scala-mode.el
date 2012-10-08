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

;; Tested only for emacs 23
(unless (<= 23 emacs-major-version)
  (error
   (format "The Scala mode has been tested only on Emacs version 23.x (and not your Emacs version %s.%s)"  
           emacs-major-version  emacs-minor-version)))

(defgroup scala nil
  "A programming mode for the Scala language 2.9"
  :group 'languages)

(defmacro scala-mode:make-local-variables (&rest quoted-names)
  (cons 'progn (mapcar #'(lambda (quoted-name) `(make-local-variable ,quoted-name)) quoted-names)))

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
   'syntax-propertize-function
   'font-lock-defaults
   'comment-start
   'comment-end
   'comment-start-skip
   'comment-column
   'comment-multi-line
   'indent-line-function
   'indent-tabs-mode)

  (add-hook 'syntax-propertize-extend-region-functions
            'scala-syntax:propertize-extend-region)
  (setq 
        syntax-propertize-function      'scala-syntax:propertize
        parse-sexp-lookup-properties    t

        ;; TODO: font-lock
        font-lock-defaults              '(scala-font-lock:keywords
                                          nil)

        ;; TODO: paragraph-start, paragraphs-separate, paragraph-ignore-fill-prefix
        ;; TODO: beginning-of-defun-function, end-of-defun-function

        ;; comments
        comment-start                   "// "
        comment-end                     ""
        comment-start-skip              "\\(//+\\|/\\*+\\)\\s *"
        comment-column                  0
        comment-multi-line              t
        ;; TODO: comment-indent-function

        indent-line-function            'scala-indent:indent-line
        indent-tabs-mode                nil
        )
  (use-local-map scala-mode-map)
  (turn-on-font-lock)
)

;; Attach .scala files to the scala-mode
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(modify-coding-system-alist 'file "\\.scala\\'" 'utf-8)
