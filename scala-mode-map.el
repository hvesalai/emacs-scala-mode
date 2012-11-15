;;; scala-mode-map.el - Major mode for editing scala, keyboard map
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

(provide 'scala-mode-map)

(require 'scala-mode-indent)

(defmacro scala-mode-map:define-keys (key-map key-funcs)
  (cons 'progn (mapcar 
   #'(lambda (key-func)
       `(define-key ,key-map ,(car key-func) ,(cadr key-func)))
   key-funcs)))

(defvar scala-mode-map nil
  "Local key map used for scala mode")

(defun scala-mode-map:add-self-insert-hooks ()
  (add-hook 'post-self-insert-hook
            'scala-indent:indent-on-parentheses)
  (add-hook 'post-self-insert-hook
            'scala-indent:indent-on-special-words)
  (add-hook 'post-self-insert-hook
            'scala-indent:indent-on-scaladoc-asterisk))

(when (not scala-mode-map)
  (let ((keymap (make-sparse-keymap)))
    (scala-mode-map:define-keys 
     keymap
     (
;;      ([(control c)(control r)]   'scala-indent:rotate-run-on-strategy)
      ))
     (setq scala-mode-map keymap)))
  
