;;; scala-mode-lib.el - Major mode for editing scala, common functions
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

(provide 'scala-mode-lib)

(defmacro scala-lib:column-after (&rest body)
  `(save-excursion
     ,@body
     (current-column)))

(defmacro scala-lib:point-after (&rest body)
  `(save-excursion
     ,@body
     (point)))
