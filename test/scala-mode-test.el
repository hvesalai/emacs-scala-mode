(ert-deftest smt:quotedid () (test-fontification "`volatile ++`"))

(defun test-fontification (scala-symbol)
  (with-temp-buffer
    (insert (format "package ensime

object Ensime {
  val %s = 123
}" scala-symbol))
    (scala-mode)
    (font-lock-ensure)
    (re-search-backward (regexp-opt `(,scala-symbol)) nil t)
    (should (equal 'font-lock-variable-name-face (get-text-property (point) 'face)))
    (should (equal 8 (syntax-class (syntax-after (point)))))))
