(defun smt:test (line exps expf)
  "line - line of scala code
exps - expected codes of syntax class
expf - expected font-locks"
  (let ((line-length (length line)))
    (with-temp-buffer
      (insert (format "package ensime

object Ensime {
  %s
}" line))
      (scala-mode)
      (font-lock-ensure)
      (re-search-backward (regexp-opt `(,line)) nil t)
      (let ((end-point (+ (point) line-length))
            (acc-syntax "")
            (acc-font ""))
        (while (< (point) end-point)
          (setq acc-syntax (concat acc-syntax (number-to-string (syntax-class (syntax-after (point))))))
          (setq acc-font (concat acc-font (font-lock-to-string (get-text-property (point) 'face))))
          (forward-char))
        (should (equal acc-syntax exps))
        (should (equal acc-font expf))))))

(defun font-lock-to-string (font-lock)
  (pcase font-lock
    (`nil "-")
    ('font-lock-constant-face "C")
    ('font-lock-variable-name-face "V")
    ('font-lock-keyword-face "K")
    ('font-lock-comment-face "O")
    ('font-lock-comment-delimiter-face "D")
    ('font-lock-doc-face "U")
    (_ "?")))

(ert-deftest smt:syntax-class-and-font-lock-test-1 ()
  (smt:test
   "val `tvw xyz/*` = `abc def/*` + 123 /* comment `abc` abc */ + 456"
   "22203333333333301033333333333010222011022222220333330222011010222"
   "KKK-VVVVVVVVVVV-K---------------CCC-DDDOOOOOOOOOOOOOOOOOOOO---CCC"))

(ert-deftest smt:syntax-class-and-font-lock-test-2 ()
  (smt:test
   "val |+| = 123"
   "2220333010222"
   "KKK-VVV-K-CCC"))

(ert-deftest smt:syntax-class-and-font-lock-test-3 ()
  (smt:test
   "val a_|+| = 123"
   "222023333010222"
   "KKK-VVVVV-K-CCC"))

(ert-deftest smt:syntax-class-and-font-lock-test-4 ()
  (smt:test
   "val a = 123 /** hello */"
   "222020102220111022222011"
   "KKK-V-K-CCC-UUUUUUUUUUUU"))

(ert-deftest smt:syntax-class-and-font-lock-test-5 ()
  (smt:test
   "val a = <td>hello</td>"
   "2220201012212222211221"
   "KKK-V-K---------------"))

(ert-deftest smt:syntax-class-and-font-lock-test-6 ()
  (smt:test
   "// val |--| = 123"
   "11022203333010222"
   "DDDOOOOOOOOOOOOOO"))
