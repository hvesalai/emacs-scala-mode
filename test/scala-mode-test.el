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
    ('font-lock-type-face "T")
    ('font-lock-string-face "S")
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

(ert-deftest smt:syntax-class-and-font-lock-test-7 ()
  (smt:test
   "val xs = 1 :: 2 :: Nil"
   "2220220102033020330222"
   "KKK-VV-K-C----C----CCC"))

(ert-deftest smt:syntax-class-and-font-lock-test-8 ()
  (smt:test
   "val xs = 1:: 2 :: Nil"
   "222022010211020330222"
   "KKK-VV-K-C---C----CCC"))

(ert-deftest smt:syntax-class-and-font-lock-test-9 ()
  (smt:test
   "val xs = 1 ::2 :: Nil"
   "222022010201120330222"
   "KKK-VV-K-C---C----CCC"))

(ert-deftest smt:syntax-class-and-font-lock-test-10 ()
  (smt:test
   "case a :: (2) :: Nil"
   "22220203304250330222"
   "KKKK-V-TT--C--CC-TTT"))

(ert-deftest smt:syntax-class-and-font-lock-test-11 ()
  (smt:test
   "abc :<: def"
   "22203330222"
   "--------KKK"))

(ert-deftest smt:syntax-class-and-font-lock-test-12 ()
  (smt:test
   "Foo<T>"
   "222121"
   "CCC-C-"))

(ert-deftest smt:syntax-class-and-font-lock-test-13 ()
  (smt:test
   "class X[T<:Mapper[T]](t: T){}"
   "22222024211222222425542102545"
   "KKKKK-T-CKKCCCCCC-C----K-T---"))

(ert-deftest smt:syntax-class-and-font-lock-test-14 ()
  (smt:test
   "class X[T <: Mapper[T]](t: T){}"
   "2222202420330222222425542102545"
   "KKKKK-T-C-KK-CCCCCC-C----K-T---"))

(ert-deftest smt:syntax-class-and-font-lock-test-15 ()
  (smt:test
   "val c = /* hello */ 20"
   "2220201011022222011022"
   "KKK-V-K-DDDOOOOOOOO-CC"))

(ert-deftest smt:syntax-class-and-font-lock-test-16 ()
  (smt:test
   "val c = /* hello **/ 20"
   "22202010110222220111022"
   "KKK-V-K-DDDOOOOOOOOO-CC"))

(ert-deftest smt:syntax-class-and-font-lock-test-17 ()
  (smt:test
   "val c = /**** hello */ 20"
   "2220201011111022222011022"
   "KKK-V-K-DDDDDDOOOOOOOO-CC"))

(ert-deftest smt:syntax-class-and-font-lock-test-18 ()
  (smt:test
   "val c = //**** hello */ 20"
   "22202010111111022222011022"
   "KKK-V-K-DDOOOOOOOOOOOOOOOO"))

(ert-deftest smt:syntax-class-and-font-lock-test-19 ()
  (smt:test
   "val c = 1 /////////// big comment"
   "222020102011111111111022202222222"
   "KKK-V-K-C-DDDDDDDDDDDDOOOOOOOOOOO"))

(ert-deftest smt:syntax-class-and-font-lock-test-20 ()
  (smt:test
   "val c = s\"result $sum\""
   "2220201027222222012227"
   "KKK-V-K--SSSSSSSSVVVVS"))

(ert-deftest smt:syntax-class-and-font-lock-test-21 ()
  (smt:test
   "val c = s\"$sum-123\""
   "2220201027122212227"
   "KKK-V-K--SVVVVSSSSS"))

(ert-deftest smt:syntax-class-and-font-lock-test-22 ()
  (smt:test
   "val c = s\"${sum.getOrElse(\"\")} - $sum\""
   "22202010271422212222222224775501012227"
   "KKK-V-K--SSSSSSSSSSSSSSSSSSSSSSSSSSSSS"))

(ert-deftest smt:syntax-class-and-font-lock-test-23 ()
  "Test that `[!%&*+/?\\\\^|~-#:<=>@]\*/` will be treated as punctuation and
_not_ a symbol. Doing so would cause comment strings such as `/* Comment &*/` to
not be recognized as a delimiter, causing the entire file to treated as a
comment. A concrete example may be viewed at https://github.com/scala/scala/blob/v2.11.11/src/reflect/scala/reflect/internal/Symbols.scala#L863"
  (smt:test
   "/* &*/"
   "110111"
   "DDDOOO"))
