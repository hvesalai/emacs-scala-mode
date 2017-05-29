(ert-deftest smt:quotedid ()
  (let* ((line "val `tvw xyz/*` = `abc def/*` + 123 /* comment `abc` abc */ + 456")
         (exps "22203333333333301033333333333010222011022222220333330222011010222") ;; expected codes of syntax class
         (expf "KKK-VVVVVVVVVVV-K---------------CCC-DDDOOOOOOOOOOOOOOOOOOOO---CCC") ;; expected font-locks
         (line-length (length line)))
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
    (_ "?")))
