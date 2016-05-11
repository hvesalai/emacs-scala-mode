;;; scala-mode-prettify.el - Extension of scala-mode for prettifying scala symbols/code
;;; Copyright (c) 2016 Merlin Göttlinger
;;; For information on the License, see the LICENSE file

(defconst scala-mode-pretty-bool-alist '(
					 ("<=" . ?≤)
					 (">=" . ?≥)
					 ("==" . ?≡)
					 ("!" . ?¬)
					 ("!=" . ?≢)
					 ("&&" . ?∧)
					 ("||" . ?∨)
					 ("true" . ?⊤)
					 ("false" . ?⊥)
					 ("Boolean" . ?𝔹)))

(defconst scala-mode-pretty-collection-alist '(
					       ("empty" . ?∅)
					       ("sum" . ?∑)
					       ("contains" . ?∍)
					       ("++" . ?⧺)
					       ("::" . ?⸬)
					       ("--" . ?╌)))

(defconst scala-mode-pretty-arrows-alist'(
					  ("->" . ?→)
					  ("<-" . ?←)
					  ("=>" . ?⇒)
					;("<=" . ?⇐)
					  ("<=>" . ?⇔)
					  ("-->" . ?⟶)
					  ("<->" . ?↔)
					  ("<--" . ?⟵)
					  ("<-->" . ?⟷)
					  ("==>" . ?⟹)
					  ("<==" . ?⟸)
					  ("<==>" . ?⟺)
					  ("~>" . ?⇝)
					  ("<~" . ?⇜)))

(defconst scala-mode-pretty-misc-alist '(
					 ("null" . ?∅)
					 ("Nothing" . ?∅)
					 ("Unit" . ?∅)
					 ("Int" . ?ℤ)
					 (":=" . ?≔)))

(defconst scala-mode-pretty-all-alist (append
				       scala-mode-pretty-bool-alist
				       scala-mode-pretty-collection-alist
				       scala-mode-pretty-arrows-alist
				       scala-mode-pretty-misc-alist))

(provide 'scala-mode2-prettify)
