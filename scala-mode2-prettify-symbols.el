;;; scala-mode2-prettify-symbols.el - Extension of scala-mode for prettifying scala symbols/code
;;; Copyright (c) 2016 Merlin Göttlinger
;;; For information on the License, see the LICENSE file
;; -*- coding: UTF-8 -*-

(defcustom scala-mode-pretty-bool-alist '(
					  ("<=" . ?≤)
					  (">=" . ?≥)
					  ("==" . ?≡)
					  ("===" . ?≣)
					  ("!" . ?¬)
					  ("!=" . ?≢)
					  ("&&" . ?∧)
					  ("||" . ?∨)
					  ("true" . ?⊤)
					  ("false" . ?⊥)
					  ("Boolean" . ?𝔹))
  "Prettify rules for boolean related operations."
  :type 'alist)

(defcustom scala-mode-pretty-collection-alist '(
						("empty" . ?∅)
						("sum" . ?∑)
						("product" . ?∏)
						("contains" . ?∍)
						("forall" . ?∀)
						("any" . ?∃)
						("intersect" . ?∩)
						("union" . ?∪)
						("diff" . ?≏)
						("subsetOf" . ?⊆)
						("++" . ?⧺)
						("::" . ?⸬)
						("--" . ?╌))
  "Prettify rules for collections related operations."
  :type 'alist)

(defcustom scala-mode-pretty-arrows-alist'(
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
					   ("<~" . ?⇜))
  "Prettify rules for arrow related code pieces."
  :type 'alist)

(defcustom scala-mode-pretty-misc-alist '(
					;("null" . ?∅)
					;("Nothing" . ?∅)
					  ("Unit" . ?∅)
					  ("Int" . ?ℤ)
					  ("assert" . ?⊦)
					  (":=" . ?≔))
  "Prettify rules for other mixed code pieces."
  :type 'alist)

(defcustom scala--prettify-symbols-alist (append
					  scala-mode-pretty-bool-alist
					  scala-mode-pretty-collection-alist
					  scala-mode-pretty-arrows-alist
					  scala-mode-pretty-misc-alist)
  "All prettify rules to be applied in scala code."
  :type 'alist)

(provide 'scala-mode2-prettify-symbols)
