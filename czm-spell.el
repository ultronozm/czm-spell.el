;;; czm-spell.el --- Spell-check that saves corrections in abbrevs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-spell.el
;; Package-Requires: ((emacs "25.1") (auctex))
;; Keywords: tex, tools, abbrev, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a no-frills spell-check function
;; `czm-spell-then-abbrev' that searches backwards in the visible
;; buffer for a misspelled word, offers a list of corrections, and
;; uses the native `ispell-save-corrections-to-abbrev' option to store
;; the chosen correction in the abbrev table.  It is designed to work
;; well with TeX buffers: it will not offer corrections for words
;; inside math environments, citations, labels or commands.
;; 
;; Some of the code here was adapted from
;; https://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html,
;; with some TeX-specific tweaks.
;;
;; My use-package declaration:
;; 
;; (use-package czm-spell
;;   :vc (:url "https://github.com/ultronozm/czm-spell.el.git"
;; 	    :rev :newest)
;;   :bind ("s-;" . czm-spell-then-abbrev))
;;
;;
;;  TODO: compare with (flyspell-abbrev-p t)?  Maybe something?

;;; Code:

(require 'ispell)
(require 'tex)

;;;###autoload
(defun czm-spell-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev.  Otherwise it will be global.
If there's nothing wrong with the word at point, keep looking for
a typo until the beginning of the visible window.  You can skip
typos you don't want to fix with `SPC', and you can abort
completely with `C-g'."
  (interactive "P")
  (let ((ispell-save-corrections-to-abbrev t)
        (ispell-use-global-abbrev-table (not p))
        (start (window-start))
        corrected)
    (save-excursion
      (backward-word)
      (while
          (let ((bef (car-safe
                      (save-excursion
                        (ignore-errors
                          (ispell-get-word t))))))
            (if (and
                 (>= (point) start)
                 bef
                 (looking-at bef)
                 (not (or
                       (let ((face (plist-get (text-properties-at (point))
                                              'face)))
                         (or
                          (memq face '(tex-math font-latex-math-face))
                          (and (listp face)
                               (or (memq 'tex-math face)
                                   (memq 'font-latex-math-face face)))))
                       (and
                        (or (eq major-mode 'latex-mode)
                            (eq major-mode 'LaTeX-mode))
                        (or (TeX-in-comment)
                            (czm-spell--inside-ref-label-or-cite-p)))
                       (looking-back
                        (regexp-opt
                         (list
                          "\\" "[" "{" "}"))
                        (1- (point))))))
                ;; Word was corrected or user quit.
                (let ((result (ispell-word t 'quiet)))
                  (cond
                   ((eq result 'quit) nil)
                   (result (setq corrected t)
                           nil)
                   (t
                    ;; Also end if we reach `bob'.
                    (not (bobp)))))
              ;; If there's no word at point, keep looking until `bob'.
              (not (bobp))))
        (backward-word)))
    (unless corrected
      (user-error "No typo at or before point"))))

(defun czm-spell--inside-ref-label-or-cite-p ()
  "Determine if point is in a reference, label, or citation."
  (interactive)
  (save-excursion
    (let* ((cur-point (point))
	          (start-of-line (line-beginning-position))
	          (open-command
            (let ((case-fold-search nil))
              (search-backward-regexp
			            (concat
			             "\\\\"
			             (regexp-opt
			              '("eqref" "ref" "href" "label" "cite" "begin" "end")))
			            start-of-line t))))
      (if (and open-command (< open-command cur-point))
	         (progn
	           (goto-char cur-point)
	           (if (search-backward "}" open-command t)
		              nil
	             t))
	       nil))))



(defun czm-spell-setup ()
  "Setup czm-spell."
  (setq ispell-tex-skip-alists
	'((("\\\\\\(hyperref\\|specialrule\\)" ispell-tex-arg-end 3)
	   ("\\\\\\(C\\(?:pagerefrange\\|refrange\\*?\\)\\|LTXtable\\|SI\\|c\\(?:pagerefrange\\|refrange\\*?\\)\\|hyperdef\\|inputminted\\|mdfapptodefinestyle\\|v\\(?:pagerefrange\\*?\\|refrange\\*?\\)\\)" ispell-tex-arg-end 2)
	   ("\\\\\\(Ac\\(?:f\\(?:p\\*\\|[*p]\\)\\|l\\(?:p\\*\\|[*p]\\)\\|p\\*\\|single\\*?\\|[*flp]\\)?\\|C\\(?:pageref\\|ref\\*?\\)\\|DeleteShortVerb\\|Fref\\|Iac\\|MakeShortVerb\\*?\\|Nameref\\|Ref\\|V\\(?:erbatimInput\\|ref\\)\\|a\\(?:c\\(?:a\\(?:p\\*\\|[*p]\\)\\|f\\(?:like\\*?\\|p\\(?:\\*\\|like\\*?\\)\\|[*p]\\)\\|l\\(?:p\\*\\|[*p]\\)\\|p\\*\\|reset\\|s\\(?:etup\\|ingle\\*?\\|p\\*\\|[*p]\\)\\|use\\|[*aflps]\\)?\\|ddfontfeatures\\|ttachfile\\(?:setup\\)?\\|uto\\(?:pageref\\*?\\|ref\\*?\\)\\)\\|c\\(?:aption\\(?:of\\*?\\|setup\\*?\\)\\|learcaptionsetup\\*?\\|pageref\\|ref\\*?\\|s\\)\\|empheqset\\|f\\(?:igureversion\\|ref\\|ullref\\|vset\\)\\|h\\(?:ref\\|yper\\(?:baseurl\\|image\\|link\\|setup\\|target\\)\\)\\|i\\(?:ac[ls]\\|nputpygments\\)\\|l\\(?:abelcref\\|cnamecref\\|st\\(?:inputlisting\\|set\\)\\)\\|mdfsetup\\|n\\(?:ame\\(?:[Cc]?ref\\)\\|olinkurl\\|um\\)\\|s\\(?:et\\(?:minted\\(?:inline\\)?\\|py\\(?:gments\\(?:fv\\|pygopt\\)\\|thontexfv\\)\\)\\|i\\(?:ndex\\|setup\\)?\\)\\|t\\(?:cbset\\(?:foreverylayer\\)?\\|extattachfile\\)\\|u\\(?:rl\\|se\\(?:\\(?:prin\\|stdou\\)tpythontex\\)\\)\\|v\\(?:pageref\\*?\\|ref\\*?\\)\\)" ispell-tex-arg-end)
	   ("\\\\\\(addlinespace\\|captionlistentry\\|tcb\\(?:item\\|ox\\)\\)" ispell-tex-arg-end 0)
	   ("\\\\\\(?:mint\\(?:inline\\)?\\|pygment\\)" TeX-ispell-tex-arg-verb-end 1)
	   ("\\\\\\(?:Verb\\|lstinline\\|py\\(?:con[cv]?\\|lab\\(?:con[cv]?\\|[bcsv]\\)?\\|[bcsv]\\)?\\|sympy\\(?:con[cv]?\\|[bcsv]\\)?\\)" TeX-ispell-tex-arg-verb-end)
	   ("\\\\fontspec" TeX-ispell-tex-arg-end 1 1 0)
	   ("\\\\cmidrule" . "{[-0-9]+}")
	   ("\\\\raisebox" TeX-ispell-tex-arg-end 1 2 0)
	   ("\\\\(" . "\\\\)")
	   ("\\\\addcontentsline" . #3=(ispell-tex-arg-end 2))
	   ("\\\\add\\(tocontents\\|vspace\\)" . #1=(ispell-tex-arg-end))
	   ("\\\\\\([aA]lph\\|arabic\\)" . #1#)
	   ("\\\\cref" . #1#)
	   ("\\\\eqref" . #1#)
	   ("\\\\cite" . #1#)
	   ("\\\\bibliographystyle" . #1#)
	   ("\\\\makebox" . #2=(ispell-tex-arg-end 0))
	   ("\\\\e?psfig" . #1#)
	   ("\\\\document\\(class\\|style\\)" . "\\\\begin[ 	
]*{document}"))
	  (("\\(BVerbatim\\*?\\|LVerbatim\\*?\\|SaveVerbatim\\|Verbatim\\(?:\\*\\|Out\\)?\\|align\\(?:\\*\\|at\\*?\\)?\\|d\\(?:array\\*?\\|group\\*?\\|math\\*?\\|series\\*?\\)\\|empheq\\|flalign\\*?\\|gather\\*?\\|lstlisting\\|m\\(?:inted\\|ultline\\*?\\)\\|py\\(?:block\\|co\\(?:de\\|n\\(?:code\\|sole\\|verbatim\\)\\)\\|gments\\|lab\\(?:block\\|co\\(?:de\\|n\\(?:code\\|sole\\|verbatim\\)\\)\\|sub\\|verbatim\\)\\|sub\\|verbatim\\)\\|sympy\\(?:block\\|co\\(?:de\\|n\\(?:code\\|sole\\|verbatim\\)\\)\\|sub\\|verbatim\\)\\|tikzpicture\\)" . "\\\\end{\\(BVerbatim\\*?\\|LVerbatim\\*?\\|SaveVerbatim\\|Verbatim\\(?:\\*\\|Out\\)?\\|align\\(?:\\*\\|at\\*?\\)?\\|d\\(?:array\\*?\\|group\\*?\\|math\\*?\\|series\\*?\\)\\|empheq\\|flalign\\*?\\|gather\\*?\\|lstlisting\\|m\\(?:inted\\|ultline\\*?\\)\\|py\\(?:block\\|co\\(?:de\\|n\\(?:code\\|sole\\|verbatim\\)\\)\\|gments\\|lab\\(?:block\\|co\\(?:de\\|n\\(?:code\\|sole\\|verbatim\\)\\)\\|sub\\|verbatim\\)\\|sub\\|verbatim\\)\\|sympy\\(?:block\\|co\\(?:de\\|n\\(?:code\\|sole\\|verbatim\\)\\)\\|sub\\|verbatim\\)\\|tikzpicture\\)}")
	   ("\\(description\\*?\\|enumerate\\*?\\|itemize\\*?\\|mdframed\\|tc\\(?:b\\(?:itemize\\|raster\\)\\|olorbox\\)\\)" ispell-tex-arg-end 0)
	   ("xltabular" ispell-tex-arg-end 2)
	   ("tcboxed\\(raster\\|itemize\\)" ispell-tex-arg-end)
	   ("tabular[*xy]" TeX-ispell-tex-arg-end)
	   ("stabular\\*" TeX-ispell-tex-arg-end)
	   ("stabular" ispell-tex-arg-end)
	   ("equation" ispell-tex-arg-end)
	   ("equation\\*" ispell-tex-arg-end)
	   ("filecontents\\*?" ispell-tex-arg-end)
	   ("\\(figure\\|table\\)\\*?" . #2#)
	   ("list" . #3#)
	   ("program" . "\\\\end[ 	]*{program}")
	   ("verbatim\\*?" . "\\\\end[ 	]*{verbatim\\*?}")))))


(provide 'czm-spell)
;;; czm-spell.el ends here
