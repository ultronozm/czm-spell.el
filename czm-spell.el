;;; czm-spell.el --- Spell-check helper  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-spell.el
;; Package-Requires: ((emacs "25.1") (auctex))
;; Keywords: tex, tools, convenience

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
;; buffer for a misspelled word and offers a list of corrections.  It
;; is designed to work well with TeX buffers: it will not offer
;; corrections for words inside math environments, citations, labels
;; or commands.
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
;;  TODO: compare with flyspell?  Maybe something?

;;; Code:

(require 'ispell)
(require 'tex)

;;;###autoload
(defun czm-spell-then-abbrev (p)
  "Spell-check word at/before point.

Prefix argument P is accepted for backward compatibility but currently ignored.
If there's nothing wrong with the word at point, keep looking for
a typo until the beginning of the visible window.  You can skip
typos you don't want to fix with `SPC', and you can abort
completely with `C-g'."
  (interactive "P")
  (ignore p)
  (let ((start (window-start))
        corrected)
    (save-excursion
      (backward-word)
      (while (if (and
                  (>= (point) start)
                  (let ((bef (car-safe
                              (save-excursion
                                (ignore-errors
                                  (ispell-get-word t))))))
                    (and
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
                            (1- (point))))))))
                 ;; Word was corrected or user quit.
                 (let ((result (ispell-word t 'quiet)))
                   (cond
                    ((eq result 'quit) nil)
                    (result (setq corrected t) nil)
                    (t (not (bobp)))))
               ;; If there's no word at point, keep looking until `bob'.
               (not (bobp)))
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

(provide 'czm-spell)
;;; czm-spell.el ends here
