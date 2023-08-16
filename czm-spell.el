;;; czm-spell.el --- Spell-check that saves corrections in abbrevs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/czm-spell.el
;; Package-Requires: ((emacs "25.1") (auctex "11.86.1"))
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

;; This packages OVERRIDES `ispell-command-loop' from `ispell.el' to
;; provides a no-frills spell-check function `czm-spell-then-abbrev'
;; that searches backwards in the visible buffer for a misspelled
;; word, offers a list of corrections, and offers to save the chosen
;; correction in the abbrev table.  It is designed to work well with
;; TeX buffers: it will not offer corrections for words inside math
;; environments, citations, labels or commands.
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
(require 'latex)
(require 'flyspell)

(defvar czm-spell--save-as-abbrev t
  "If non-nil, save the correction as an abbrev.
Used internally.")

;;;###autoload
(defun czm-spell-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev.  Otherwise it will be global.
If there's nothing wrong with the word at point, keep looking for
a typo until the beginning of the visible window.  You can skip
typos you don't want to fix with `SPC', and you can abort
completely with `C-g'."
  (interactive "P")
  (setq czm-spell--save-as-abbrev t)
  (let ((start (window-start))
 	bef aft)
    (save-excursion
      (backward-word)
      (while (if (and
		  (>= (point) start)
		  (setq bef (car-safe (save-excursion (ignore-errors (ispell-get-word t)))))
		  (looking-at bef)
		  (not (or
			(texmathp)
                        (and
                         ; in latex mode
                         (eq major-mode 'latex-mode)
			 (TeX-in-comment))
			(czm-spell--inside-ref-label-or-cite-p)
			(looking-back
			 (regexp-opt
			  (list
			   "\\" "[" "{"))
			 (1- (point))))))
                 ;; Word was corrected or used quit.
		 (let ((result (ispell-word t 'quiet)))
		   (if result
		       (progn
			 (when (not (eq result 'quit))
			   (setq aft
				 (if (stringp result)
				     result
				   (car result))))
			 nil)		; End the loop.
		     ;; Also end if we reach `bob'.
		     (not (bobp))))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
	  (when czm-spell--save-as-abbrev
	    ;; (y-or-n-p (format "Expand \"%s\" to \"%s\"? " bef aft))
            (define-abbrev
	      (if p local-abbrev-table global-abbrev-table)
	      bef aft)
            (message "\"%s\" now expands to \"%s\" %sally"
                     bef aft (if p "loc" "glob"))))
      (user-error "No typo at or before point"))))

(defun czm-spell--inside-ref-label-or-cite-p ()
  "Determine if point is in a reference, label, or citation."
  (interactive)
  (save-excursion
    (let* ((cur-point (point))
	   (start-of-line (line-beginning-position))
	   (open-command (search-backward-regexp
			  (concat
			   "\\\\"
			   (regexp-opt
			    '("eqref" "ref" "href" "label" "cite" "begin" "end")))
			  start-of-line t)))
      (if (and open-command (< open-command cur-point))
	  (progn
	    (goto-char cur-point)
	    (if (search-backward "}" open-command t)
		nil
	      t))
	nil))))

(advice-add 'ispell-command-loop :override #'czm-spell-override-ispell-command-loop)

(defun czm-spell-override-ispell-command-loop (miss guess word start end)
  "Display possible corrections from list MISS.

Override for ispell-command-loop.
This modified function adds an additional command character
\`C-c' that temporarily disables storing corrections as abbrevs.

Original documentation:
GUESS lists possibly valid affix construction of WORD.
Returns nil to keep word.
Returns 0 to insert locally into buffer-local dictionary.
Returns string for new chosen word.
Returns list for new replacement word (will be rechecked).
  Query-replace when list length is 2.
  Automatic query-replace when second element is `'query-replace'.
Highlights the word, which is assumed to run from START to END.
Global `ispell-pdict-modified-p' becomes a list where the only value
indicates whether the dictionary has been modified when option `a'
or `i' is used.
Global `ispell-quit' set to start location to continue spell session."
  (let ((count ?0)
	(choices miss)
	(window-min-height (min window-min-height
				ispell-choices-win-default-height))
	(command-characters '( ?  ?i ?a ?A ?r ?R ?? ?x ?X ?q ?l ?u ?m ?\C-c))
	(skipped 0)
	char num result textwin)

    ;; setup the *Choices* buffer with valid data.
    (with-current-buffer (get-buffer-create ispell-choices-buffer)
      (setq mode-line-format
	    (concat
             "--  %b  --  word: " word
             "  --  dict: " (or ispell-current-dictionary "default")
             "  --  prog: " (file-name-nondirectory ispell-program-name)))
      ;; No need for horizontal scrollbar in choices window
      (with-no-warnings
	(setq horizontal-scroll-bar nil))
      (erase-buffer)
      (if guess
	  (progn
	    (insert "Affix rules generate and capitalize "
		    "this word as shown below:\n\t")
	    (while guess
	      (when (> (+ 4 (current-column) (length (car guess)))
		       (window-width))
		(insert "\n\t"))
	      (insert (car guess) "    ")
	      (setq guess (cdr guess)))
	    (insert (substitute-command-keys
		     "\nUse option `i' to accept this spelling and put it in your private dictionary.\n"))))
      (while choices
	(when (> (+ 7 (current-column)
		    (length (car choices))
		    (if (> count ?~) 3 0))
		 (window-width))
	  (insert "\n"))
	;; not so good if there are over 20 or 30 options, but then, if
	;; there are that many you don't want to scan them all anyway...
	(while (memq count command-characters) ; skip command characters.
	  (setq count (1+ count)
		skipped (1+ skipped)))
	(insert "(" count ") " (car choices) "  ")
	(setq choices (cdr choices)
	      count (1+ count)))
      (setq count (- count ?0 skipped)))

    (run-hooks 'ispell-update-post-hook)

    ;; ensure word is visible
    (if (not (pos-visible-in-window-group-p end))
	(sit-for 0))

    ;; Display choices for misspelled word.
    (setq textwin (selected-window))
    (ispell-show-choices)
    (select-window textwin)

    ;; highlight word, protecting current buffer status
    (unwind-protect
	(progn
	  (and ispell-highlight-p
	       (ispell-highlight-spelling-error start end t))
	  ;; Loop until a valid choice is made.
	  (while
	      (eq
	       t
	       (setq
		result
		(progn
		  (undo-boundary)
		  (let (message-log-max)
		    (message (concat "C-h or ? for more options; SPC to leave "
				     "unchanged, Character to replace word")))
		  (let ((inhibit-quit t)
			(input-valid t))
		    (setq char nil skipped 0)
		    ;; If the user types C-g, or generates some other
		    ;; non-character event (such as a frame switch
		    ;; event), stop ispell.  As a special exception,
		    ;; ignore mouse events occurring in the same frame.
		    (while (and input-valid (not (characterp char)))
		      (setq char (read-key))
		      (setq input-valid
			    (or (characterp char)
				(and (mouse-event-p char)
				     (eq (selected-frame)
					 (window-frame
					  (posn-window (event-start char))))))))
		    (when (or quit-flag (not input-valid) (= char ?\C-g))
		      (setq char ?X quit-flag nil)))
		  ;; Adjust num to array offset skipping command characters.
		  (let ((com-chars command-characters))
		    (while com-chars
		      (if (and (> (car com-chars) ?0) (< (car com-chars) char))
			  (setq skipped (1+ skipped)))
		      (setq com-chars (cdr com-chars)))
		    (setq num (- char ?0 skipped)))

		  (cond
		   ((= char ? ) nil)	; accept word this time only
		   ((= char ?i)	; accept and insert word into pers dict
		    (ispell-send-string (concat "*" word "\n"))
		    (setq ispell-pdict-modified-p '(t)) ; dictionary modified!

                    (when flyspell-mode
                      (flyspell-unhighlight-at start))
		    nil)
		   ((or (= char ?a) (= char ?A)) ; accept word without insert
		    (ispell-send-string (concat "@" word "\n"))
		    (cl-pushnew word ispell-buffer-session-localwords
                                :test #'equal)
		    (when flyspell-mode
                      (flyspell-unhighlight-at start))
		    (or ispell-buffer-local-name ; session localwords might conflict
			(setq ispell-buffer-local-name (buffer-name)))
		    (if (null ispell-pdict-modified-p)
			(setq ispell-pdict-modified-p
			      (list ispell-pdict-modified-p)))
		    (if (= char ?A) 0))	; return 0 for ispell-add buffer-local
		   ((or (= char ?r) (= char ?R)) ; type in replacement
		    (and (eq 'block ispell-highlight-p) ; refresh tty's
			 (ispell-highlight-spelling-error start end nil t))
		    (let ((result
			   (if (or (= char ?R) ispell-query-replace-choices)
			       (list (read-string
				      (format "Query-replacement for %s: "word)
				      word)
				     t)
			     (cons (read-string "Replacement for: " word)
				   nil))))
		      (and (eq 'block ispell-highlight-p)
			   (ispell-highlight-spelling-error start end nil
							    'block))
		      result))
		   ((or (= char ??) (= char help-char) (= char ?\C-h))
		    (and (eq 'block ispell-highlight-p)
			 (ispell-highlight-spelling-error start end nil t))
		    (ispell-help)
		    (and (eq 'block ispell-highlight-p)
			 (ispell-highlight-spelling-error start end nil
							  'block))
		    t)
		   ;; Quit and move point back.
		   ((= char ?x)
		    (ispell-pdict-save ispell-silently-savep)
		    (message "Exited spell-checking")
		    (setq ispell-quit t)
		    nil)
		   ;; Quit and preserve point.
		   ((= char ?X)
		    (ispell-pdict-save ispell-silently-savep)
		    (message "%s"
			     (substitute-command-keys
			      (concat
			       "Spell-checking suspended; use "
			       "\\[universal-argument] \\[ispell-word] to resume")))
		    (setq ispell-quit start)
		    nil)
		   ((= char ?q)
		    (if (y-or-n-p "Really kill Ispell process? ")
			(progn
			  (ispell-kill-ispell t) ; terminate process.
			  (setq ispell-quit (or (not ispell-checking-message)
						(point))
				ispell-pdict-modified-p nil))
		      t))		; continue if they don't quit.
		   ((= char ?l)
		    (and (eq 'block ispell-highlight-p) ; refresh tty displays
			 (ispell-highlight-spelling-error start end nil t))
		    (let ((new-word (read-string
				     "Lookup string (`*' is wildcard): "
				     word)))
		      (if new-word
			  (progn
			    (with-current-buffer (get-buffer-create
                                                  ispell-choices-buffer)
			      (erase-buffer)
			      (setq count ?0
				    skipped 0
				    mode-line-format ;; setup the *Choices* buffer with valid data.
				    (concat "--  %b  --  word: " new-word
					    "  --  word-list: "
					    (or ispell-complete-word-dict
						ispell-alternate-dictionary))
				    miss (ispell-lookup-words new-word)
				    choices miss)
			      (while choices
				(when (> (+ 7 (current-column)
					    (length (car choices))
					    (if (> count ?~) 3 0))
					 (window-width))
				  (insert "\n"))
				(while (memq count command-characters)
				  (setq count (1+ count)
					skipped (1+ skipped)))
				(insert "(" count ") " (car choices) "  ")
				(setq choices (cdr choices)
				      count (1+ count)))
			      (setq count (- count ?0 skipped)))
			    (setq textwin (selected-window))
			    (ispell-show-choices)
			    (select-window textwin))))
		    (and (eq 'block ispell-highlight-p)
			 (ispell-highlight-spelling-error start end nil
							  'block))
		    t)		    ; reselect from new choices
		   ((= char ?u)	    ; insert lowercase into dictionary
		    (ispell-send-string (concat "*" (downcase word) "\n"))
		    (setq ispell-pdict-modified-p '(t)) ; dictionary modified!
		    nil)
		   ((= char ?m)		; type in what to insert
		    (ispell-send-string
		     (concat "*" (read-string "Insert: " word) "\n"))
		    (setq ispell-pdict-modified-p '(t))
		    (cons word nil))
		   ((and (>= num 0) (< num count))
		    (if ispell-query-replace-choices ; Query replace flag
			(list (nth num miss) 'query-replace)
		      (nth num miss)))
		   ((= char ?\C-l)
		    (redraw-display) t)
		   ((= char ?\C-r)
		    ;; This may have alignment errors if current line is edited
		    (if (marker-position ispell-recursive-edit-marker)
			(progn
			  (message "Only one recursive edit session supported")
			  (beep)
			  (sit-for 2))
		      (set-marker ispell-recursive-edit-marker start)
		      ;;(set-marker ispell-region-end reg-end)
		      (and ispell-highlight-p ; unhighlight
			   (ispell-highlight-spelling-error start end))
		      (unwind-protect
			  (progn
			    (message
			     "%s"
			     (substitute-command-keys
			      (concat "Exit recursive edit with"
				      " \\[exit-recursive-edit]")))
			    (save-window-excursion (save-excursion
						     (recursive-edit))))
			;; protected
			(goto-char ispell-recursive-edit-marker)
			(if (not (equal (marker-buffer
					 ispell-recursive-edit-marker)
					(current-buffer)))
			    (progn
			      (set-marker ispell-recursive-edit-marker nil)
			      (error
			       "Cannot continue ispell from this buffer")))
			(set-marker ispell-recursive-edit-marker nil)))
		    (list word nil))  ; recheck starting at this word.
		   ((= char ?\C-z)
		    (funcall (key-binding "\C-z"))
		    t)
		   ((= char ?\C-c)
		    (setq czm-spell--save-as-abbrev nil)
		    (message "Abbrev-saving temporarily disabled")
		    t)
		   (t (ding) t))))))
	  result)
      ;; protected
      (and ispell-highlight-p		; unhighlight
	   (save-window-excursion
	     (select-window textwin)
	     (ispell-highlight-spelling-error start end))))))

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
