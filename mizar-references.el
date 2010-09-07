(require 'mizar)

;;; Gathering references

(defvar mizar-by-refs-regexp "[ ]+by[ ]+[a-zA-Z:_0-9 ]+\\(,[a-zA-Z:_0-9 ]+\\)*;?")

(defvar mizar-from-refs-regexp "[ ]+from[ ]+[a-zA-Z:_0-9 ]+\\(,[a-zA-Z:_0-9 ]+\\)*;?")

(defun mizar-split-refs (references)
  "Take a list REFERENCES of references and split them up."
  (let ((refs))
    (dolist (ref-line references (remove-duplicates refs :test 'equal))
      (dolist (ref (split-string ref-line ","))
	(push ref refs)))))

(defun mizar-gather-refs (beg end)
  "Gather references in the region delimited by BEG and END."
  (interactive "r")
  (let ((refs)
	(beg (or beg (point-min)))
	(end (or end (point-max))))
    (save-excursion
      (let ((pos (goto-char beg)))
	(while (and pos (<= pos end))
	  (setq pos (re-search-forward mizar-by-refs-regexp end t))
	  (when pos
	    (let ((match (match-string-no-properties 0)))
	      (setq match (substring match 4))
	      (when (eq (elt match (1- (length match))) ?\;)
		(setq match (substring match 0 -1)))
	      (push match refs))))))
  (mizar-split-refs refs)))

(defun mizar-non-local-ref (ref)
  "Determine whether REF is a non-local reference."
  (or (string< "Lm" ref)
      (string< "Th" ref)
      (string< "Def" ref)
      (find ?: ref)))

(defun mizar-library-ref (ref)
  "Determine whether the reference REF is a library reference."
  (find ?: ref))

(defun mizar-intra-article-ref (ref)
  "Determine whether reference REF is an intra-article
reference (refers to a theorem or lemma or definition from the
  current article."
  (and (mizar-non-local-ref ref)
       (not (mizar-library-ref ref))))

(defun mizar-gather-non-local-refs (beg end)
  "Gather all the non-local refs in the region delimited by BEG and END."
  (interactive "r")
  (let ((beg (or beg (point-min)))
	(end (or end (point-max)))
	(refs))
    (let ((all-refs (mizar-gather-refs beg end)))
      (dolist (ref all-refs refs)
	(when (mizar-non-local-ref ref)
	  (push ref refs)))))) 

(defun mizar-gather-intra-article-refs (beg end)
  "Gather all the non-local intra-article references in the
region delimited by BEG and END."
  (interactive "r")
  (let ((refs nil))
    (dolist (ref (mizar-gather-non-local-refs beg end) refs)
      (unless (mizar-library-ref ref)
	(push ref refs)))))

;;; Keeping labels sane

(defvar mizar-last-theorem-label 0)

(defvar mizar-theorem-regexp "theorem[ \n\t]+Th\\([0-9]+\\):")

(defun mizar-author-move-to-nearest-uncommented-position ()
  (while (re-search-backward "[:][:]" (line-beginning-position) t)
    (backward-char)))

(defun mizar-author-within-comment ()
  "Determine whether we are currently within the scope of a comment."
  (let ((now (point))
	(later))
    (save-excursion
      (mizar-author-move-to-nearest-uncommented-position)
      (setq later (point)))
    (not (eq now later))))

(defun mizar-scan-theorem-labels-in-region (beg end)
  "Scan the theorems in the region delimited by buffer positions
BEG and END, gathering their labels along the way.  Set
`mizar-last-theorem-label' to the max of the labels we
encountered."
  (let ((labels))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "[ \t]*theorem[ \t\n]+\\(Th[0-9]+\\):" end t)
	(unless (mizar-author-within-comment)
	  (push (cons (match-string-no-properties 1) (point)) labels))))
    labels))

(defun mizar-scan-theorem-labels-in-buffer ()
  "Scan the theorems in the current buffer, gathering their
labels along the way.  Set `mizar-last-theorem-label' to the max
of the labels we encountered."
  (mizar-scan-theorem-labels-in-region (point-min) (point-max)))

(defun mizar-scan-theorem-labels-following-point ()
  "Scan the theorems in the current buffer, gathering their
labels along the way, starting at point and going to the end of
the buffer."
  (mizar-scan-theorem-labels-in-region (point) (point-max)))

;; "theorem[ \\n\\t]+Th[0-9]+:"

(defun mizar-scan-lemma-labels ()
  "Scan the lemmas in the current buffer, gathering their labels
along the way.  Set `mizar-last-lemma-label' to the max of the
labels we encountered."
  (let ((labels))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward "Lm\\(.+\\):" nil t)
	(push (string-to-number (match-string-no-properties 1)) labels)))
    (setq mizar-last-lemma-label (apply 'max labels))
    labels))

(defun mizar-check-theorem-label-consistency ()
  "Check the theorem label consistency."
  (let ((labels (mapcar #'cdr (mizar-scan-theorem-labels-in-buffer)))
	(duplicate-labels))
    (while labels
      (let ((label (car labels)))
	(when (member label (cdr labels))
	  (push label duplicate-labels))
	(setq labels (cdr labels))))
    duplicate-labels))

(defun mizar-relabel-theorem (new-label)
  "Relabel the current theorem with the label NEW-LABEL."
)

(defun mizar-find-uses (label)
  "Find the places where LABEL is used in a justification."
  (let ((regex (concat "[ \t]+by[ \t]+.*" label "[^0-9]"))
	(positions)
	(len (length label)))
    (message "regex = %S" regex)
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward regex nil t)
	(push (- (match-end 0) len) positions)))
    positions))

(defun mizar-find-theorem-definition (label)
  "Find where the theorem whose label is LABEL is defined."
  (let ((pos)
	(regex (concat "theorem[ \n\t]+" label ":")))
    (save-excursion
      (beginning-of-buffer)
      (when (re-search-forward regex)
	(setq pos (match-beginning 0))))
    pos))

(defun mizar-find-lemma-definition (label)
  "Find where the lemma whose label is LABEL is defined."
  (let ((pos)
	(regex (concat "^" label ":")))
    (save-excursion
      (beginning-of-buffer)
      (when (re-search-forward regex)
	(setq pos (match-beginning 0))))
    pos))  

(defun mizar-find-definition-definition (label)
  "Find where the definition whose label is LABEL is defined."
  (let ((pos)
	(regex (concat ":" label ":")))
    (save-excursion
      (beginning-of-buffer)
      (when (re-search-forward regex)
	(setq pos (match-beginning 0))))
    pos))  

(defun mizar-find-definition (label)
  "Find the buffer position of the definition of the item whose
label is LABEL."
  (cond ((string= "Def" (substring label 0 3))
	 (mizar-find-definition-definition label))
	((string= "Lm" (substring label 0 2))
	 (mizar-find-lemma-definition label))
	(t ;; theorem
	 (mizar-find-theorem-definition label))))

(defun mizar-theorem-proof (label)
  "Return the proof of the theorem whose label is LABEL."
  (let ((pos (mizar-find-theorem-definition label)))
    (when (numberp pos)
      (save-excursion
	(goto-char pos)
	(re-search-forward (concat label ":[ \t][^ \t]") nil t)
	(goto-char (1- (match-end 0))) ;; I wish I knew how to
				       ;; complete this.
	))))

(defun mizar-next-theorem-citation (label)
  "Look for the next use of LABEL in the current article."
  (re-search-forward (concat " by \\([^,]+,\\)*" label) nil t))

(defun mizar-new-theorem ()
  "Make a new theorem."
  (interactive)
  (unless (bolp)
    (end-of-line)
    (newline 2))
  (insert "theorem")
  (newline)
  (incf mizar-last-theorem-label)
  (insert "Th" (number-to-string mizar-last-theorem-label) ": "))


(defun mizar-new-definition ()
  "Make a new definition."
  (unless (bolp)
    (end-of-line)
    (newline 2))
  (insert "definition" " "))

(defun mizar-new-registration ()
  "Insert a new registration."
  (unless (bolp)
    (end-of-line)
    (newline 2))
  (insert "registration" " "))

(defun mizar-new-reservation ()
  "Insert a new reservation."
  (unless (bolp)
    (end-of-line)
    (newline 2))
  (insert "reserve" " "))

(defun mizar-new-proof ()
  "Insert a new proof."
  (insert "proof")
  (newline)
  (insert "end" ";")
  (beginning-of-line)
  (backward-char 1)
  (indent-according-to-mode))
  
(defun mizar-next-theorem ()
  "Go to the next theorem in the current article."
  (when (looking-at "[^;]*theorem")
    (forward-char 1))
  (if (re-search-forward "[^;]*theorem" nil t)
      (backward-char 7)
    (backward-char 1)))

(defun mizar-previous-theorem ()
  "Go to the previous theorem in the current article."
  (re-search-backward "[^;]*theorem" nil t))

(defun mizar-current-theorem-labeledp ()
  "Determine whether the current theorem is labeled."
  ;; I ignore the question of whether we are "within" a theorem
  (if (looking-at "[^;]*theorem")
      (re-search-forward "[^;]theorem[ 
]+Th[^;]+:" nil t)
    (re-search-backward "[^;]*theorem[ 
]+Th[^;]+:" nil t)))
    

(defun mizar-get-current-theorem-label ()
  "Get the label of the current theorem."
  ;; I assume that we are within a theorem; I don't want to deal right
  ;; now with the problem of figuring out whether we are "within" a
  ;; theorem
  (when (mizar-current-theorem-labeledp)
    (re-search-backward "[^;]*theorem" nil t)
    (re-search-forward "[^;]Th\(.+\):" nil t)
    (string-to-number (match-string-no-properties 1))))

(defun mizar-next-definition ()
  "Go to the next definition."
  (when (looking-at "[^;]*definition")
    (forward-char 1))
  (if (re-search-forward "[^;]*definition" nil t)
      (backward-char 10)
    (backward-char 1)))  

(defun mizar-previous-definition ()
  "Go to the previous definition."
  (re-search-backward "[^;]*definition" nil t))  

(defun mizar-next-text-item ()
  "Go to the beginning of the next text item as defined in the
mizar syntax.  Do nothing if there are no more text items after
  the current one.")
;; Can this be done with syntax tables?  What are syntax tables,
;; anyway?

(defun mizar-previous-text-item ()
  "Go to the beginning of the previous text item as defined by the
mizar syntax.  Do nothing if there are no more text items before
the current one.")

(defun mizar-substs-safe (subst1 subst2)
  "Determine whether the substitutions SUBST1 and SUBST2 are safe
  in the sense that the source string of SUBST2 is a substring of
  the target string of SUBST1."
  (let ((source1 (car subst1))
	(target1 (cdr subst1))
	(source2 (car subst2))
	(target2 (cdr subst2)))
    (if (not (search source2 target1))
	t
      (error "Substitutions %S and %S aren't safe!" subst1 subst2))))

(defun mizar-subst-alist-safe (subst-alist)
  "Determine whether SUBST-ALIST is safe, that is, whether there
exist substitution pairs (A . B) and (C . D) in SUBST-ALIST such
that (A . B) occurs before (C . D) and B is a superstring o C."
  (or (null subst-alist)
      (let ((subst1 (car subst-alist)))
	(and (every #'(lambda (subst2) (mizar-substs-safe subst1 subst2))
		    (cdr subst-alist))
	     (mizar-subst-alist-safe (cdr subst-alist))))))

(defun mizar-replace-strings (beg end subst-alist)
  (let ((beg (or beg (point-min)))
	(end (or end (point-max))))
    (if (mizar-subst-alist-safe subst-alist)
	(dolist (subst subst-alist)
	  (let ((source (car subst))
		(target (cdr subst)))
	    (save-excursion
	      (let ((case-fold-search nil))
		(goto-char beg)
		(while (search-forward source nil t)
		  (replace-match target nil t))))))
      (error "Substitution association list unsafe! %S" subst-alist))))

(defun select (lst pred)
  "The sublist of LST consisting of those elements that satisfy PRED."
  (let (subl)
    (dolist (x lst (nreverse subl))
      (when (funcall pred x)
	(push x subl)))))

(defun mizar-references-preceding-th-label ()
  (let ((genuine-theorem-found nil)
	(found-label nil))
    (save-excursion
      (while (not genuine-theorem-found)
	(re-search-backward "^[ \t]*theorem[ \t\n]+\\(Th[0-9]+\\):" nil t)
	(unless (mizar-author-within-comment)
	  (setq genuine-theorem-found t
		found-label (match-string-no-properties 1)))))
    found-label))

(defun mizar-references-preceding-th-number ()
  (let ((preceding-label (mizar-references-preceding-th-label)))
    (if preceding-label
	(string-to-int (substring preceding-label 2))
      0)))

(defun mizar-edit-reference (old-ref new-ref starting-pos)
  (let ((old-ref-regexp (concat old-ref "\\([,;: \n]\\)")))
    (save-excursion
      (goto-char starting-pos)
      (while (re-search-forward old-ref-regexp nil t)
	(replace-match (concat new-ref (match-string-no-properties 1)) t t)))))
  
(defun mizar-edit-theorem-number (old-num new-num)
  (let ((old-label (concat "Th" (int-to-string old-num)))
	(new-label (concat "Th" (int-to-string new-num))))
    (save-excursion
      (search-forward (concat old-label ":") nil)
      (replace-match (concat new-label ":"))
      (mizar-edit-reference old-label new-label (point-min)))))

(defun mizar-label-number (label)
  "The number of the label LABEL.  LABEL can be a theorem,
definition, lemma, local label"
  (with-temp-buffer
    (insert label)
    (beginning-of-buffer)
    (re-search-forward "\\(Lm\\|Th\\|[A-Z]\\|Def\\)\\([0-9]+\\)")
    (let ((number-as-string (match-string-no-properties 2)))
      (string-to-int number-as-string))))

(defun mizar-references-bump-future-theorem-labels ()
  (let ((point (point))
	(future-labels (mizar-scan-theorem-labels-following-point)))
    (dolist (label-and-position (sort* future-labels '> :key 'cdr))
      (let ((label (car label-and-position)))
	(let ((num (mizar-label-number label)))
	  (mizar-edit-theorem-number num (1+ num)))))))
  
(defun mizar-promote-current-lemma-to-theorem ()
  "Promote the current lemma to a theorem.

The label for the new theorem will be determined by the label of
the immediately preceding theorem; if there is no preceding
theorem, the label of the new theorem will be \"Th1\".  The
labels of the succeeding theorems will be adjusted: their numbers
will be increased by one and references will be correspondingly adjusted."
  (unless (looking-at "Lm\\([0-9]+\\):")
    (error "Point is not currently at the beginning of a lemma!"))
  (let ((current-label (match-string-no-properties 1))) 
    (let ((preceding-th-label (mizar-references-preceding-th-label)))
      (let ((preceding-th-number (mizar-label-number preceding-th-label)))
	(let ((new-label (concat "Th" (int-to-string (1+ preceding-th-number)))))
	  (let ((current-labels (mizar-scan-theorem-labels-in-buffer)))
	    (let ((future-labels (select current-labels #'(lambda (x) (> (car x) (point))))))
	      (when (find (1+ preceding-th-number) future-labels :key 'cdr)
		(mizar-references-bump-future-theorem-labels))
	      (search-forward "Lm\\([0-9]+\\):")
	      (replace-string (concat "theorem" " " new-label ":"))
	      (mizar-references-replace-reference current-label new-label))))))))

(defun mizar-new-theorem-label (number)
  (concat "Th" (int-to-string number)))

(defun mizar-author-new-theorem ()
  "Insert a new theorem at point.  Assign a label to the theorem
  depending on the labels of the preceding theorem; if there is
  no precedingtheorem, the new theorem number will be 1.  In any
  case, the labels of theorems following point, as well as
  references to those future theorems, will be adjusted."
  (interactive)
  (let ((preceding-th-label (mizar-references-preceding-th-label)))
    (let ((preceding-th-number (if preceding-th-label
				   (mizar-label-number preceding-th-label)
				 0)))
      (let ((new-label (mizar-new-theorem-label (1+ preceding-th-number))))
	(mizar-references-bump-future-theorem-labels)
	(unless (looking-at " \t\n")
	  (newline 2)
	  (forward-line -2))
	(insert "theorem " new-label ":")
	(newline)))))

(defun mizar-author-new-vocabulary-item (&optional type-default name-default)
  "Insert a vocabulary item of type TYPE and name NAME in the vocabulary file associated with the current article.  TYPE can be one of the codes G, U, M, O, K, L, R, and V."
  (interactive)
  (let* ((type
	  (completing-read "Type: "
			   '("structure" "left bracket" "right bracket"
			     "mode" "functor" "predicate" "selector"
			     "attribute")
			   nil
			   t
			   nil
			   nil
			   type-default))
	 (name (read-string "Name: " nil nil name-default)))
    (let ((file-name (file-name-sans-extension
		      (file-name-nondirectory (buffer-file-name))))
	  (current-dir (file-name-directory (buffer-file-name))))
      (let ((text-dir (concat current-dir "../dict/")))
	(unless (file-exists-p text-dir)
	  (make-directory text-dir))
	(let ((voc-file-name (concat text-dir file-name ".voc")))
	  (find-file-other-window voc-file-name)
	  (mizar-voc-new-item (mizar-voc-type->code type) name))))))

;;; Navigation

(defun mizar-next-theorem ()
  "Go to the next theorem in the current article."
  (interactive)
  (when (looking-at "[^;]*theorem[^s]")
    (forward-char 1))
  (if (re-search-forward "[^;]*theorem[^s]" nil t)
      (backward-char 8)
    (backward-char 1)))

(defun mizar-previous-theorem ()
  "Go to the previous theorem in the current article."
  (interactive)
  (re-search-backward "[^;]*theorem[^s]" nil t))

(defvar mizar-proof-introducing-keywords
  '("theorem" "existence" "uniqueness" "coherence" "compatibility"
    "symmetry" "correctness"))

(defvar mizar-local-label-regex "[A-Z][0-9]+:")

(defun mizar-author-last-local-label ()
  "Determine the last label used in the current proof.

This function assumes that point is within a proof block."
  (save-excursion
    (mizar-author-move-to-nearest-uncommented-position)
    (let ((w (current-word))
	  (found-yet nil)
	  (our-label nil))
      (while (and (not found-yet)
		  (not (member w mizar-proof-introducing-keywords)))
        (if (looking-at (concat "\\(" mizar-local-label-regex "\\)"))
	    (setq our-label (match-string-no-properties 1)
		  found-yet t)
	  (backward-word)
	  (setq w (current-word))))
      (when found-yet
	(substring our-label 0 (1- (length our-label)))))))

(defun mizar-author-toplevel-proof-begin ()
  "Determine the buffer position where the current top-level
proof begins."
  (let ((toplevel))
  (save-excursion
    (mizar-author-move-to-nearest-uncommented-position)
    (while (not (member (current-word) mizar-proof-introducing-keywords))
      (backward-word)
      (while (mizar-author-within-comment)
	(mizar-author-move-to-nearest-uncommented-position)
	(backward-word)))
    (setq toplevel (point)))
  toplevel))

(defun mizar-author-find-balanced-proof (start-pos)
  "Determine the end of the subproof which begins at START-POS."
  (save-excursion
    (goto-char start-pos)
    (forward-word 2) ;; skip the proof-introducing keyword and the following "proof"
    (let ((proofs 1)
	  (ends 0))
      (while (not (eq proofs ends))
	(forward-word)
	(let ((w (current-word)))
	  (when (or (string= w "proof")
		    (string= w "suppose"))
	    (setq proofs (1+ proofs)))
	  (when (string= w "end")
	    (setq ends (1+ ends)))))
      (point))))
  
(defun mizar-author-toplevel-proof-end ()
  (mizar-author-find-balanced-proof (mizar-author-toplevel-proof-begin)))

(defun mizar-author-next-label (end-pos)
  "Find the next label before buffer position END-POS.  If there are no
labels, return nil."
  (save-excursion
    (let ((w (forward-word))
	  (found-yet nil)
	  (our-label nil))
      (while (and (not found-yet)
		  (< ( point) end-pos))
	  (backward-word)
	  (when (and (looking-at (concat "\\(" mizar-local-label-regex "\\)"))
		     (not (mizar-author-within-comment)))
	    (let ((our-match (match-string-no-properties 1)))
	      (setq our-label (substring our-match 0 (1- (length our-match)))
		    found-yet t)))
	  (forward-word 2)
	  (setq w (current-word)))
      (when found-yet
	our-label))))
      
(defun mizar-author-next-local-label ()
  "Determine the label of the next statement.  If there is no
next local label, return nil."
  (mizar-author-next-label (mizar-author-local-proof-end)))

(defvar mizar-local-proof-introducing-keywords '("proof" "suppose" "now"))

(defun mizar-backward-word-skipping-comments ()
  (backward-word)
  (while (mizar-author-within-comment)
    (mizar-author-move-to-nearest-uncommented-position)
    (backward-word)))

(defun mizar-author-local-proof-begin ()
  "Determine the buffer position where the current local proof begins."
  (let ((toplevel))
  (save-excursion
    (mizar-author-move-to-nearest-uncommented-position)
    (while (not (member (current-word t)
			mizar-local-proof-introducing-keywords))
      (mizar-backward-word-skipping-comments))
    (setq toplevel (point)))
  toplevel))

(defun mizar-author-local-proof-end ()
  (mizar-author-find-balanced-proof (mizar-author-local-proof-begin)))

(defun mizar-author-new-local-label (&optional prefix)
  "Generate a new local label.  If PREFIX is given, use it as the
prefix.  Otherwise, use `mizar-default-label-name'."
  (if prefix
      (concat prefix "1")
    (concat mizar-default-label-name "1")))

(defun mizar-local-labels-in-region (beg end)
  (let ((labels))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward (concat "\\(" mizar-local-label-regex "\\)") end t)
	(let ((match (match-string-no-properties 1)))
	  (let ((match-sans-colon (substring match 0
					     (1- (length match)))))
	    (push (cons match-sans-colon (point)) labels)))))
    (nreverse labels)))

(defun mizar-local-labels-in-subproof-following-point ()
  (mizar-local-labels-in-region (point) (mizar-author-local-proof-end)))

(defun mizar-local-labels-in-subproof-following-point-with-prefix (prefix)
  "Determine the local labels used in the current subproof and
following point whose prefix matches the given prefix PREFIX,
which is assumed to be just a capital letter."
  (let ((labels (mizar-local-labels-in-subproof-following-point)))
    (select labels #'(lambda (label-and-position)
		       (let ((label (car label-and-position)))
			 (let ((this-prefix (substring label 0 1)))
			   (string= prefix this-prefix)))))))

(defun mizar-label-prefix (label)
  "The prefix of the label LABEL."
  (substring label 0 1))

(defun mizar-make-label (prefix number)
  (concat prefix (int-to-string number)))

(defun mizar-author-bump-local-labels-following-point (prefix num)
  "Bump the local labels following point whose prefix is PREFIX and whose
number is greater than NUM."
  (let ((labels-and-positions
	 (mizar-local-labels-in-subproof-following-point-with-prefix prefix))
	(local-end (mizar-author-local-proof-end)))
    (dolist (label-and-position (reverse labels-and-positions))
      (let ((future-label (car label-and-position))
	    (future-pos (cdr label-and-position)))
	(let ((future-label-number (mizar-label-number future-label)))
	  (let ((new-label (mizar-make-label prefix (1+ future-label-number))))
	    (save-excursion
	      (let ((old-ref-regex (concat future-label "\\([,:; \n]\\)")))
		(goto-char future-pos)
		(backward-word)
		(while (re-search-forward old-ref-regex nil t)
		  (replace-match (concat new-label (match-string-no-properties 1)) t t))))))))))

(defun mizar-author-new-proof-step ()
  "Make a new proof step."
  (let ((last-label (mizar-author-last-local-label)))
    (let ((new-label 
	   (if last-label
	       (mizar-author-next-local-label)
	     (mizar-author-new-local-label))))
      (newline)
      (insert new-label ": ")
      (mizar-indent-line)
      (mizar-author-bump-local-labels-following-point
       (mizar-label-prefix last-label)
       (mizar-label-number new-label)))))

(defun mizar-current-step-end ()
  "Determine the buffer position of the end of the proof step
  that we are currently in."
  (let ((end))
  (save-excursion
    (re-search-forward ";[ \t\n]" nil t)
    (while (not (eq (char-after) ?\;))
      (backward-char))
    (setq end (point)))
  end))

(defvar mizar-step-introducing-keywords
  '("let" "consider" "reconsider" "hence" "thus"))

(defun mizar-matches-local-label (purported-label)
  "Check whether PURPORTED-LABEL really is a local label."
  (with-temp-buffer
    (insert purported-label)
    (beginning-of-buffer)
    (re-search-forward "^[A-Z][0-9]+" nil t)))

(defun mizar-matches-reference (purported-reference)
  "Check whether PURPORTED-REFERENCE really is a reference (to a
  theorem, lemma, or definition)."
  (with-temp-buffer
    (insert purported-reference)
    (beginning-of-buffer)
    (or (looking-at "Th[0-9]+")
	(looking-at "Lm[0-9]+")
	(looking-at "Def[0-9]+"))))

(defun mizar-current-step-begin ()
  "Determine the buffer position of the beginning of the proof
step that we are currently in." 
  (let ((pos))
  (save-excursion
    (let ((found-yet nil))
      (while (not found-yet)
	(let ((w (current-word)))
	  (if (mizar-matches-local-label w)
	      (let ((prospective-point (point)))
		(save-excursion
		  (backward-word)
		  (let ((prev-word (current-word)))
		    (unless (or (string= prev-word "by")
				(string= prev-word "of")
				(string= prev-word "that")
				(mizar-matches-reference prev-word))
		      (setq found-yet t
			    pos prospective-point))))
		(unless found-yet
		  (mizar-backward-word-skipping-comments)))
	    (if (member w mizar-step-introducing-keywords)
		(setq found-yet t
		      pos (point))
	      (mizar-backward-word-skipping-comments)))))))
  pos))

(defun mizar-forward-step ()
  "Go one step forward in the current proof, if possible."
  (let ((the-end (mizar-author-local-proof-end))
	(purported-next (point)))
    (save-excursion
      (goto-char (mizar-current-step-end))
      (forward-word)
      (unless (string= (current-word) "end")
	(setq purported-next (point))))
    (unless (eq (point) purported-next)
      (goto-char purported-next)
      (backward-word))))

(defun mizar-backward-step ()
  "Go one step backward in the current proof, if possible."
  (let ((the-beginning (mizar-author-local-proof-begin))
	(purported-prev (point)))
    (save-excursion
      (goto-char (mizar-current-step-begin))
      (mizar-backward-word-skipping-comments)
      (unless (member (current-word) mizar-proof-introducing-keywords)
	(setq purported-prev (mizar-current-step-begin))))
    (unless (eq (point) purported-prev)
      (goto-char purported-prev))))

(defun mizar-num-theorems ()
  "Count the number of theorems in the current article."
  (length (mizar-scan-theorem-labels-in-buffer)))

(defun mizar-greatest-theorem-number-used ()
  (let ((current-labels-and-positions (mizar-scan-theorem-labels-in-buffer)))
    (let ((only-labels (mapcar 'car current-labels-and-positions)))
      (let ((labels-as-strings (mapcar #'(lambda (label-str)
					  (substring label-str 2))
				       only-labels)))
	(let ((labels-as-numbers (mapcar 'string-to-int labels-as-strings)))
	  (apply 'max labels-as-numbers))))))

(defun mizar-uniquify-theorem-labels ()
  "Send all theorems to completely new numbers.  They will be in
ascending order, and their number will be greater than the number
of all the theorems in the buffer.  This function is used in
`mizar-fix-theorem-labels' to ensure hygiene."
  (let ((num-theorems (mizar-num-theorems))
	(greatest (mizar-greatest-theorem-number-used))
	(offset 1))
    (save-excursion
      (end-of-buffer)
      (while (re-search-backward "^[ \t]*theorem[ \t\n]+\\(Th[0-9]+\\):" nil t)
	(unless (mizar-author-within-comment)
	  (let ((old-label (match-string-no-properties 1))
		(new-label (concat "Th"
				   (int-to-string 
				    (+ greatest offset num-theorems)))))
	    (mizar-edit-reference old-label new-label (point)))
	  (decf num-theorems))))))

(defun mizar-fix-theorem-labels ()
  "Ensure that labels of the theorem make sense, that is, if
there are n theorems in the current article, then make sure that
the last one is labeled by \"Th<n>\", the penultimate theorem is
labeled \"Th<n-1>\", etc."
  (let ((num-theorems (mizar-num-theorems)))
    (save-excursion
      (mizar-uniquify-theorem-labels)
      (end-of-buffer)
      (while (> num-theorems 0)
	(re-search-backward "^[ \t]*theorem[ \t\n]+\\(Th[0-9]+\\):" nil t)
	(unless (mizar-author-within-comment)
	  (let ((old-label (match-string-no-properties 1))
		(new-label (concat "Th" (int-to-string num-theorems))))
	    (mizar-edit-reference old-label new-label (point)))
	  (decf num-theorems))))))
      

(provide 'mizar-references)

;;; mizar-references.el ends here