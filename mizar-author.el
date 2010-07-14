(require 'mizar-references)
(require 'mizar-voc)

(defun mizar-author-new-theorem ()
  "Insert a new theorem at point.  Assign a label to the theorem
  depending on the labels of the preceding theorems, and adjust
  the labels of succeeding theorems, as well as references to
  those future theorems."
  (interactive)
  (let ((preceding-th-number (mizar-references-preceding-th-number)))
    (let ((new-label (concat "Th" (int-to-string (1+ preceding-th-number)))))
      (let ((current-labels (mizar-scan-theorem-labels)))
	(let ((future-labels (select current-labels
				     #'(lambda (x) (> (car x) (point))))))
	  (when (find (1+ preceding-th-number) future-labels :key 'cdr)
	    (mizar-references-bump-future-theorem-labels))))
      (unless (looking-at " \t\n")
	(newline 2)
	(forward-line -2))
      (insert "theorem " new-label ":")
      (newline))))

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

(defvar mizar-proof-introducing-keywords
  '("theorem" "existence" "uniqueness"))

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
	  (when (string= w "proof")
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
	    (setq our-label (match-string-no-properties 1)
		  found-yet t))
	  (forward-word 2)
	  (setq w (current-word)))
	(when found-yet
	  our-label))))
      
(defun mizar-author-next-local-label ()
  "Determine the label of the next statement.  If there is no
next local label, return nil."
  (mizar-author-next-label (mizar-author-toplevel-proof-end)))

(defvar mizar-local-proof-introducing-keywords '("proof" "suppose" "now"))

(defun mizar-author-local-proof-begin ()
  "Determine the buffer position where the current local proof begins."
  (let ((toplevel))
  (save-excursion
    (mizar-author-move-to-nearest-uncommented-position)
    (while (not (member (current-word t)
			mizar-local-proof-introducing-keywords))
      (backward-word)
      (while (mizar-author-within-comment)
	(mizar-author-move-to-nearest-uncommented-position)
	(backward-word)))
    (setq toplevel (point)))
  toplevel))

(defun mizar-author-local-proof-end ()
  (mizar-author-find-balanced-proof (mizar-author-local-proof-begin)))

(defun mizar-author-new-local-label (label)
  "Determine the label that follows the given label LABEL."
  (let (label-prefix label-number)
    (with-temp-buffer
      (insert label)
      (beginning-of-buffer)
      (re-search-forward "\\([A-Z]\\)\\([0-9]+\\)")
      (setq label-prefix (match-string-no-properties 1))
      (setq label-number (string-to-int (match-string-no-properties 2))))
    (concat label-prefix (int-to-string (1+ label-number)))))

(defun mizar-author-new-proof-step ()
  "Make a new proof step."
  (let ((last-label (mizar-author-last-local-label)))
    (let ((new-label 
	   (if last-label
	       (mizar-author-next-local-label last-label)))
	  (mizar-author-new-local-label))
      (newline)
      (insert new-label ": ")
      (mizar-indent-line))))
      ;; (mizar-references-bump-future-local-labels))))

(provide 'mizar-author)