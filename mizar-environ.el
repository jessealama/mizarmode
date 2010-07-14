;;; mizar-environ.el --- Utilities for working with environments in mizar articles

;;; Commentary:
;; 

(require 'mizar)
(require 'mizar-references)
(require 'cl) ;; for `labels'

;;; Code:

(defconst vocabularies-keyword "vocabularies")
(defconst notations-keyword "notations")
(defconst constructors-keyword "constructors")
(defconst registrations-keyword "registrations")
(defconst requirements-keyword "requirements")
(defconst theorems-keyword "theorems")
(defconst definitions-keyword "definitions")
(defconst schemes-keyword "schemes")

;;; Beginning and end of the environment

(defun mizar-environ-begin ()
  "Determine the buffer position where the environment begins."
  (let ((pos))
    (save-excursion
      (let ((found nil))
	(goto-char (point-min))
	(while (not found)
	  (re-search-forward "^[ \t]*environ" nil t)
	  (unless (mizar-author-within-comment)
	    (backward-word)
	    (setq pos (point)
		  found t)))))
    pos))

(defun mizar-environ-end ()
  "Determine the buffer position where the environment ends."
  (let ((pos))
    (save-excursion
      (goto-char (mizar-environ-begin))
      (let ((found nil))
	(while (not found)
	  (re-search-forward "[ \t\n]begin" nil t)
	  (unless (mizar-author-within-comment)
	    (backward-word)
	    (setq pos (point)
		  found t)))))
    (1- pos)))

;;; Beginnings and ends of directives

(defun mizar-environ-directive-begin (directive-name)
  "Determine the buffer position where the directive whose name
is DIRECTIVE-NAME begins.  It is the position of the first
occurrence of the first character of DIRECTIVE-NAME that is not
commented out and within the bounds of the environment.  Return
nil if there is no such directive in the current environment.

This function assumes that the directive whose name is
DIRECTIVE-NAME is on its own line, that is, that on the line in
which it occurs (not commented out), every character preceding
the first character if the first occurrence is a whitespace
characters (space and tab, to be more precise)."
  (let ((pos nil))
    (save-excursion
      (goto-char (mizar-environ-begin))
      (when (re-search-forward (concat "^[ \t]*" directive-name) (mizar-environ-end) t)
	(backward-word)
	(setq pos (point))))
  pos))

(defun mizar-environ-directive-end (directive-name)
  "Determine the buffer position of the end of the directive
whose name is DIRECTIVE-NAME.  It is the buffer position
immediately following the semicolon that terminates the list of
MML identifiers following the keyword DIRECTIVE-NAME.  Return nil
if there is no such directive in the current environment."
  (let ((pos nil))
    (save-excursion
      (let ((begin (mizar-environ-directive-begin directive-name)))
	(when begin
	  (goto-char begin)
	  (let ((found nil))
	    (while (not found)
	      (re-search-forward ";")
	      (unless (mizar-author-within-comment)
		(setq found t
		      pos (point))))))))
    pos))

(defun mizar-environ-vocabularies-begin ()
  "Determine the buffer position where the vocabularies directive
begins.  See `mizar-environ-directive-begin'."
  (mizar-environ-directive-begin vocabularies-keyword))

(defun mizar-environ-vocabularies-end ()
  "Determine the buffer position of the end of the vocabularies
directive.  See `mizar-environ-directive-end'."
  (mizar-environ-directive-end vocabularies-keyword))
    
(defun mizar-environ-notations-begin ()
  "Determine the buffer position where the notations directive
begins. See `mizar-environ-directive-begin'."
  (mizar-environ-directive-begin notations-keyword))

(defun mizar-environ-notations-end ()
  "Determine the buffer position where the notations directive
ends.  See `mizar-environ-directive-end'."
  (mizar-environ-directive-begin notations-keyword))

(defun mizar-environ-constructors-begin ()
  "Determine the buffer position where the constructors directive
begins. See `mizar-environ-directive-begin'."
  (mizar-environ-directive-begin constructors-keyword))

(defun mizar-environ-constructors-end ()
  "Determine the buffer position where the constructors directive
ends.  See `mizar-environ-directive-end'."
  (mizar-environ-directive-begin constructors-keyword))

(defun mizar-environ-registrations-begin ()
  "Determine the buffer position where the registrations
directive begins. See `mizar-environ-directive-begin'."
  (mizar-environ-directive-begin registrations-keyword))

(defun mizar-environ-registrations-end ()
  "Determine the buffer position where the registrations
directive ends.  See `mizar-environ-directive-end'."
  (mizar-environ-directive-begin registrations-keyword))

(defun mizar-environ-requirements-begin ()
  "Determine the buffer position where the requirements
directive begins. See `mizar-environ-directive-begin'."
  (mizar-environ-directive-begin requirements-keyword))

(defun mizar-environ-requirements-end ()
  "Determine the buffer position where the requirements
directive ends.  See `mizar-environ-directive-end'."
  (mizar-environ-directive-begin requirements-keyword))

(defun mizar-environ-definitions-begin ()
  "Determine the buffer position where the definitions
directive begins. See `mizar-environ-directive-begin'."
  (mizar-environ-directive-begin definitions-keyword))

(defun mizar-environ-definitions-end ()
  "Determine the buffer position where the definitions
directive ends.  See `mizar-environ-directive-end'."
  (mizar-environ-directive-begin definitions-keyword))

(defun mizar-environ-theorems-begin ()
  "Determine the buffer position where the theorems
directive begins. See `mizar-environ-directive-begin'."
  (mizar-environ-directive-begin theorems-keyword))

(defun mizar-environ-theorems-end ()
  "Determine the buffer position where the theorems
directive ends.  See `mizar-environ-directive-end'."
  (mizar-environ-directive-begin theorems-keyword))

(defun mizar-environ-schemes-begin ()
  "Determine the buffer position where the schemes
directive begins. See `mizar-environ-directive-begin'."
  (mizar-environ-directive-begin schemes-keyword))

(defun mizar-environ-schemes-end ()
  "Determine the buffer position where the schemes
directive ends.  See `mizar-environ-directive-end'."
  (mizar-environ-directive-begin schemes-keyword))

;;; The contents of directives

;; Regular expressions matching the various directives

(defun mizar-environ-directive-item-list-regexp ()
  "A regular expression matching the list of items within a
directive."
  (rx
   (group
    (regexp "\\(?:[A-Z0-9]+_\\)*[A-Z0-9]+")
    (0+ (and (0+ (regexp "[ \t\n]"))
	     ","
	     (0+ (regexp "[ \t\n]"))
	     (regexp "\\(?:[A-Z0-9]+_\\)*[A-Z0-9]+"))))))

(defun mizar-environ-regexp (directive)
  "A regular expression matching the directive DIRECTIVE and the
list of MML identifiers that follow it."
  (concat
   (rx bol
       (0+ (regexp "[ \t]")))
   directive
   (rx (1+ (regexp "[ \t\n]")))
   (mizar-environ-directive-item-list-regexp)))

(defun mizar-environ-vocabularies-regexp ()
  "A regular expression matching the vocabularies directive and
the list of MML identifiers that follow it."
  (mizar-environ-regexp vocabularies-keyword))

(defun mizar-environ-notations-regexp ()
  "A regular expression matching the notations directive and
the list of MML identifiers that follow it."
  (mizar-environ-regexp notations-keyword))

(defun mizar-environ-constructors-regexp ()
  "A regular expression matching the constructors directive and
the list of MML identifiers that follow it."
  (mizar-environ-regexp constructors-keyword))

(defun mizar-environ-registrations-regexp ()
  "A regular expression matching the registrations directive and
the list of MML identifiers that follow it."
  (mizar-environ-regexp registrations-keyword))

(defun mizar-environ-theorems-regexp ()
  "A regular expression matching the theorems directive and
the list of MML identifiers that follow it."
  (mizar-environ-regexp theorems-keyword))

(defun mizar-environ-definitions-regexp ()
  "A regular expression matching the definitions directive and
the list of MML identifiers that follow it."
  (mizar-environ-regexp definitions-keyword))

(defun mizar-environ-requirements-regexp ()
  "A regular expression matching the requirements directive and
the list of MML identifiers that follow it."
  (mizar-environ-regexp requirements-keyword))

(defun mizar-environ-schemes-regexp ()
  "A regular expression matching the schemes directive and
the list of MML identifiers that follow it."
  (mizar-environ-regexp schemes-keyword))

(defun mizar-environ-trim (str)
  "Take out any initial and trailing whitespace in string STR."
  ;; stolen from gnus-simplify-all-whitespace
  (while (string-match "[ \t\n]+" str)
    (setq str (replace-match "" nil nil str)))
  str)

(defmacro mizar-environ-parse-by-regexp (directive-regexp)
  "Determine the list of MML identifiers following the directive
matching the regular expression DIRECTIVE-REGEXP.

This functions produced by this macro assume that nothing in the
environment

schemes;				;
 is commented out."
  (let ((mml-ids (gensym)))
    `(let ((,mml-ids nil))
     (save-excursion
       (goto-char (mizar-environ-begin))
       (when (re-search-forward ,directive-regexp (mizar-environ-end) t)
	 (setq ,mml-ids (match-string-no-properties 1))))
     (mapcar 'mizar-environ-trim (split-string ,mml-ids ",")))))

(defun mizar-environ-vocabularies ()
  "Determine the vocabularies being used in the current article."
  (when (mizar-environ-vocabularies-begin)
    (mizar-environ-parse-by-regexp (mizar-environ-vocabularies-regexp))))

(defun mizar-environ-notations ()
  "Determine the notations being used in the current article."
  (when (mizar-environ-notations-begin)
    (mizar-environ-parse-by-regexp (mizar-environ-notations-regexp))))

(defun mizar-environ-constructors ()
  "Determine the constructors being used in the current article."
  (when (mizar-environ-constructors-begin)
    (mizar-environ-parse-by-regexp (mizar-environ-constructors-regexp))))

(defun mizar-environ-requirements ()
  "Determine the requirements being used in the current article."
  (when (mizar-environ-requirements-begin)
    (mizar-environ-parse-by-regexp (mizar-environ-requirements-regexp))))

(defun mizar-environ-definitions ()
  "Determine the definitions being used in the current article."
  (when (mizar-environ-definitions-begin)
    (mizar-environ-parse-by-regexp (mizar-environ-definitions-regexp))))

(defun mizar-environ-theorems ()
  "Determine the theorems being used in the current article."
  (when (mizar-environ-theorems-begin)
    (mizar-environ-parse-by-regexp (mizar-environ-theorems-regexp))))

(defun mizar-environ-schemes ()
  "Determine the schemes being used in the current article."
  (when (mizar-environ-schemes-begin)
    (mizar-environ-parse-by-regexp (mizar-environ-schemes-regexp))))

(defun mizar-environ-registrations ()
  "Determine the registrations being used in the current article."
  (when (mizar-environ-registrations-begin)
    (mizar-environ-parse-by-regexp (mizar-environ-registrations-regexp))))

;;; Inserting content into a directive

(defun mizar-environ-indent-directive (directive)
  "Indent the region corresponding to the extend of the directive
DIRECTIVE."
  (let ((begin (mizar-environ-directive-begin directive))
	(end (mizar-environ-directive-end directive)))
    (indent-region begin end)))

(defun current-line-is-only-whitespace? ()
  "Determine whether the current line consists only of
  whitespace."
  (let ((every-white? t))
    (save-excursion
      (beginning-of-line)
      (while (and every-white? (not (eolp)))
	(if (looking-at "\\s-")
	    (forward-char)
	  (setq every-white? nil))))
    every-white?))

(defun mizar-environ-clear-directive (directive)
  "Remove the directive DIRECTIVE from the environment.  Return
the buffer position where the directive used to begin, or NIL if
the directive didn't exist in the first place."
  (let ((beg (mizar-environ-directive-begin directive))
	(end (mizar-environ-directive-end directive)))
    (when beg ;; there actually is such a directive
      (delete-region beg end)
      ;; Determine whether after deletion the current line is just
      ;; whitespace.  If it is, kill it.  Otherwise, leave it alone.
      (when (current-line-is-only-whitespace?)
	(beginning-of-line)
	(kill-line)
	(when (current-line-is-only-whitespace?)
	  (beginning-of-line)
	  (kill-line))))
    beg))

(defun mizar-environ-clear-vocabularies ()
  (mizar-environ-clear-directive vocabularies-keyword))

(defun mizar-environ-clear-notations ()
  (mizar-environ-clear-directive notations-keyword))

(defun mizar-environ-clear-constructors ()
  (mizar-environ-clear-directive constructors-keyword))

(defun mizar-environ-clear-registrations ()
  (mizar-environ-clear-directive registrations-keyword))

(defun mizar-environ-clear-requirements ()
  (mizar-environ-clear-directive requirements-keyword))

(defun mizar-environ-clear-definitions ()
  (mizar-environ-clear-directive definitions-keyword))

(defun mizar-environ-clear-theorems ()
  (mizar-environ-clear-directive theorems-keyword))

(defun mizar-environ-clear-schemes ()
  (mizar-environ-clear-directive schemes-keyword))

(defun mizar-environ-clear-environment ()
  (let ((beg (mizar-environ-begin))
	(end (mizar-environ-end)))
    (save-excursion
      (goto-char beg)
      (forward-word 1) ;; skip over "environ"
      (kill-region (point) end)
      (newline))))

(defun mizar-environ-insert-identifiers (directive new-identifiers &optional start-pos)
  "Insert the list NEW-IDENTIFIERS of MML
identifiers (represented as strings) into the content of the
directive DIRECTIVE.  If START-POS is non-nil, then the new
directive will be inserted starting at START-POS.  If there is no
directive corresponding to DIRECTIVE, it will be created.  (In
the case where NEW-IDENTIFIERS is empty and DIRECTIVE doesn't
exist, the directive DIRECTIVE will not be created.)"
  (when new-identifiers
    (let ((end (mizar-environ-directive-end directive)))
      (save-excursion 
	(if end
	    (progn 
	      (goto-char end)
	      (backward-delete-char 1))
	  (progn
	    (if start-pos
		(goto-char start-pos)
	      (progn
		(goto-char (mizar-environ-begin))
		(forward-word) ;; skip "environ"
		(newline 2)))
	    (insert (if start-pos "" "  ") directive " ")))
	(dolist (identifier new-identifiers)
	  (insert identifier ", "))
	(backward-delete-char 2) ;; get rid of trailing ", "
	(insert ";")
	(unless (eolp)
	  (newline 2))))))


(defun mizar-environ-insert-vocabularies (new-identifiers &optional start-pos)
  (mizar-environ-insert-identifiers vocabularies-keyword
				    new-identifiers
				    start-pos))

(defun mizar-environ-insert-notations (new-identifiers &optional start-pos)
  (mizar-environ-insert-identifiers notations-keyword 
				    new-identifiers 
				    start-pos))

(defun mizar-environ-insert-constructors (new-identifiers &optional start-pos)
  (mizar-environ-insert-identifiers constructors-keyword
				    new-identifiers
				    start-pos))

(defun mizar-environ-insert-registrations (new-identifiers &optional start-pos)
  (mizar-environ-insert-identifiers registrations-keyword
				    new-identifiers
				    start-pos))

(defun mizar-environ-insert-requirements (new-identifiers &optional start-pos)
  (mizar-environ-insert-identifiers requirements-keyword
				    new-identifiers
				    start-pos))

(defun mizar-environ-insert-definitions (new-identifiers &optional start-pos)
  (mizar-environ-insert-identifiers definitions-keyword
				    new-identifiers
				    start-pos))

(defun mizar-environ-insert-theorems (new-identifiers &optional start-pos)
  (mizar-environ-insert-identifiers theorems-keyword
				    new-identifiers
				    start-pos))

(defun mizar-environ-insert-schemes (new-identifiers &optional start-pos)
  (mizar-environ-insert-identifiers schemes-keyword
				    new-identifiers
				    start-pos))

;;; Checking whether the order of the MML identifiers in the notations
;;; directive follows the order given in mml.lar

(defvar mizar-mml-lar-contents nil
  "A list representing the current contents of mml.lar, as
distributed with mizar.")

(defun mizar-mml-lar ()
  "Determine the list of articles in mml.lar."
  (or mizar-mml-lar-contents
      (let ((ids nil))
	(with-current-buffer (find-file-noselect (concat mizfiles "mml.lar"))
	  (goto-char (point-min))
	  (while (re-search-forward "^\\(.+\\)$" nil t)
	    (let ((mml-id (match-string-no-properties 1)))
	      (push (upcase mml-id) ids))))
	(setq mizar-mml-lar-contents (nreverse ids)))))

(defun mizar-environ-mml-lar-< (item-1 item-2)
  "Determine whether the identifiers ITEM-1 precedes ITEM-2 in
the MML.LAR order.  If ITEM-1 is not an official MML identifier
but ITEM-2 is, return NIL.  If ITEM-1 is an official MML
identifier and ITEM-2 is not, return T.  If neither ITEM-1 nor
ITEM-2 is an official MML identifer, return T (i.e., assume that
the user has correctly ordered the non-MML items).  Note that the special item TARSKI should be before every item, even though it is not one of the items in MML.LAR."
  (if (string= item-1 "TARSKI")
      (not (string= item-2 "TARSKI"))
    (if (string= item-2 "TARSKI")
	nil
      (let ((pos-1 (position item-1 (mizar-mml-lar) :test #'string=))
	    (pos-2 (position item-2 (mizar-mml-lar) :test #'string=)))
	(if (null pos-1)
	    (not (null pos-2))
	  (or (null pos-2) (< pos-1 pos-2)))))))

(defun mizar-environ-sort-items-by-mml-lar-order (items)
  (sort items #'mizar-environ-mml-lar-<))

;;; Importing and merging environments from other articles

(defun mizar-environ-insert-fresh-environment (vocabularies 
					       constructors
					       notations
					       registrations
					       requirements
					       theorems
					       definitions
					       schemes)
  (mizar-environ-clear-environment)
  (mizar-environ-insert-vocabularies vocabularies)
  (mizar-environ-insert-constructors constructors)
  (mizar-environ-insert-notations notations)
  (mizar-environ-insert-registrations registrations)
  (mizar-environ-insert-requirements requirements)
  (mizar-environ-insert-theorems theorems)
  (mizar-environ-insert-definitions definitions)
  (mizar-environ-insert-schemes schemes))

(defun mizar-environ-import-article-environment (mml-identifier)
  "Add the environment of the MML article MML-IDENTIFIER to the
current environment."
  (interactive "sArticle: ")
  (let ((full-name (concat mizfiles "abstr" "/" 
			   (downcase mml-identifier) ".abs")))
    (if (file-exists-p full-name)
	(let ((current-vocabularies (mizar-environ-vocabularies))
	      (current-constructors (mizar-environ-constructors))
	      (current-notations (mizar-environ-notations))
	      (current-registrations (mizar-environ-registrations))
	      (current-requirements (mizar-environ-requirements))
	      (current-theorems (mizar-environ-theorems))
	      (current-definitions (mizar-environ-definitions))
	      (current-schemes (mizar-environ-schemes)))
	  (let (new-vocabularies
		new-constructors 
		new-notations
		new-registrations
		new-requirements
		new-theorems
		new-definitions
		new-schemes)
	    (with-current-buffer (find-file-noselect full-name)
	      (setq new-vocabularies  (mizar-environ-vocabularies)
		    new-constructors  (mizar-environ-constructors)
		    new-notations     (mizar-environ-notations)
		    new-registrations (mizar-environ-registrations)
		    new-requirements  (mizar-environ-requirements)
		    new-theorems      (mizar-environ-theorems)
		    new-definitions   (mizar-environ-definitions)
		    new-schemes       (mizar-environ-schemes)))
	    (setq new-vocabularies
		  (delete-duplicates
		   (append current-vocabularies new-vocabularies)
		   :test #'string=))
	    (setq new-constructors
		  (delete-duplicates
		   (append current-constructors new-constructors)
		   :test #'string=))
	    (setq new-notations
		  (delete-duplicates
		   (append current-notations new-notations)
		   :test #'string=))
	    (setq new-registrations
		  (delete-duplicates
		   (append current-registrations new-registrations)
		   :test #'string=))
	    (setq new-requirements
		  (delete-duplicates
		   (append current-requirements new-requirements)
		   :test #'string=))
	    (setq new-theorems
		  (delete-duplicates
		   (append current-theorems new-theorems)
		   :test #'string=))
	    (setq new-definitions
		  (delete-duplicates
		   (append current-definitions new-definitions)
		   :test #'string=))
	    (setq new-schemes
		  (delete-duplicates
		   (append current-schemes new-schemes)
		   :test #'string=))

	    (setq new-notations
		  (mizar-environ-sort-items-by-mml-lar-order new-notations))

	    (mizar-environ-insert-fresh-environment new-vocabularies
						    new-constructors
						    new-notations
						    new-registrations
						    new-requirements
						    new-theorems
						    new-definitions
						    new-schemes)))
	     
      (error "No such article %S in the MML!" mml-identifier))))

(defun mizar-environ-add-articles-to-environment (&rest articles)
  "Add the MML identifiers ARTICLES to every directive in the
current article. The notations directive will possibly be
reordered to ensure that it follows MML.LAR order.  Articles that
are not in the MML are assumed to exist, and are they are assumed
to be given in the correct order vis a vis the notations
directive."
  (let ((new-vocabularies (append (mizar-environ-vocabularies)
				  (copy-sequence articles)))
	(new-constructors (append (mizar-environ-constructors) 
				  (copy-sequence articles)))
	(new-notations (append (mizar-environ-notations) 
			       (copy-sequence articles)))
	(new-registrations (append (mizar-environ-registrations) 
				   (copy-sequence articles)))
	(new-requirements (append (mizar-environ-requirements) 
				  (copy-sequence articles)))
	(new-theorems (append (mizar-environ-theorems) 
			      (copy-sequence articles)))
	(new-definitions (append (mizar-environ-definitions) 
				 (copy-sequence articles)))
	(new-schemes (append (mizar-environ-schemes) 
			     (copy-sequence articles))))

     (setq new-notations 
	   (sort new-notations 'mizar-environ-mml-lar-<))

     (mizar-environ-insert-fresh-environment 
      (delete-duplicates new-vocabularies :test #'string=)
      (delete-duplicates new-constructors :test #'string=)
      (delete-duplicates new-notations :test #'string=)
      (delete-duplicates new-registrations :test #'string=)
      (delete-duplicates new-requirements :test #'string=)
      (delete-duplicates new-theorems :test #'string=)
      (delete-duplicates new-definitions :test #'string=)
      (delete-duplicates new-schemes :test #'string=))))

(provide 'mizar-environ)

;;; mizar-environ.el ends here
