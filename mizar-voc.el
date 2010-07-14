;;; mizar-voc.el --- Work with mizar vocabulary files

;;; Commentary:
;; This file provides some simple utilties for working with mizar
;; vocabulary files.

(require 'mizar)

;;; Code:
(defmacro mizar-voc-new-item (code name)
  "Define a new mizar vocabulary item in the current vocabulary
file; its code is CODE and the name of the item to be defined is
NAME."
  `(progn
     (unless (looking-at "^[ \t\n]")
       (end-of-line)
       (newline))
     (insert ,code ,name)))

(defun mizar-voc-new-structure-symbol (name)
  "Insert a new structure structure symbol in the current vocabulary file with name NAME."
  (interactive "sName: ")
  (mizar-voc-new-item "G" name))

(defun mizar-voc-new-selector-symbol (name)
  "Insert a new selector symbol in the current vocabulary file with name NAME."
  (interactive "sName: ")
  (mizar-voc-new-item "U" name))

(defun mizar-voc-new-mode-symbol (name)
  "Insert a new mode symbol in the current vocabulary file with
name NAME."
  (interactive "sName: ")
  (mizar-voc-new-item "M" name))

(defun mizar-voc-new-functor-symbol (name)
  "Insert a new functor symbol in the current vocabulary file
with name NAME."
  (interactive "sName: ")
  (mizar-voc-new-item "O" name))

(defun mizar-voc-new-left-functor-bracket-symbol (name)
  "Insert a new left functor bracket symbol in the current
vocabulary file with name NAME."
  (interactive "sName: ")
  (mizar-voc-new-item "K" name))

(defun mizar-voc-new-right-functor-bracket-symbol (name)
  "Insert a new right functor bracket symbol in the current
vocabulary file with name NAME."
  (interactive "sName: ")
  (mizar-voc-new-item "L" name))

(defun mizar-voc-new-predicate-symbol (name)
  "Insert a new predicate symbol in the current vocabulary file
with name NAME."
  (interactive "sName: ")
  (mizar-voc-new-item "R" name))

(defun mizar-voc-new-attribute-symbol (name)
  "Insert a new attribute symbol in the current vocabulary file
with name NAME."
  (interactive "sName: ")
  (mizar-voc-new-item "V" name))

(defun mizar-voc-on-item-linep ()
  "Determine whether we are on a line that has a vocabulary item
on it."
  t) ;; skip for now

(defun mizar-voc-delete-item ()
  "Delete the vocabulary item at point from the current vocabulary file."
  (interactive)
  (unless (mizar-voc-on-item-linep)
    (error "Not currently on a line that contains a vocabulary item"))
  (delete-region (line-beginning-position) (line-end-position)))

(defun mizar-voc-new-item-command (type name)
  "Insert a new vocabulary item in the current vocabulary file
with name NAME and type TYPE.  The variable TYPE should be among
the possible codes G, U, M, O, K, L, R, and V."
  (interactive)
  (unless (member type '("G" "U" "M" "O" "K" "L" "R" "V"))
    (error "Unknown vocabulary item type %s" type))
  (mizar-voc-new-item type name))

(defun mizar-voc-type->code (type)
  "Transform a vocabulary type (an English word) into its
symbolic code equivalent."
  (cond ((string= type "structure") "G")
	((string= type "left bracket") "K")
	((string= type "right bracket") "L")
	((string= type "mode") "M")
	((string= type "functor") "O")
	((string= type "predicate") "R")
	((string= type "selector") "U")
	((string= type "attribute") "V")
	(t (error "Unknown type %s" type))))

(defun mizar-voc-new-item-command-with-prompt (&optional type-default name-default)
  "Insert a new vocabulary item in the current vocabulary file."
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
    (mizar-voc-new-item (mizar-voc-type->code type) name)))

(defun mizar-voc-current-item-code ()
  "The code of the current vocablary item."
  (unless (mizar-voc-on-item-linep)
    (error "Not currently on a line that contains a vocabulary item"))
  (char-to-string (char-after (line-beginning-position))))

(defun mizar-voc-current-item-name ()
  "The name of the current vocabulary item."
  (unless (mizar-voc-on-item-linep)
    (error "Not currently on a line that contains a vocabulary item"))
  (buffer-substring-no-properties (1+ (line-beginning-position))
				  (line-end-position)))

(defun mizar-voc-code->type (code)
  "Transform a mizar vocabulary code CODE into the corresponding
English word."
  (cond ((string= code "G") "structure")
	((string= code "K") "left bracket")
	((string= code "L") "right bracket")
	((string= code "M") "mode")
	((string= code "O") "functor")
	((string= code "R") "predicate")
	((string= code "U") "selector")
	((string= code "V") "attribute")
	(t (error "Unknown code %s" code))))

(defun mizar-voc-edit-item ()
  "Edit the current vocabulary item."
  (interactive)
  (unless (mizar-voc-on-item-linep)
    (error "Not currently on a line that contains a vocabulary item"))
  (let ((code (mizar-voc-current-item-code))
	(name (mizar-voc-current-item-name)))
    (mizar-voc-delete-item)
    (mizar-voc-new-item-command-with-promt (mizar-voc-code->type code) name)))

(provide 'mizar-voc)

;;; mizar-voc.el ends here
