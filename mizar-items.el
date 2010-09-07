(require 'mizar-author)

(defun mizar-items ()
  "All items in the current article."
  (save-excursion
    (goto-char 0)
    (mizar-next-theorem)
    
  

;;; mizar-items.el ends here